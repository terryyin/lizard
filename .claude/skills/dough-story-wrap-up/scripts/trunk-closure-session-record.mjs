// Trunk Mode `finish`'s record stage once the accepted final closure's
// completion receipt permits retirement, before retirement: the session
// evidence is sealed with the accepted closure pair, that completion and its
// close time, kept in the per-worktree evidence file, and published as a
// separate records-only commit through the session close resume. The record
// commit is never one of its own ranges and no observer covers it. A rerun
// resumes from the sealed evidence without repeating the closure push or the
// completion. Without the worktree, a record the target already holds for the
// closure settles the stage; otherwise the stage stops. Story Branch
// wrap-up's record stage shares `publishClosureRecord`.
import { existsSync } from "node:fs";
import {
  readSessionEvidence,
  sealSessionEvidence,
} from "../../dough-execute-plan/scripts/execution-session-evidence.mjs";
import { git } from "../../dough-execute-plan/scripts/publication-git.mjs";
import { resumeSessionClose } from "../../dough-execute-plan/scripts/session-record-publication.mjs";
import { sessionRecordDirectoryBeside } from "../../dough-execute-plan/scripts/session-record.mjs";
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";

const holdsClosure = (ranges, sha) =>
  ranges?.find((range) => range.candidate === sha);

// The completion a sealed close already settled for `acceptedSha`, so a
// rerun does not repeat it.
export function sealedCompletion(workspace, acceptedSha) {
  if (!existsSync(workspace)) return undefined;
  const sealed = readSessionEvidence(workspace);
  if (!sealed?.closedAt || !holdsClosure(sealed.ranges, acceptedSha)) return;
  return sealed.closing;
}

// The closure's pair on its parent; sealing keeps a pair delivery recorded.
async function closurePair(workspace, acceptedSha) {
  const base = (
    await git(workspace, "rev-parse", `${acceptedSha}^`)
  ).stdout.trim();
  return { base, candidate: acceptedSha };
}

// A record commit after `acceptedSha` on `tracking` whose ranges hold
// `closedSha`.
async function recordOnTarget(
  inspection,
  tracking,
  acceptedSha,
  backlogPath,
  closedSha,
) {
  const directory = sessionRecordDirectoryBeside(backlogPath);
  const range = `${acceptedSha}..${tracking}`;
  const log = await git(
    inspection,
    "log",
    "--diff-filter=A",
    "--name-only",
    "--format=@%H",
    range,
    "--",
    directory,
  );
  let sha;
  for (const line of log.stdout.split("\n").filter(Boolean)) {
    if (line.startsWith("@")) sha = line.slice(1);
    else {
      const shown = await git(inspection, "show", `${sha}:${line}`);
      if (holdsClosure(JSON.parse(shown.stdout).ranges, closedSha))
        return { status: "published", path: line, sha };
    }
  }
  return null;
}

// Seals `closing`, the settled completion, with the record and publishes it.
// `closedSha` is the session's last own commit, `acceptedSha` unless Story
// Branch integration accepted it inside a merge, which adds no range.
// Returns `{ record }` once published, else `{ stopped, record, recovery }`.
export async function publishClosureRecord(
  {
    workspace,
    inspection,
    tracking,
    branch,
    acceptedSha,
    closedSha = acceptedSha,
    targetRef,
    remote,
    backlogPath = defaultBacklogPath,
    recovery = "rerun finish",
  },
  closing,
) {
  if (!existsSync(workspace)) {
    const found = await recordOnTarget(
      inspection,
      tracking,
      acceptedSha,
      backlogPath,
      closedSha,
    );
    if (found) return { record: found };
    return {
      stopped: "record",
      record: { status: "not-published" },
      recovery:
        "the execution worktree held the sealed session record and is gone; report the closure's session record as unpublished",
    };
  }
  const sealed = sealSessionEvidence(
    workspace,
    await closurePair(workspace, closedSha),
    closing,
  );
  const { record } = await resumeSessionClose({
    record: sealed,
    workspace,
    branch,
    targetRef,
    remote,
    backlogPath,
    // A record commit carries no behavior proof, so a rebase onto another
    // writer's commit invalidates none, as for the closure.
    validate: async () => ({ ok: true }),
  });
  if (record.status === "published") {
    const sha = (
      await git(workspace, "rev-parse", `refs/heads/${branch}`)
    ).stdout.trim();
    return { record: { status: "published", path: record.path, sha } };
  }
  return {
    stopped: "record",
    record: {
      status: record.status,
      path: record.path,
      error: record.error ?? record.publication?.status ?? null,
    },
    recovery: `follow record.error, then ${recovery}; it publishes the sealed session record kept in the worktree without another closure push or completion`,
  };
}
