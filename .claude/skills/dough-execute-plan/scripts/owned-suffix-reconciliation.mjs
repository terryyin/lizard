// Replay only the owned unpublished suffix onto a newer authorized tip.
// Suffixes touching the backlog or the done records beside it go through the
// product-backlog rebase adapter; other suffixes use a raw rebase, without
// rename detection when they change only session records, whose near-identical
// files would otherwise pair one writer's prune with another's new record.
// Callers own proof, push, and observation.
import { fileURLToPath } from "node:url";
import { doneDirectoryBeside } from "../../dough-product-backlog/scripts/product-backlog-git-done-catalog.mjs";
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import { exec, git } from "./publication-git.mjs";
import { sessionRecordDirectoryBeside } from "./session-record.mjs";

const rebaseCli = fileURLToPath(
  new URL(
    "../../dough-product-backlog/scripts/product-backlog-git-rebase.mjs",
    import.meta.url,
  ),
);

async function suffixTouchesBacklogRecords(
  workspace,
  base,
  branch,
  backlogPath,
) {
  const names = (
    await git(
      workspace,
      "diff",
      "--name-only",
      `${base}..${branch}`,
      "--",
      backlogPath,
      doneDirectoryBeside(backlogPath),
    )
  ).stdout.trim();
  return names !== "";
}

async function suffixChangesOnlySessionRecords(
  workspace,
  base,
  branch,
  backlogPath,
) {
  const directory = `${sessionRecordDirectoryBeside(backlogPath)}/`;
  const names = (
    await git(workspace, "diff", "--name-only", `${base}..${branch}`)
  ).stdout
    .split("\n")
    .filter(Boolean);
  return names.length > 0 && names.every((name) => name.startsWith(directory));
}

async function rebaseThroughAdapter({
  workspace,
  onto,
  upstream,
  branch,
  backlogPath,
}) {
  try {
    const { stdout, stderr } = await exec(process.execPath, [
      rebaseCli,
      "rebase",
      "--onto",
      onto,
      "--ref",
      upstream,
      "--branch",
      branch,
      "--cwd",
      workspace,
      "--file",
      backlogPath,
    ]);
    return { ok: true, stdout, stderr };
  } catch (error) {
    return {
      ok: false,
      code: error.code ?? 1,
      stdout: error.stdout ?? "",
      stderr: error.stderr ?? "",
    };
  }
}

async function rebaseRaw(workspace, onto, upstream, branch, renames = true) {
  const config = renames ? [] : ["-c", "merge.renames=false"];
  try {
    await git(workspace, ...config, "rebase", "--onto", onto, upstream, branch);
    return { ok: true };
  } catch (error) {
    return {
      ok: false,
      code: error.code ?? 1,
      stdout: error.stdout ?? "",
      stderr: error.stderr ?? "",
      message: error.message,
    };
  }
}

export async function reconcileOwnedSuffix({
  workspace,
  onto,
  upstream,
  branch,
  backlogPath = defaultBacklogPath,
}) {
  const touches = await suffixTouchesBacklogRecords(
    workspace,
    upstream,
    branch,
    backlogPath,
  );
  if (touches) {
    return rebaseThroughAdapter({
      workspace,
      onto,
      upstream,
      branch,
      backlogPath,
    });
  }
  const recordsOnly = await suffixChangesOnlySessionRecords(
    workspace,
    upstream,
    branch,
    backlogPath,
  );
  return rebaseRaw(workspace, onto, upstream, branch, !recordsOnly);
}
