// Settles the final closure for Trunk Mode `finish`. A final closure the
// target already holds is recognized through resumeInterruptedPublication
// without a second push, on the matching observer that covers it, live or
// already ended, so completion can be reused or repeated. An unpublished one
// is published once through managed delivery, rebased when the target moved.
// A rerun with the original `--final` after `finish` rebased and published it
// recognizes the rebased closure the target holds and resumes it the same way.
// Once the execution worktree is gone, only an accepted closure is settled,
// from the recorded management context and the observer's checkout path.
import { existsSync, realpathSync } from "node:fs";
import { basename, dirname, join } from "node:path";
import {
  isLiveMatchingMailbox,
  listMatchingMailboxes,
} from "../../dough-execute-plan/scripts/ci-mailbox-match.mjs";
import { listRegisteredRevisions } from "../../dough-execute-plan/scripts/ci-mailbox-revision-coverage.mjs";
import { deliverManagedExecutionIncrement } from "../../dough-execute-plan/scripts/execution-increment-delivery.mjs";
import { observerAdapter } from "../../dough-execute-plan/scripts/execution-increment-resume.mjs";
import {
  git,
  targetBranchName,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import { resumeInterruptedPublication } from "../../dough-execute-plan/scripts/publication-resume.mjs";
import { isAncestor } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";

const publicationRecoveries = {
  conflict:
    "resolve the rebase stopped in the worktree under Resolve a publication rebase conflict, then rerun finish with the rebased tip as --final and the fetched target tip it extends as --previously-published-base",
  "candidate-mismatch":
    "the branch tip is not --final; commit the final closure or name the branch tip as --final, then rerun finish",
};

// The canonical checkout path an observer was started for, also after that
// worktree was removed: its parent still resolves as it did then.
export function observerRoot(workspace) {
  return existsSync(workspace)
    ? realpathSync(workspace)
    : join(realpathSync(dirname(workspace)), basename(workspace));
}

// The one matching mailbox that covers `sha`, preferring a live one; else the
// one live matching mailbox, which has yet to register it. Otherwise none.
function closureMailbox({ repo, branch, root, storage, sha }) {
  const matches = listMatchingMailboxes({ repo, branch, root, storage });
  const live = matches.filter((directory) =>
    isLiveMatchingMailbox(directory, { repo, branch, root, storage }),
  );
  const covering = matches.filter((directory) =>
    listRegisteredRevisions(directory).includes(sha.toLowerCase()),
  );
  const chosen = [
    covering.filter((directory) => live.includes(directory)),
    covering,
    covering.length ? [] : live,
  ].find((candidates) => candidates.length > 0);
  if (chosen?.length === 1) return { directory: chosen[0] };
  return {
    reason: chosen
      ? "ambiguous matching observers for the final closure"
      : "no matching observer covers the final closure",
  };
}

// Recognizes the accepted final closure without pushing, registers it on a
// live matching observer that lacks it, and reports that observer.
async function resumeAcceptedClosure({
  inspection,
  accepted,
  superseded = [],
  targetRef,
  remote,
  repo,
  root,
  storage,
}) {
  const found = closureMailbox({
    repo,
    branch: targetBranchName(targetRef),
    root,
    storage,
    sha: accepted,
  });
  const resumed = await resumeInterruptedPublication({
    ownedWorkspace: inspection,
    candidateSha: accepted,
    supersededShas: superseded,
    publishedRevisions: [accepted],
    observer: found.directory
      ? observerAdapter(found.directory, targetRef)
      : null,
    targetRef,
    remote,
  });
  return {
    acceptedSha: accepted,
    pushCount: resumed.pushCount,
    observation: found.directory
      ? { state: "recovered", directory: found.directory, reused: true }
      : { state: "unobserved", pendingCi: "unobserved", reason: found.reason },
    startReceipt: null,
  };
}

// Author, author date, and message survive the rebase delivery performs, so
// they identify the final closure after it was rebased. An amend keeps them
// too: without the worktree, an unpublished amendment of a published closure
// is taken as that closure.
async function closureIdentity(inspection, sha) {
  return (
    await git(inspection, "log", "-1", "--format=%an%x00%ae%x00%at%x00%B", sha)
  ).stdout;
}

// The commit `rev` names in this repository, or null when it holds none.
async function commitOf(inspection, rev) {
  try {
    return (
      await git(inspection, "rev-parse", "-q", "--verify", `${rev}^{commit}`)
    ).stdout.trim();
  } catch (error) {
    if (error.code !== 1) throw error;
    return null;
  }
}

// The rebased final closure the fetched target holds: the branch tip while the
// branch exists, else a revision a matching observer registered, whose
// identity equals `final`'s. Null when none is recognized.
async function rebasedFinalClosure({
  inspection,
  tracking,
  branch,
  final,
  targetRef,
  repo,
  root,
  storage,
}) {
  const original = await commitOf(inspection, final);
  if (!original) return null;
  const tip = await commitOf(inspection, `refs/heads/${branch}`);
  const candidates = tip
    ? [tip]
    : listMatchingMailboxes({
        repo,
        branch: targetBranchName(targetRef),
        root,
        storage,
      }).flatMap(listRegisteredRevisions);
  const wanted = await closureIdentity(inspection, original);
  for (const candidate of new Set(candidates)) {
    // An observer may have registered a revision this repository lacks.
    if (!(await commitOf(inspection, candidate))) continue;
    if (!(await isAncestor(inspection, candidate, tracking))) continue;
    if ((await closureIdentity(inspection, candidate)) === wanted)
      return candidate;
  }
  return null;
}

// Closure commits carry records, not behavior proof, so a non-conflicting
// rebase onto a moved target invalidates no proof: the rebased closure is
// published once.
async function publishFinalClosure(request) {
  const delivered = await deliverManagedExecutionIncrement({
    ...request,
    validatedCandidate: request.final,
    validate: async () => ({ ok: true }),
  });
  const observation = delivered.observation;
  const startReceipt = delivered.startReceipt ?? null;
  if (!delivered.ok) {
    return {
      stopped: delivered.status === "conflict" ? "conflict" : "publish",
      publication: delivered.publication,
      status: delivered.status,
      candidate: delivered.candidate ?? null,
      remoteTip: delivered.remoteTip ?? null,
      error: delivered.error,
      recovery:
        publicationRecoveries[delivered.status] ??
        `publication stopped with ${delivered.status}; recover it under Resume an interrupted publication, then rerun finish`,
      observation,
      startReceipt,
    };
  }
  return {
    acceptedSha: delivered.receipt.sha,
    pushCount: 1,
    observation,
    startReceipt,
  };
}

// Returns the accepted final closure with its observer, or `stopped` naming
// the unfinished step with nothing pushed or retired.
export async function settleFinalClosure(request) {
  const { workspace, inspection, tracking, final } = request;
  if (await isAncestor(inspection, final, tracking)) {
    return resumeAcceptedClosure({ ...request, accepted: final });
  }
  const rebased = await rebasedFinalClosure(request);
  if (rebased) {
    return resumeAcceptedClosure({
      ...request,
      accepted: rebased,
      superseded: [final],
    });
  }
  if (inspection !== workspace) {
    return {
      stopped: "context",
      publication: "not-attempted",
      reason:
        "execution worktree is absent before the final closure is accepted",
      recovery:
        "if an earlier finish result reported acceptedSha, rerun finish with it as --final; otherwise report the unpublished final closure as the gap, since nothing can publish it without its worktree",
    };
  }
  return publishFinalClosure(request);
}
