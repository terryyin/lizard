// Read-only checks behind Dough Land's worktree retirement: which worktree Git
// lists at a path, whether its creation record names this work, whether a
// revision is contained, and whether a separately published remote branch may
// go. Also the preserved-result shapes every refusal reports.
import { existsSync, realpathSync } from "node:fs";
import {
  git,
  lsRemoteSha,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import {
  createdForRoot,
  isAncestor,
} from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
const notCreatedForWork =
  "reused, host-owned, or unrecorded workspace, not created for this work";

function canonical(path) {
  return existsSync(path) ? realpathSync(path) : path;
}

// True when the Git command succeeds, false when it answers no (exit 1).
async function succeeds(repo, ...args) {
  try {
    await git(repo, ...args);
    return true;
  } catch (error) {
    if (error.code === 1) {
      return false;
    }
    throw error;
  }
}

export function refExists(repo, ref) {
  return succeeds(repo, "show-ref", "--verify", "--quiet", ref);
}

export async function findWorktree(repository, worktree) {
  const { stdout } = await git(repository, "worktree", "list", "--porcelain");
  const wanted = canonical(worktree);
  const blocks = stdout.split("\n\n").filter((block) => block.trim() !== "");
  for (const block of blocks) {
    const lines = block.split("\n");
    const path = lines
      .find((line) => line.startsWith("worktree "))
      ?.slice("worktree ".length);
    if (!path || canonical(path) !== wanted) {
      continue;
    }
    const branchLine = lines.find((line) => line.startsWith("branch "));
    return {
      path,
      branch: branchLine ? branchLine.slice("branch refs/heads/".length) : null,
    };
  }
  return null;
}

export function preserved(reason, worktree, branch, extra = {}) {
  return {
    removed: false,
    partial: false,
    worktree: "preserved",
    branch: "preserved",
    reason,
    path: worktree,
    branchName: branch,
    ...extra,
  };
}

// A removal step ran but was not verified; `results` names what was done.
export function unverifiedRemoval(reason, worktree, branch, results = {}) {
  return preserved(reason, worktree, branch, { partial: true, ...results });
}

// One work-scoped ownership gate over the worktree's creation refs: a ref
// naming `identity` retires whichever session created it; one naming other
// work retains; without a ref, only the caller's created-for-this-work fact
// retires.
export async function ownershipHold(worktree, identity, createdForWork) {
  const refs = await git(
    worktree,
    "for-each-ref",
    "--format=%(refname:lstrip=4)",
    createdForRoot,
  );
  const works = refs.stdout.split("\n").filter((name) => name !== "");
  if (identity && works.includes(identity)) {
    return null;
  }
  if (works.length > 0) {
    return {
      reason: `created for other work: ${works.join(", ")}`,
      createdFor: works,
    };
  }
  return createdForWork === true ? null : { reason: notCreatedForWork };
}

// After the fetch: the remote branch's current tip, or "" when it is already
// absent, and the reason to keep everything when the fetched target lacks that
// tip or any revision the caller names as contained.
export async function containmentHold({
  management,
  remote,
  remoteBranch,
  tracking,
  contained,
}) {
  const remoteTip = remoteBranch
    ? await lsRemoteSha(remote, `refs/heads/${remoteBranch}`, management)
    : "";
  if (remoteTip && !(await isAncestor(management, remoteTip, tracking))) {
    return { remoteTip, reason: "remote execution tip is not integrated" };
  }
  for (const sha of contained) {
    if (!(await isAncestor(management, sha, tracking))) {
      return { remoteTip, reason: "unique unpublished work" };
    }
  }
  return { remoteTip, reason: null };
}
