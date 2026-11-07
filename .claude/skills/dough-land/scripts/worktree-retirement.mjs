#!/usr/bin/env node
// Dough Land "Retire the worktree": from the repository management context,
// retire a worktree created for this work once the fetched target contains its
// branch tip, then delete a separately published remote branch trunk contains.
// Keeps a dirty, ambiguous, other-branch, not-owned, or uncontained worktree and
// branches; never forces, resets, or deletes the target branch.
import { existsSync } from "node:fs";
import { resolve } from "node:path";
import { isDirectCliEntry } from "../../dough-execute-plan/scripts/ci-direct-entry.mjs";
import {
  git,
  lsRemoteSha,
  originTrackingRef,
  resolveManagementContext,
  targetBranchName,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import {
  containmentHold,
  findWorktree,
  ownershipHold,
  preserved,
  refExists,
  unverifiedRemoval,
} from "./retirement-checks.mjs";
import { isAncestor } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";

const trunkTarget = "refs/heads/main";

// Remove the worktree and safely delete its branch, then any separately
// published `remoteBranch`, each verified and accepted as already absent on a
// rerun. The fetched target must contain the branch tip, the remote branch tip,
// and every `contained` revision. Once that is known, `holdReason` may name a
// caller's own obligation that keeps everything.
export async function retireWorktree({
  repository,
  worktree,
  branch,
  remote = "origin",
  targetRef = trunkTarget,
  identity,
  createdForWork = false,
  remoteBranch = "",
  contained = [],
  holdReason,
}) {
  const remoteResult = remoteBranch ? { remoteBranch: "preserved" } : {};
  const keep = (reason, extra = {}) =>
    preserved(reason, worktree, branch, { ...remoteResult, ...extra });
  if (remoteBranch && remoteBranch === targetBranchName(targetRef)) {
    return keep("remote branch is the target");
  }
  const management = await resolveManagementContext(repository, worktree);
  if (!management) {
    return keep("management context unavailable");
  }
  const listed = await findWorktree(management, worktree);
  if ((!listed && existsSync(worktree)) || listed?.branch === null) {
    return keep("ambiguous checkout");
  }
  if (listed && listed.branch !== branch) {
    return keep("another workspace");
  }
  if (listed) {
    const owner = await ownershipHold(worktree, identity, createdForWork);
    if (owner) {
      const { reason, ...extra } = owner;
      return keep(reason, extra);
    }
    const status = (await git(worktree, "status", "--porcelain")).stdout;
    if (status !== "") {
      return keep("dirty checkout");
    }
  }
  await git(management, "fetch", remote);
  const tracking = originTrackingRef(targetRef, remote);
  const branchRef = `refs/heads/${branch}`;
  const branchPresent = await refExists(management, branchRef);
  const tipContained =
    !branchPresent || (await isAncestor(management, branch, tracking));
  const hold = await containmentHold({
    management,
    remote,
    remoteBranch,
    tracking,
    contained,
  });
  const held =
    hold.reason ||
    (!tipContained && "unique unpublished work") ||
    (await holdReason?.({ management, tracking }));
  if (held) {
    return keep(held);
  }
  const worktreeResult = listed ? "removed" : "already-absent";
  if (listed) {
    await git(management, "worktree", "remove", worktree);
    if (await findWorktree(management, worktree)) {
      return unverifiedRemoval(
        "worktree removal was not verified",
        worktree,
        branch,
        remoteResult,
      );
    }
  }
  if (branchPresent) {
    // `git branch -d` treats a branch as merged when its tip is in its
    // upstream, so point the upstream at the fetched target. Never force.
    await git(management, "branch", `--set-upstream-to=${tracking}`, branch);
    await git(management, "branch", "-d", branch);
    if (await refExists(management, branchRef)) {
      return unverifiedRemoval(
        "local branch removal was not verified",
        worktree,
        branch,
        { worktree: worktreeResult, ...remoteResult },
      );
    }
  }
  const results = {
    worktree: worktreeResult,
    branch: branchPresent ? "removed" : "already-absent",
  };
  if (remoteBranch) {
    // Never the target branch, and only once the target contains its tip.
    if (hold.remoteTip) {
      await git(management, "push", remote, "--delete", remoteBranch);
      const remoteRef = `refs/heads/${remoteBranch}`;
      if (await lsRemoteSha(remote, remoteRef, management)) {
        return unverifiedRemoval(
          "remote branch removal was not verified",
          worktree,
          branch,
          { ...results, ...remoteResult },
        );
      }
    }
    results.remoteBranch = hold.remoteTip ? "removed" : "already-absent";
  }
  return {
    removed: true,
    partial: false,
    ...results,
    reason: null,
    repository: management,
  };
}

const required = ["repository", "worktree", "branch", "remote", "targetRef"];
const usage =
  "usage: worktree-retirement.mjs retire --repository PATH --worktree PATH --branch NAME --remote NAME --target-ref REF [--identity WORK] [--created-for-work] [--remote-branch NAME] [--contained SHA]...";

function argumentsOf(argv) {
  if (argv[0] !== "retire") {
    throw new Error(usage);
  }
  const result = {};
  for (let index = 1; index < argv.length; index += 1) {
    const flag = argv[index];
    if (flag === "--created-for-work") {
      result.createdForWork = true;
      continue;
    }
    if (!flag.startsWith("--") || index + 1 >= argv.length) {
      throw new Error(`invalid argument ${flag}\n${usage}`);
    }
    const key = flag
      .slice(2)
      .replace(/-[a-z]/g, (match) => match[1].toUpperCase());
    if (key === "contained") {
      result.contained = [...(result.contained ?? []), argv[++index]];
    } else {
      result[key] = argv[++index];
    }
  }
  for (const field of required) {
    if (!result[field]) {
      throw new Error(`missing ${field}\n${usage}`);
    }
  }
  return result;
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  let args;
  try {
    args = argumentsOf(process.argv.slice(2));
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
  if (args) {
    const worktree = resolve(args.worktree);
    let result;
    try {
      result = await retireWorktree({
        ...args,
        repository: resolve(args.repository),
        worktree,
      });
    } catch (error) {
      // A failed Git step stops retirement; report what is known.
      result = { removed: false, reason: "git error", error: error.message };
    }
    process.stdout.write(
      `${JSON.stringify({ ok: result.removed, ...result })}\n`,
    );
    if (!result.removed) process.exitCode = 1;
  }
}
