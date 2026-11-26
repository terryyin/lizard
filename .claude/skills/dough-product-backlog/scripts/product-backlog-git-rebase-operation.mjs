import { readFileSync } from "node:fs";
import { join } from "node:path";
import {
  stagedResolutionRefusal,
  validateCandidate,
} from "./product-backlog-git-candidate.mjs";
import { commitRebuiltDoneCatalog } from "./product-backlog-git-done-catalog.mjs";
import { rebaseState } from "./product-backlog-git-operation-state.mjs";
import { acceptCleanRebase } from "./product-backlog-git-rebase-aggregate.mjs";
import {
  backlogIsUnmerged,
  gitLine,
  gitOutcome,
  registerBacklogDrivers,
} from "./product-backlog-git-repository.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

const noEditor = { GIT_EDITOR: "true", EDITOR: "true" };

function describeReplay(repoRoot) {
  const state = rebaseState(repoRoot);
  if (!state) {
    return (
      `Git's own rebase state could not be read; identify the replayed ` +
      `commit, its parent, and the current destination directly with ` +
      `\`git status\` and \`git log\` before resolving.`
    );
  }
  const subject = gitLine(
    ["log", "-1", "--format=%s", state.replayedCommit],
    repoRoot,
  );
  return (
    `Replaying ${state.replayedCommit} ("${subject}"), whose own parent is ` +
    `${state.replayedParent}, onto ${state.destination}. Do not trust Git's ` +
    `"ours"/"theirs" labels for intent here: during a rebase "ours" names ` +
    `the destination (${state.destination}) and "theirs" names the ` +
    `replayed commit (${state.replayedCommit}), the reverse of an ordinary merge.`
  );
}

function interpretStop(repoRoot, file, outcome) {
  if (backlogIsUnmerged(repoRoot, file)) {
    return {
      status: "conflict",
      message:
        `${describeReplay(repoRoot)}\n${file} is left unresolved by this ` +
        `replay; the rebase stays paused and Git stays mid-rebase. Resolve ` +
        `it by hand, \`git add ${file}\`, then run \`continue\` for this ` +
        `same rebase.`,
    };
  }
  return {
    status: "blocked",
    message:
      `The rebase stopped, but ${file} is not the unresolved path; ` +
      `something unrelated needs human resolution first. This gate leaves ` +
      `the rebase exactly as it is.\n${outcome.stderr}`,
  };
}

export function rebaseBacklog({ repoRoot, file, ref, onto, branch }) {
  registerBacklogDrivers(repoRoot, file);
  const preRebaseTip = gitLine(["rev-parse", branch ?? "HEAD"], repoRoot);
  const destinationAtStart = gitLine(["rev-parse", onto ?? ref], repoRoot);
  const args = ["rebase", ...(onto ? ["--onto", onto] : []), ref];
  if (branch) args.push(branch);

  const outcome = gitOutcome(args, repoRoot, noEditor);
  if (outcome.code !== 0) return interpretStop(repoRoot, file, outcome);

  const result = acceptCleanRebase(
    repoRoot,
    file,
    onto ?? ref,
    preRebaseTip,
    destinationAtStart,
  );
  if (result.status !== "rebased") return result;
  return commitRebuiltDoneCatalog(repoRoot, file, destinationAtStart, result);
}

export function continueRebase({ repoRoot, file }) {
  const state = rebaseState(repoRoot);
  if (!state) {
    throw new BacklogError(`No rebase is in progress in ${repoRoot}.`);
  }
  const refusal = stagedResolutionRefusal(repoRoot, file);
  if (refusal) return refusal;

  // Reuse the replayed message and the human's accepted decision.
  const continued = gitOutcome(["rebase", "--continue"], repoRoot, noEditor);
  if (continued.code === 0) {
    return commitRebuiltDoneCatalog(repoRoot, file, state.onto, {
      status: "rebased",
      message:
        `The rebase completed; every replayed commit is present and ` +
        `unpublished.`,
    });
  }
  return interpretStop(repoRoot, file, continued);
}

// Read the result of a disputed clean replay without applying another decision.
export function validateRebase({ repoRoot, file }) {
  const contents = readFileSync(join(repoRoot, file), "utf8");
  const outcome = validateCandidate(contents, "the branch's current result");
  if (!outcome.valid) {
    return { status: "refused", message: outcome.reason };
  }
  return {
    status: "validated",
    message:
      `${file} on the current branch is a backlog this tool can read; ` +
      `nothing was changed.`,
  };
}
