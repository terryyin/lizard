import { existsSync } from "node:fs";
import {
  creditMergeInProgress,
  DeveloperIdentityRefused,
} from "../../dough-execute-plan/scripts/workspace-agent-authorship.mjs";
import {
  acceptStaged,
  stagedResolutionRefusal,
  validateCandidate,
} from "./product-backlog-git-candidate.mjs";
import {
  doneDirectoryChanged,
  stageRebuiltDoneCatalog,
  stagedMisplacedAgentProfiles,
} from "./product-backlog-git-done-catalog.mjs";
import { describeMisplacedAgentProfiles } from "./product-backlog-misplaced-agent-profile.mjs";
import {
  backlogIsUnmerged,
  git,
  gitLine,
  gitOutcome,
  gitPath,
  registerBacklogDrivers,
} from "./product-backlog-git-repository.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

// Validate a fast-forward candidate before Git advances the branch.
function fastForward(repoRoot, file, ref) {
  let candidate;
  try {
    candidate = git(["show", `${ref}:${file}`], repoRoot);
  } catch {
    throw new BacklogError(
      `${file} is not present at ${ref}, so there is no fast-forward ` +
        `candidate to validate.`,
    );
  }
  const outcome = validateCandidate(candidate, "the fast-forward candidate");
  if (!outcome.valid) {
    return {
      status: "refused-before-commit",
      message:
        `The fast-forward candidate for ${file} at ${ref} is not a backlog ` +
        `this tool can read, so the current branch was not advanced.\n${outcome.reason}`,
    };
  }
  git(["merge", "--ff-only", ref], repoRoot);
  return { status: "fast-forwarded" };
}

// Rebuild the catalog within the merge commit and credit the developer.
async function commitAcceptedMerge(repoRoot, file, subject) {
  const mergeHead = gitLine(["rev-parse", "MERGE_HEAD"], repoRoot);
  const mergeBase = gitLine(["merge-base", "HEAD", mergeHead], repoRoot);
  if (doneDirectoryChanged(repoRoot, file, mergeBase, ["HEAD", mergeHead])) {
    stageRebuiltDoneCatalog(repoRoot, file);
  }
  try {
    await creditMergeInProgress(repoRoot);
  } catch (error) {
    if (!(error instanceof DeveloperIdentityRefused)) throw error;
    return {
      status: "refused-before-commit",
      message:
        `${file}'s ${subject} is accepted, but the merge stays uncommitted ` +
        `because its developer credit is refused. Fix the Git committer ` +
        `identity, then run \`continue\` for this same merge.\n${error.message}`,
    };
  }
  const committed = gitOutcome(["commit", "--no-edit"], repoRoot);
  if (committed.code !== 0) {
    return {
      status: "blocked",
      message:
        `${file}'s ${subject} is accepted, but the merge could not be ` +
        `committed. Something unrelated to this file is still unresolved; ` +
        `this gate leaves it exactly as it was.\n${committed.stderr}`,
    };
  }
  return { status: "accepted" };
}

// Keep a divergent merge uncommitted until its staged backlog is accepted.
export async function mergeBacklog({ repoRoot, file, ref }) {
  const headSha = gitLine(["rev-parse", "HEAD"], repoRoot);
  const mergeBase = gitLine(["merge-base", headSha, ref], repoRoot);
  if (mergeBase === headSha) {
    return fastForward(repoRoot, file, ref);
  }
  if (mergeBase === gitLine(["rev-parse", `${ref}^{commit}`], repoRoot)) {
    return {
      status: "blocked",
      message:
        `The current branch already contains ${ref}; nothing was merged or ` +
        `changed, and there is no merge to \`continue\`.`,
    };
  }

  registerBacklogDrivers(repoRoot, file);
  // Directory-rename inference is off for this merge only: when one side
  // replaces the last profiles in `agents/` with done records, Git would
  // otherwise read `agents/` as moved into `done/` and carry the other side's
  // live profile along. Ordinary file renames are still detected.
  gitOutcome(
    [
      "-c",
      "merge.directoryRenames=false",
      "merge",
      "--no-commit",
      "--no-ff",
      ref,
    ],
    repoRoot,
  );

  if (backlogIsUnmerged(repoRoot, file)) {
    return {
      status: "conflict",
      message:
        `${file} is left unresolved after merging ${ref} into ${repoRoot}; ` +
        `the merge stays uncommitted and Git stays mid-merge. Resolve it by ` +
        `hand, \`git add ${file}\`, then run \`continue\` for this same merge.`,
    };
  }

  const outcome = acceptStaged(repoRoot, file);
  if (!outcome.valid) {
    return {
      status: "refused-before-commit",
      message:
        `${ref} merged into the index without a Git conflict in ${file}, ` +
        `but the result fails this tool's own invariants, so the merge ` +
        `stays uncommitted rather than being accepted as is.\n${outcome.reason}`,
    };
  }

  return commitAcceptedMerge(repoRoot, file, "own result");
}

export async function continueMerge({ repoRoot, file }) {
  if (!existsSync(gitPath(repoRoot, "MERGE_HEAD"))) {
    throw new BacklogError(`No merge is in progress in ${repoRoot}.`);
  }
  const refusal = stagedResolutionRefusal(repoRoot, file);
  if (refusal) return refusal;
  const misplaced = stagedMisplacedAgentProfiles(repoRoot, file);
  if (misplaced.length > 0) {
    return {
      status: "refused",
      message:
        `The staged result holds an agent profile among the done records ` +
        `beside ${file}, so the merge stays uncommitted. Move each profile ` +
        `back to the path named below, then run \`continue\` for this same ` +
        `merge.\n${describeMisplacedAgentProfiles(misplaced)}`,
    };
  }
  return commitAcceptedMerge(repoRoot, file, "staged candidate");
}
