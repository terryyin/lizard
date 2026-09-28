#!/usr/bin/env node
// Adapts one authorized merge of the product backlog to real Git state. This
// is the Git-aware counterpart `product-backlog-merge.mjs` deliberately is
// not: it obtains the ancestor and each side's real bytes from Git itself —
// a validated fast-forward candidate when no reconciliation is needed at all,
// or the shared resolver run by a self-registered custom merge driver for
// every other case, clean or conflicted — and gates the actual result before
// it advances anything. A non-fast-forward merge stays uncommitted until its
// result is accepted; a stopped merge is resumed only by an explicit human
// decision, validated against this tool's own invariants and never re-run
// through disputed reconciliation.
//
// This never selects a side, aborts, resets, or repairs a conflict itself:
// on any refusal the affected Git state — refs, index, worktree, and any
// unrelated conflicted path — is left exactly as Git already had it, for a
// human to resolve and then explicitly continue.
import { existsSync } from "node:fs";
import {
  creditMergeInProgress,
  DeveloperIdentityRefused,
} from "../../dough-execute-plan/scripts/workspace-agent-authorship.mjs";
import {
  acceptStaged,
  validateCandidate,
} from "./product-backlog-git-candidate.mjs";
import { runGitOperationCli } from "./product-backlog-git-cli.mjs";
import {
  ensureDriverRegistered,
  git,
  gitLine,
  gitOutcome,
  gitPath,
  repositoryRoot,
} from "./product-backlog-git-repository.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

export { ensureDriverRegistered, repositoryRoot, validateCandidate };

// A fast-forward candidate never goes through reconciliation at all — only
// one side changed anything since the ancestor — but an unreadable or
// invalid candidate is still a reason to leave the managed target where it
// was rather than advance it onto a backlog this tool could not then read
// back.
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

// Commits a merge whose own path is now accepted, unless Git itself still
// refuses because something unrelated is still unresolved — in which case
// this gate leaves the merge exactly as it was, for a human to resolve. In an
// agent's owned workspace the merge commit credits the developer like any
// other agent commit, and an unusable developer leaves it uncommitted.
async function commitAcceptedMerge(repoRoot, file, subject) {
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

// One authorized merge: fast-forward when Git's own history already makes
// this a pure advance, or a real, possibly multi-file `git merge` gated by
// the shared resolver (through the driver registered above) for everything
// else. The merge is never committed by this call alone — only once this
// path's own result is validated, and only if Git itself has nothing else
// left unresolved.
export async function mergeOperation({ repoRoot, file, ref }) {
  const headSha = gitLine(["rev-parse", "HEAD"], repoRoot);
  const mergeBase = gitLine(["merge-base", headSha, ref], repoRoot);
  if (mergeBase === headSha) {
    return fastForward(repoRoot, file, ref);
  }

  ensureDriverRegistered(repoRoot, file);
  gitOutcome(["merge", "--no-commit", "--no-ff", ref], repoRoot);

  const unresolved = gitLine(["ls-files", "-u", "--", file], repoRoot);
  if (unresolved !== "") {
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

// Resumes a merge this tool already stopped, once a human has supplied a
// resolution: validates what is actually staged for this path — never
// re-running the reconciliation the human's decision may deliberately
// differ from — and only then commits. An unresolved path, a staged
// candidate that fails this tool's invariants, or one that does not match
// the working tree all stay stopped exactly where they were.
export async function continueOperation({ repoRoot, file }) {
  if (!existsSync(gitPath(repoRoot, "MERGE_HEAD"))) {
    throw new BacklogError(`No merge is in progress in ${repoRoot}.`);
  }
  const unresolved = gitLine(["ls-files", "-u", "--", file], repoRoot);
  if (unresolved !== "") {
    throw new BacklogError(
      `${file} is still unresolved in ${repoRoot}. Edit it by hand, then ` +
        `\`git add ${file}\`, before continuing.`,
    );
  }
  const outcome = acceptStaged(repoRoot, file);
  if (!outcome.valid) {
    return { status: "refused", message: outcome.reason };
  }
  return commitAcceptedMerge(repoRoot, file, "staged candidate");
}

const usage =
  `Usage:\n` +
  `  product-backlog-git-merge.mjs merge --ref <ref> [--file <path>] [--cwd <dir>]\n` +
  `  product-backlog-git-merge.mjs continue [--file <path>] [--cwd <dir>]\n`;

await runGitOperationCli({
  argv: process.argv.slice(2),
  primaryName: "merge",
  usage,
  primaryOperation: mergeOperation,
  continueOperation,
  failingStatuses: ["conflict", "refused-before-commit", "refused", "blocked"],
});
