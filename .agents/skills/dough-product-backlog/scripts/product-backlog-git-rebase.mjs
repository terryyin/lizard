#!/usr/bin/env node
// Adapts one authorized rebase of the product backlog to real Git state, the
// rebase-shaped counterpart to `product-backlog-git-merge.mjs`. Every commit
// Git replays that touches the backlog path goes through the same
// self-registering merge driver slice 1 built — Git invokes it identically
// for a rebase's per-commit replay as it does for an ordinary merge, clean or
// conflicted alike, confirmed directly against this project's installed Git
// rather than assumed — feeding the existing `mergeBacklogs` core unmodified.
//
// A conflicted replay is identified by real revisions read from Git's own
// rebase state (`rebaseState`), never by trusting the "ours"/"theirs"
// conflict-marker labels the merge machinery reuses for rebase: those labels
// are reversed relative to an ordinary merge (see
// `.claude/skills/dough-product-backlog/references/merge-conflicts.md`).
// A stopped rebase is resumed only by an explicit human decision, validated
// against this tool's own invariants and never re-run through disputed
// reconciliation — exactly the same recovery discipline slice 1 established
// for merges, reused here rather than reimplemented. This never selects a
// side, aborts, resets, or repairs a conflict itself: on any refusal the
// affected Git state — rebase metadata, index, and worktree — is left
// exactly as Git already had it, and the branch actually being rebased is
// never moved (Git itself defers that to the rebase's own completion), so
// the unpublished suffix stays exactly where it was for a human to resolve
// and then explicitly continue.
//
// A replay that finishes with no Git conflict anywhere in it is not
// automatically trusted, either: it is gated once more by the whole-operation
// aggregate comparison in `product-backlog-git-rebase-aggregate.mjs`
// (`acceptCleanRebase`) before being reported as accepted — see that module
// for the mechanism, the empirical cases that motivate it, and why
// `continueOperation`'s own clean finish, below, is deliberately excluded.
import {
  acceptCleanRebase,
  validateOperation,
} from "./product-backlog-git-rebase-aggregate.mjs";
import { acceptStaged } from "./product-backlog-git-candidate.mjs";
import { runGitOperationCli } from "./product-backlog-git-cli.mjs";
import { rebaseState } from "./product-backlog-git-operation-state.mjs";
import {
  blockedStopMessage,
  ensureDriverRegistered,
  gitLine,
  gitOutcome,
  repositoryRoot,
} from "./product-backlog-git-repository.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

export {
  ensureDriverRegistered,
  rebaseState,
  repositoryRoot,
  validateOperation,
};

// Continuing a rebase must never stop to open an editor: every step reuses
// the replayed commit's own recorded message, the same way this gate never
// invents commit content of its own. Only these two Git calls need it.
const noEditor = { GIT_EDITOR: "true", EDITOR: "true" };

// The real identification a human needs to resolve a stopped replay: which
// commit is being replayed, its real (pre-rebase) parent, and the real
// commit this step is replaying onto — spelled from Git's own rebase state,
// never from "ours"/"theirs".
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

// The recoverable shape of a stop this gate did not itself decide: something
// other than this path is still unresolved, so nothing about this path's own
// state is judged. Mirrors `commitAcceptedMerge`'s "blocked" outcome.
function interpretStop(repoRoot, file, outcome) {
  const unresolved = gitLine(["ls-files", "-u", "--", file], repoRoot);
  if (unresolved !== "") {
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
    message: blockedStopMessage("rebase", file, outcome.stderr),
  };
}

// One authorized rebase: every commit Git replays that touches the backlog
// path is gated by the shared resolver (through the driver registered
// above), and a replay that finishes without a single Git conflict on this
// path is then gated once more by the whole-operation aggregate comparison
// (`acceptCleanRebase`) before being reported as accepted.
//
// `preRebaseTip`/`destinationAtStart` default to the branch actually being
// replayed and the commit it is replayed onto, captured before Git moves
// anything. They only name the whole-rebase aggregate endpoints. They do not
// choose which commits Git replays.
//
// Without `--onto`, this is `git rebase <ref>` of the current branch, or
// `git rebase <ref> <branch>` when a branch that is not checked out is named.
// With `--onto`, `--ref` is the upstream cutoff and Git replays only the
// commits after that cutoff:
// `git rebase --onto <onto> <ref> [<branch>]`.
export function rebaseOperation({
  repoRoot,
  file,
  ref,
  onto,
  branch,
  "pre-rebase-tip": explicitPreRebaseTip,
  "destination-at-start": explicitDestinationAtStart,
}) {
  ensureDriverRegistered(repoRoot, file);
  const preRebaseTip =
    explicitPreRebaseTip ?? gitLine(["rev-parse", branch ?? "HEAD"], repoRoot);
  const destinationAtStart =
    explicitDestinationAtStart ?? gitLine(["rev-parse", onto ?? ref], repoRoot);

  const args = ["rebase"];
  if (onto) {
    args.push("--onto", onto, ref);
    if (branch) args.push(branch);
  } else if (branch) {
    args.push(ref, branch);
  } else {
    args.push(ref);
  }

  const outcome = gitOutcome(args, repoRoot, noEditor);
  if (outcome.code === 0) {
    return acceptCleanRebase(
      repoRoot,
      file,
      onto ?? ref,
      preRebaseTip,
      destinationAtStart,
    );
  }
  return interpretStop(repoRoot, file, outcome);
}

// Resumes a rebase this tool already stopped, once a human has supplied a
// resolution: validates what is actually staged for this path — never
// re-running the reconciliation the human's decision may deliberately differ
// from — and only then lets Git continue replaying the remaining suffix. An
// unresolved path, a staged candidate that fails this tool's invariants, or
// one that does not match the working tree all stay stopped exactly where
// they were, with every later commit still unreplayed and unpublished. If
// continuing surfaces a fresh conflict on this same path for the next
// commit, that is reported the same way the first stop was, not conflated
// with an invalid supplied resolution.
export function continueOperation({ repoRoot, file }) {
  if (!rebaseState(repoRoot)) {
    throw new BacklogError(`No rebase is in progress in ${repoRoot}.`);
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
  const continued = gitOutcome(["rebase", "--continue"], repoRoot, noEditor);
  if (continued.code === 0) {
    return {
      status: "rebased",
      message:
        `The rebase completed; every replayed commit is present and ` +
        `unpublished.`,
    };
  }
  return interpretStop(repoRoot, file, continued);
}

const usage =
  `Usage:\n` +
  `  product-backlog-git-rebase.mjs rebase --ref <upstream> ` +
  `[--onto <newbase>] [--branch <branch>]\n` +
  `    [--file <path>] [--cwd <dir>]\n` +
  `    [--pre-rebase-tip <ref>] [--destination-at-start <ref>]\n` +
  `  product-backlog-git-rebase.mjs continue [--file <path>] [--cwd <dir>]\n` +
  `  product-backlog-git-rebase.mjs validate [--file <path>] [--cwd <dir>]\n`;

await runGitOperationCli({
  argv: process.argv.slice(2),
  primaryName: "rebase",
  usage,
  primaryOperation: rebaseOperation,
  continueOperation,
  validateOperation,
  failingStatuses: ["conflict", "refused", "blocked", "disputed"],
  extraOptions: {
    onto: { type: "string" },
    branch: { type: "string" },
    "pre-rebase-tip": { type: "string" },
    "destination-at-start": { type: "string" },
  },
});
