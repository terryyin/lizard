#!/usr/bin/env node
// Adapts one authorized cherry-pick of one or more commits onto the current
// branch to real Git state, the cherry-pick-shaped counterpart to
// `product-backlog-git-merge.mjs` and `product-backlog-git-rebase.mjs`. Every
// commit Git applies that touches the backlog path goes through the same
// self-registering merge driver slice 1 built — confirmed empirically that
// `git cherry-pick` invokes it identically to `git merge`/`git rebase`, clean
// or conflicted alike — feeding the existing `mergeBacklogs` core unmodified.
//
// Unlike a rebase, cherry-pick's index stages during a stop are never
// reversed: stage 2 ("ours") is always the current branch (the destination),
// and stage 3 ("theirs") is always the commit being picked, confirmed
// empirically rather than assumed from rebase's own reversed convention. A
// stopped pick is identified from Git's own top-level `CHERRY_PICK_HEAD`
// marker (`cherryPickState`), never guessed — confirmed empirically to live
// there, under `.git/sequencer/`, never under rebase's own state directories
// — and resumed only by an explicit human decision, validated against this
// tool's own invariants and never re-run through disputed reconciliation.
// This never selects a side, aborts, resets, skips, or repairs a conflict
// itself: on any refusal the affected Git state is left exactly as Git
// already had it, for a human to resolve and then explicitly continue. Two
// stop shapes with no rebase analogue — a picked merge commit missing
// `--mainline`, and Git's own "this step is now empty" stop — are documented
// and handled in `product-backlog-git-cherry-pick-stop.mjs`.
//
// A single-commit pick's driver invocation already receives the true,
// correct three-way triple (the picked commit's own real parent as ancestor,
// the real destination-at-start as "ours", the real picked commit as
// "theirs") — confirmed empirically — so `validateCandidate`/`acceptStaged`
// alone are sufficient there, matching slice 3's own prediction that a single
// pick has no rebase-shaped composition gap. A multi-commit sequence
// (`git cherry-pick a b c` in one invocation), though, was confirmed
// empirically to reproduce the exact same "each step locally clean, aggregate
// wrong" composition slice 3 found for rebase, reusing the same masking
// mechanism (an earlier pick absorbs a destination's concurrent change,
// unopposed as far as any single step can see) — so a clean multi-commit
// pick is gated once more by the whole-operation aggregate comparison in
// `product-backlog-git-cherry-pick-aggregate.mjs` before being reported as
// accepted, the same as `product-backlog-git-rebase-aggregate.mjs`'s
// clean-rebase gate, and carrying the same documented exclusion and accepted
// gap for `continueOperation`'s own finish (see that module's header).
import {
  acceptStaged,
  validateCandidate,
} from "./product-backlog-git-candidate.mjs";
import { acceptCleanPick } from "./product-backlog-git-cherry-pick-aggregate.mjs";
import { interpretStop } from "./product-backlog-git-cherry-pick-stop.mjs";
import { runGitOperationCli } from "./product-backlog-git-cli.mjs";
import { cherryPickState } from "./product-backlog-git-operation-state.mjs";
import {
  ensureDriverRegistered,
  gitLine,
  gitOutcome,
  repositoryRoot,
} from "./product-backlog-git-repository.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
// Read-only recovery for a "disputed" result is entirely Git/operation-
// generic (validates the branch's current bytes, writes nothing, re-runs no
// reconciliation) despite living beside the rebase adapter that first needed
// it; reused unchanged rather than duplicated.
import { validateOperation } from "./product-backlog-git-rebase-aggregate.mjs";

export {
  ensureDriverRegistered,
  repositoryRoot,
  validateCandidate,
  validateOperation,
};

// Cherry-pick's own `--continue` must never stop to open an editor: it
// reuses the picked commit's own recorded message (plus Git's own
// "(cherry picked from commit ...)" trailer), the same way this gate never
// invents commit content of its own.
const noEditor = { GIT_EDITOR: "true", EDITOR: "true" };

// One authorized cherry-pick of one or more commits: every commit Git
// applies that touches the backlog path is gated by the shared resolver
// (through the driver registered above). `ref` is one revision, or several
// separated by whitespace — the exact revisions Git's own `cherry-pick`
// accepts as multiple positional arguments, applied in the order supplied.
// A picked merge commit requires `mainline` (Git's own `-m <n>`); omitted for
// a non-merge pick, and required, undecided by this tool, for a merge pick.
//
// A sequence (more than one revision) that finishes with no Git conflict
// anywhere is gated once more by the whole-operation aggregate comparison,
// treating the last supplied revision as the picked line's own tip and the
// branch checked out before the pick began as the true destination-at-start
// — the same shape a multi-commit rebase of that same line onto this same
// destination would have. A single revision has no such gap (see this file's
// own header) and is reported accepted as soon as its own result validates.
export function pickOperation({ repoRoot, file, ref, mainline }) {
  ensureDriverRegistered(repoRoot, file);
  const revisions = ref.trim().split(/\s+/).filter(Boolean);
  if (revisions.length === 0) {
    throw new BacklogError("Supply at least one revision to --ref.");
  }
  const destinationAtStart = gitLine(["rev-parse", "HEAD"], repoRoot);

  const args = ["cherry-pick", ...revisions];
  if (mainline) {
    args.push("--mainline", String(mainline));
  }
  const outcome = gitOutcome(args, repoRoot, noEditor);

  if (outcome.code !== 0) {
    return interpretStop(repoRoot, file, mainline, outcome);
  }

  const outcomeValid = acceptStaged(repoRoot, file);
  if (!outcomeValid.valid) {
    return {
      status: "refused-after-commit",
      message:
        `${revisions.join(" ")} applied without a Git conflict in ${file}, ` +
        `but the result fails this tool's own invariants. The pick already ` +
        `committed — Git itself leaves no uncommitted window for a clean ` +
        `cherry-pick — so ${file} on the current branch is left exactly as ` +
        `the pick produced it, unpublished and fully recoverable, for a ` +
        `human to repair by hand or explicitly accept and then \`validate\` ` +
        `before it is published.\n${outcomeValid.reason}`,
    };
  }

  if (revisions.length === 1) {
    return {
      status: "picked",
      message: `${revisions[0]} applied cleanly onto ${destinationAtStart}.`,
    };
  }

  return acceptCleanPick(repoRoot, file, revisions, destinationAtStart);
}

// Resumes a cherry-pick this tool already stopped, once a human has supplied
// a resolution (a real conflict resolved by hand, or Git's own "empty" stop
// decided with `--skip`/`--allow-empty`): validates what is actually staged
// for this path — never re-running the reconciliation the human's decision
// may deliberately differ from — and only then lets Git continue applying
// any remaining picks. Deliberately never re-gated by the whole-operation
// aggregate on finish, the same documented boundary and accepted gap
// `product-backlog-git-rebase.mjs`'s own `continueOperation` carries.
export function continueOperation({ repoRoot, file }) {
  if (!cherryPickState(repoRoot)) {
    throw new BacklogError(`No cherry-pick is in progress in ${repoRoot}.`);
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
  const continued = gitOutcome(
    ["cherry-pick", "--continue"],
    repoRoot,
    noEditor,
  );
  if (continued.code === 0) {
    return {
      status: "picked",
      message:
        `The cherry-pick completed; every picked commit is present and ` +
        `unpublished.`,
    };
  }
  return interpretStop(repoRoot, file, undefined, continued);
}

const usage =
  `Usage:\n` +
  `  product-backlog-git-cherry-pick.mjs pick --ref <rev[ rev...]> ` +
  `[--mainline <n>] [--file <path>] [--cwd <dir>]\n` +
  `  product-backlog-git-cherry-pick.mjs continue [--file <path>] [--cwd <dir>]\n` +
  `  product-backlog-git-cherry-pick.mjs validate [--file <path>] [--cwd <dir>]\n`;

await runGitOperationCli({
  argv: process.argv.slice(2),
  primaryName: "pick",
  usage,
  primaryOperation: pickOperation,
  continueOperation,
  validateOperation,
  failingStatuses: [
    "conflict",
    "empty",
    "blocked",
    "refused-after-commit",
    "refused",
    "disputed",
  ],
  extraOptions: {
    mainline: { type: "string" },
  },
});
