#!/usr/bin/env node
// The Git-generic whole-operation comparison a multi-step replay's own
// per-step reconciliation cannot make for itself. A rebase's merge driver is
// invoked once per replayed commit, and each invocation only ever sees that
// commit's own immediate parent as "ancestor" and the destination as this
// same operation has already left it after replaying the prior steps --
// never the operation's true pre-operation branch tip or its true
// destination-at-start. A sequence of steps that are each individually clean
// (no Git conflict) can still compose into a result the whole-operation
// comparison would refuse.
//
// Confirmed empirically in a scratch repository before this was written, for
// two mechanisms that never surface a Git conflict at any step: (1) an
// earlier commit changes a value to coincidentally match the destination's
// own concurrent change, silently absorbing the divergence, and a later
// commit changes the same value again, unopposed as far as any single step
// can see, composing to a result a whole-branch comparison of the two real
// final values would refuse as a genuine two-sided direction dispute; (2) the
// same mechanism for a moved list entry, composing to the duplicate-move
// regression slice 1 already proved for an ordinary merge. A genuinely
// compatible multi-commit rebase (each side's commits touch different
// entries) was confirmed not to trigger a false refusal here.
//
// This performs no repair and makes no decision of its own: it runs the same
// shared resolver (`mergeBacklogs`) once more, over the true whole-operation
// ancestor and both true endpoints, and reports whether its own accepted
// candidate agrees with the bytes the operation actually left at
// `resultRef`. Nothing here is rebase-specific, and nothing here assumes
// "the destination is always `main`" or "the pre-operation tip is always
// whatever `ORIG_HEAD` says right now": `preOperationTip` and
// `destinationAtStart` are the caller's real revisions to supply, so a
// caller with different actual boundaries (a rejected-push retry rebasing
// only an unpublished suffix, an execution-branch replay onto trunk) can
// reuse this unchanged.
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { git, gitLine } from "./product-backlog-git-repository.mjs";
import { mergeBacklogs } from "./product-backlog-merge.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

function contentAt(repoRoot, file, ref) {
  try {
    return git(["show", `${ref}:${file}`], repoRoot);
  } catch {
    throw new BacklogError(
      `${file} is not present at ${ref}, so the whole-operation aggregate ` +
        `comparison cannot be run.`,
    );
  }
}

function withTemporaryVersions(contents, run) {
  const directory = mkdtempSync(join(tmpdir(), "dough-backlog-aggregate-"));
  try {
    const paths = contents.map((content, at) => {
      const path = join(directory, `version-${at}.md`);
      writeFileSync(path, content, "utf8");
      return path;
    });
    return run(paths);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
}

// The real whole-operation reconciliation: the true merge-base of
// `preOperationTip` and `destinationAtStart`, and each side's real bytes at
// those exact revisions -- never a per-step ancestor or a moving
// destination. `resultRef` is what the operation actually left, most often
// `HEAD` once a rebase has finished. Returns `{ agrees: true }` when the
// shared resolver's own accepted candidate matches those bytes exactly, or
// `{ agrees: false, refusal }` otherwise -- either because the shared
// resolver itself refuses (a genuine two-sided dispute the per-step replay
// never had to surface), or because it accepts a candidate that differs from
// what the operation actually produced.
export function aggregateOutcome({
  repoRoot,
  file,
  preOperationTip,
  destinationAtStart,
  resultRef,
}) {
  const mergeBase = gitLine(
    ["merge-base", preOperationTip, destinationAtStart],
    repoRoot,
  );
  const ancestor = contentAt(repoRoot, file, mergeBase);
  const branch = contentAt(repoRoot, file, preOperationTip);
  const destination = contentAt(repoRoot, file, destinationAtStart);
  const actual = contentAt(repoRoot, file, resultRef);

  return withTemporaryVersions(
    [ancestor, branch, destination],
    ([ancestorPath, branchPath, destinationPath]) => {
      try {
        const outcome = mergeBacklogs({
          ancestor: ancestorPath,
          branches: [branchPath, destinationPath],
        });
        if (outcome.source !== actual) {
          return {
            agrees: false,
            refusal:
              `The shared resolver's own whole-operation candidate for ` +
              `${file} does not match what the operation actually left at ` +
              `${resultRef}, comparing the true pre-operation tip ` +
              `(${preOperationTip}) against the true destination-at-start ` +
              `(${destinationAtStart}).`,
          };
        }
        return { agrees: true };
      } catch (error) {
        if (!(error instanceof BacklogError)) {
          throw error;
        }
        return { agrees: false, refusal: error.message };
      }
    },
  );
}
