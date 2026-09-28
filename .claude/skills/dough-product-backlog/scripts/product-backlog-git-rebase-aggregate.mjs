#!/usr/bin/env node
// The clean-finish half of gating a rebase, split out of
// `product-backlog-git-rebase.mjs` for cohesion: a replay that finishes with
// no Git conflict anywhere on this path is not automatically trusted either.
// `rebaseOperation`'s own single, unstopped `git rebase` call still only ever
// reconciled each commit against that commit's own immediate parent and the
// destination as this same operation had already left it — never the
// operation's true pre-rebase tip or true destination-at-start — so it is
// gated here by `aggregateOutcome`'s whole-operation comparison before being
// reported as accepted (see `product-backlog-git-aggregate.mjs` for the
// mechanism and the empirical cases that motivate it).
//
// A rebase that instead stopped for a real Git conflict on this path, was
// resolved by a human, and finished via `continueOperation` is deliberately
// never routed through this module: confirmed empirically, re-running the
// whole-operation aggregate there — using the true pre-rebase tip, which
// still carries the original, disputed content the human's decision
// deliberately overrode — reproduces the same dispute as a false refusal of
// an already-accepted decision. `continueOperation`'s own clean finish stays
// an ordinary, ungated completion, the same boundary slice 2 already
// documented; only `product-backlog-git-rebase.mjs` decides which finished
// path a rebase took, and only calls into this module for the clean one.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { aggregateOutcome } from "./product-backlog-git-aggregate.mjs";
import { validateCandidate } from "./product-backlog-git-candidate.mjs";

// The whole-operation aggregate check for a rebase that finished with no Git
// conflict on this path at any step: real evidence, not a re-decision. It
// never selects a side or repairs anything — a disputed outcome leaves the
// completed rebase's local commits exactly where the rebase left them, on
// the branch, unpublished and fully recoverable, for a human to repair or
// explicitly accept and then have the managed caller validate (`validate`)
// before publishing.
export function acceptCleanRebase(
  repoRoot,
  file,
  ref,
  preRebaseTip,
  destinationAtStart,
) {
  const check = aggregateOutcome({
    repoRoot,
    file,
    preOperationTip: preRebaseTip,
    destinationAtStart,
    resultRef: "HEAD",
  });
  if (!check.agrees) {
    return {
      status: "disputed",
      message:
        `Rebased onto ${ref} with no Git conflict in ${file} at any step, ` +
        `but the whole-rebase aggregate comparison of the true pre-rebase ` +
        `tip (${preRebaseTip}) against the true destination-at-start ` +
        `(${destinationAtStart}) disagrees with the result. This is not a ` +
        `Git conflict, so there is nothing staged to resolve in the usual ` +
        `way: the rebase's local commits are left exactly where the ` +
        `rebase completed them — on the branch, unpublished and fully ` +
        `recoverable. Repair ${file} by hand, or decide the current result ` +
        `should stand as is; either way, run \`validate\` on it before the ` +
        `managed caller publishes.\n${check.refusal}`,
    };
  }
  return {
    status: "rebased",
    message:
      `Rebased onto ${ref}; no Git conflict touched ${file}, and the ` +
      `whole-rebase aggregate comparison of the true pre-rebase tip and the ` +
      `true destination-at-start agrees with the result.`,
  };
}

// Read-only recovery for a "disputed" whole-rebase result: never a Git
// conflict, so there is nothing staged to `git add`, and nothing here
// writes, amends, or moves anything. Validates whatever a human has left as
// the branch's real, current bytes for `file` — their own hand repair, or
// the automatic candidate accepted exactly as it stood — against this
// tool's own invariants, the same way `acceptStaged` validates a resolved
// merge/rebase conflict, and never re-runs the aggregate reconciliation a
// human's decision may deliberately differ from.
export function validateOperation({ repoRoot, file }) {
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
