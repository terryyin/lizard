#!/usr/bin/env node
// The clean-finish half of gating a multi-commit cherry-pick, split out of
// `product-backlog-git-cherry-pick.mjs` for cohesion, mirroring
// `product-backlog-git-rebase-aggregate.mjs`'s split from
// `product-backlog-git-rebase.mjs`. A single-commit pick's own driver
// invocation already receives the true, correct three-way triple (see
// `product-backlog-git-cherry-pick.mjs`'s own header), so it never reaches
// this module; only a sequence of two or more revisions applied in one
// `pick` invocation does.
//
// Confirmed empirically, reusing slice 3's own masking mechanism (an earlier
// step absorbs a destination's concurrent change, unopposed as far as any
// single step can see): a multi-commit cherry-pick sequence composes to a
// Git-conflict-free result the whole-operation comparison can still refuse,
// the same shape slice 3 found for a multi-commit rebase. Treating the last
// supplied revision as the picked line's own tip and the branch checked out
// before the pick began as the true destination-at-start reduces this to
// exactly the same `aggregateOutcome` call `acceptCleanRebase` makes.
//
// `pickOperation`'s own `continueOperation` finish (after a human has
// resolved a real Git conflict, or decided Git's own "empty" stop, partway
// through the same sequence) is deliberately never routed through this
// module — the same documented boundary and accepted gap
// `product-backlog-git-rebase-aggregate.mjs` carries for the same reason:
// re-running the aggregate there could falsely re-dispute an already-made
// human decision, or would need state tracked across the whole sequence,
// which is out of this slice's authority.
import { aggregateOutcome } from "./product-backlog-git-aggregate.mjs";

// The whole-operation aggregate check for a cherry-pick sequence that
// finished with no Git conflict on this path at any step: real evidence, not
// a re-decision. It never selects a side or repairs anything — a disputed
// outcome leaves the picked commits exactly where the pick left them, on the
// branch, unpublished and fully recoverable, for a human to repair or
// explicitly accept and then have the managed caller validate (`validate`)
// before publishing.
export function acceptCleanPick(repoRoot, file, revisions, destinationAtStart) {
  const pickedTip = revisions.at(-1);
  const check = aggregateOutcome({
    repoRoot,
    file,
    preOperationTip: pickedTip,
    destinationAtStart,
    resultRef: "HEAD",
  });
  if (!check.agrees) {
    return {
      status: "disputed",
      message:
        `${revisions.join(" ")} applied with no Git conflict in ${file} at ` +
        `any step, but the whole-operation aggregate comparison of the ` +
        `picked line's own tip (${pickedTip}) against the true ` +
        `destination-at-start (${destinationAtStart}) disagrees with the ` +
        `result. This is not a Git conflict, so there is nothing staged to ` +
        `resolve in the usual way: the picked commits are left exactly ` +
        `where the pick completed them — on the branch, unpublished and ` +
        `fully recoverable. Repair ${file} by hand, or decide the current ` +
        `result should stand as is; either way, run \`validate\` on it ` +
        `before it is published.\n${check.refusal}`,
    };
  }
  return {
    status: "picked",
    message:
      `${revisions.join(" ")} applied; no Git conflict touched ${file}, and ` +
      `the whole-operation aggregate comparison agrees with the result.`,
  };
}
