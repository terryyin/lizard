# PR540 switch label boundaries

Disposition: **already fixed / unnecessary** under SEED-010#review-structural-js-prs
slice 4. Left unmerged; PR closed with acknowledgement.

Frozen head: `d225a6d60af856ac155b882d1a3770b7d66457ef` (was OPEN at review).

## Evidence

On current target, claim fixture reports both anonymous callbacks and `a` 1–6
CCN 2. PR contributor cases (functions after case label including ternary case
labels; block/template after case) pass for `.js`/`.ts`/`.jsx`/`.tsx`. Trunk
owns the behavior via `case` in `_MARKER_TOKENS` and `default` via `last_tokens`
with `_ternary_depth`, not a parallel `_case_labels` counter (ADR-0000/0001).

Focused JS/TS suite: 641 passed, 5 skipped. Empty product change.
