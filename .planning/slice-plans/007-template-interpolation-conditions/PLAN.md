# Template-interpolation condition counting

**Mapped story identity:** SEED-019#template-interpolation-conditions
**Source:** [canonical story](../../seeds/SEED-019-template-interpolation-conditions.md#template-interpolation-conditions)

## Goal and scope

For JS/TS/JSX/TSX analyzer users, conditions inside template interpolations
contribute to CCN (story `label` → CCN 3), and
[PR #545](https://github.com/terryyin/lizard/pull/545) receives one complete
disposition under
[REVIEW-CONTRACT.md](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md):
performance-qualified exact-head merge with thanks, or an evidenced further
deferral when the six-gate paired 95% upper bounds stay ≥3% after an eligible
≤15-minute hot-path trim.

**In scope:** Re-adapt the reviewed `interpolation_code` / atomic-template scan
path onto current trunk (not a blind drop of the review-time corrected tree);
semantic proof on pr545 fixtures and focused JS/TS consumers; six-gate harness
(`docs/reviews/fabio-malpezzi-20261027/paired.py` SHA256
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`); full
suite/`make`/required CI before merge; exact-head merge and acknowledgement —
or linked further deferral.
**Excluded:** Unrelated language readers; performance-harness redesign;
reopening #546 keyword-member scope (guard only).
**Architecture / PFE:** Extend existing `TypeScriptReader` /
`javascript_templates` owners under Accepted ADR-0000 and ADR-0001. No parallel
parser. No new consequential architectural direction.
**Assumptions:** Attempt8's incremental names/counts failure (+4.767% upper) is
evidence against the prior adapted candidate as measured; trunk has since gained
#546/#547 so product files must be re-adapted. Contract 15-minute product-edit
budget (excluding review/measurement) still bounds eligible trim and
re-adaptation; if a fresh estimate exceeds it, stop with further deferral.

## Outside-in proof

Public `lizard.analyze_file.analyze_source_code` on
[pr545/inputs.json](../../../docs/reviews/fabio-malpezzi-20261027/pr545/inputs.json)
fixtures (especially `label.*` CCN 3); focused JS/TS suite from the contract;
six incremental/cumulative paired workloads via
[pr545/measure.py](../../../docs/reviews/fabio-malpezzi-20261027/pr545/measure.py);
then full pytest, `make`, required CI, exact-head merge guard, and thank-you URL
— or a linked further-deferral record when gates cannot pass.

## Settled decisive premises

Observed in this preparation workspace at `a514a98` (2026-10-28):

| Premise | Consuming slices | Observation | Result |
| --- | --- | --- | --- |
| Story `label` still under-counts on current trunk | 1, 2 | `analyze_source_code` on `` `${x ? 1 : 2} ${x && y}` `` for `.js`/`.ts`/`.jsx`/`.tsx` → CCN 1 each | Defect remains |
| PR #545 still open at frozen head | 2 | `gh pr view 545` → OPEN, `baa95a4df8278421bb3d3a6487516030cc03a979`; 4 build checks SUCCESS | True — refresh again before merge |
| Prior adapted candidate restores `label` CCN 3 when its corrected product is applied | 1 | Scratch-copied cache `corrected/` product files → CCN 3 on all four extensions; reverted | Semantic fix still works in isolation |
| Blind drop of review-time corrected tree is unsafe on current trunk | 1 | HEAD differs from review target for `typescript.py` and `tsx.py` (#546/#547); HEAD-vs-corrected differs on all four product files | Must re-adapt onto current readers |
| Contract paired harness bytes remain available | 2 | `docs/reviews/fabio-malpezzi-20261027/paired.py` and cache `pr545/proof/paired.py` SHA256 `ec1c9c5e…76520` | True |
| #546 keyword-member rule holds on trunk for `${p.catch(x)}` | 1 | `analyze_source_code` → CCN 1 | Guard baseline — preserve after interpolation path |

Unsettled by design (authorized early probe inside slice 2): whether the
**re-adapted** candidate's six paired 95% upper bounds are all &lt;3% on a quiet
machine. Attempt8 settles only the prior candidate.

## Ordered slices

### 1. Restore interpolation condition CCN on current trunk
Type: Behavior
Status: planned
Proof: `label.*` and discriminating `inputs.json` fixtures show expected CCN via
`analyze_source_code`; focused
`test/test_languages/testJavaScript*.py testTypeScript*.py testES6.py testJSX.py testTSX.py testVue.py`;
`${p.catch(x)}` stays CCN 1; no merge in this slice.
Behavior: Given current trunk under-counts conditions inside template
interpolations → re-adapt `interpolation_code` and the TSX/JSX atomic-template
scan onto current `TypeScriptReader` / `javascript_templates` (preserving
#546/#547) → public analysis reports CCN 3 for story `label` across the four
extensions with nested/literal/comment/regex/boundary controls intact.
Safe stop: Corrected semantics exist locally with proof; users are unchanged
until slice 2 disposes the PR.

### 2. Performance-qualify and dispose PR #545
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/545 — frozen head
`baa95a4df8278421bb3d3a6487516030cc03a979` (refresh before publish)
Proof: All six contract comparisons' paired 95% upper bounds strictly &lt;3%
(or evidenced further deferral after one eligible ≤15-minute hot-path trim);
full pytest/`make`/required CI; exact-head merge acknowledgement URL — or
linked follow-up when still failing.
Behavior: Given slice 1's re-adapted candidate → early-probe the six-gate harness
on a quiet machine against current target and starting baseline → if any gate
fails, apply at most one ≤15-minute hot-path trim and re-measure once under a
prospectively fixed quiet setup → if gates pass, publish to the contributor head
without force, merge guarded by exact head, post thanks; if gates still fail or
the trim/re-adapt estimate exceeds the 15-minute product budget, leave #545
unmerged with a linked further deferral and do not repeatedly retime the same
failed candidate.
Depends on slice 1. Early probe owns the first six-gate observation of the
re-adapted tree; its failure routes to trim-or-deferral inside this same slice
rather than a separate outcome.

## Current decisions

- Two Behavior slices: semantic re-adapt first; performance disposition second.
- Reuse reviewed design (`interpolation_code`, `_scan_atomic_templates` for
  TSX/JSX only); do not invent a parallel counter.
- No project S/M/L band or numeric slice timing policy; the contract's 15-minute
  product-edit limit (excluding review/measurement) bounds eligible trim and
  fresh re-adaptation overruns.
- Keyword-member residual from the original #545 review is not a deferred
  promise here after #546; treat it as a regression guard only.

## Learnings

- Review-time corrected blobs restore CCN 3 but diverge from current
  `typescript.py` / `tsx.py` after #546/#547; execution must merge the concept,
  not replace whole files from the old corrected tree.
- Attempt8 already consumed the justified quiet longer retry for that prior
  candidate; a new candidate (re-adapted and/or trimmed) may be measured under
  the contract, but the same bytes must not be re-timed seeking a pass.

## Concern evidence

- Slice-plan refinement: not needed — two ordered Behaviors match the refined
  story, proof is mapped, decisive premises for approach were observed in this
  checkout, and the only remaining performance fact is an authorized early probe
  inside slice 2.
- Remaining slice-specific concerns: none identified beyond that probe (which
  does not block readiness).

## Story obligations

None yet. During execution, every reported gap, loss, limitation, or interim
behavior uses this section with
**Source:** [canonical story](../../seeds/SEED-019-template-interpolation-conditions.md#template-interpolation-conditions).
