---
id: SEED-019
status: active
planted: 2026-10-28
planted_during: SEED-010#review-lexical-js-prs wrap-up product advice
trigger_when: Land PR #545 once template-interpolation CCN is performance-qualified
scope: small
---

# SEED-019: Template-interpolation condition counting

## Why This Matters

JS/TS/JSX/TSX users need conditions inside template interpolations to contribute
to CCN. SEED-010#review-lexical-js-prs left PR #545 unmerged: the adapted
candidate proves `label` CCN 3, but attempt8 incremental names/counts paired
95% upper CI is 4.767% and fails the strict &lt;3% gate. Evidence and adapted
path live under
[docs/reviews/fabio-malpezzi-20261027/pr545/](../../docs/reviews/fabio-malpezzi-20261027/pr545/).

## Selected Stories

<a id="template-interpolation-conditions"></a>

### 1. Count conditions inside template interpolations under the performance gate

**Identity:** SEED-019#template-interpolation-conditions

For JS-family analyzer users, analyzing a template whose interpolations contain
conditions (story `label` returning `` `${x ? 1 : 2} ${x && y}` ``) reports
CCN 3 across `.js` / `.ts` / `.jsx` / `.tsx`, while nested templates, literal
text, strings, comments, regex, and function boundaries stay correct. Land the
fix only when all six contract performance comparisons’ paired 95% upper
bounds are strictly below 3% on a quiet machine (or after a ≤15-minute hot-path
trim that achieves that), then exact-head merge and thank the contributor.
Preserve the deferred keyword-member-inside-interpolation residual until this
lands. Source PR: https://github.com/terryyin/lizard/pull/545. Contract:
[REVIEW-CONTRACT.md](../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md).

**Evaluation:** Reproduce target red → corrected green on the pr545 fixtures;
focused JS/TS suite including reached consumers; full suite/`make`; six-gate
harness SHA256
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`; required CI;
exact-head merge and thanks — or an evidenced further deferral.

**Completion:** Merged acknowledgement on #545, or a linked further follow-up
when the gate still fails after an eligible trim.
