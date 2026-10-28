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
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/007-template-interpolation-conditions/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"d54d9c29ff73e6d9069ea896e66fa26d53abc98dd602af5fbad8ca52603346ab","plan":"4020ac2a96a87c02d9964f8650afd55b63a4244dd73d3a13b40d76c46418ddb0"}}
```

**Goal:** For JS/TS/JSX/TSX analyzer users who rely on CCN and warning thresholds,
conditions written inside template interpolations contribute to each function's
CCN the same way they do outside templates, and PR #545 lands only after that
fix is performance-qualified under the review contract — with exact-head merge
and contributor thanks, or an evidenced further deferral if the gate still fails
after an eligible hot-path trim.

**Scope:** Own
[PR #545](https://github.com/terryyin/lizard/pull/545) (frozen contributor head
`baa95a4df8278421bb3d3a6487516030cc03a979`, still OPEN at refinement) through a
complete disposition under
[REVIEW-CONTRACT.md](../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md).
Reuse the adapted candidate and review record under
[pr545/](../../docs/reviews/fabio-malpezzi-20261027/pr545/) (DISPLAY-ONLY
`adapted.diff`; executable bytes in `literal-git-patches.json`): count conditions
inside `${…}` on the `TypeScriptReader` fast path via `interpolation_code`, with
atomic-template backtick scans limited to TSX/JSX. Required behavior: story
`label` returning `` `${x ? 1 : 2} ${x && y}` `` reports CCN 3 across `.js` /
`.ts` / `.jsx` / `.tsx`; nested templates, literal text, strings, comments,
regex, and following function boundaries stay correct. Land only when all six
contract performance comparisons' paired 95% upper bounds are strictly below 3%
on a quiet machine, or after a ≤15-minute hot-path trim that achieves that
(harness SHA256
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`). Then
exact-head merge and thank the contributor. Refresh PR state/head and current
target before acting.

Deferred promises: unrelated language readers; redesign of the performance
harness. #546 already landed the keyword-member fix on trunk; this story only
guards that `${p.catch(f)}` (and similar) stay non-conditions after the
interpolation path is added — it does not reopen keyword-member scope.
Rejection constraints: do not merge without all six gates' paired 95% upper
bounds strictly below 3%; do not repeatedly time the same invalid or
already-failed candidate to seek a pass; do not invent a parallel parser.
Assumptions: attempt8's incremental names/counts failure (+4.767% upper) is
current evidence against the adapted candidate as-is; a justified quiet longer
retry already ran; ADR-0001 governs CCN meaning and ADR-0000 keeps human
architectural ownership — no new Architecture section for this extension of
existing reader behavior.

**Key examples:**

- Given `function label(x, y) { return \`v: ${x ? 1 : 2} ${x && y}\`; }` on
  current trunk, public analysis reports CCN 1 for `.js` / `.ts` / `.jsx` /
  `.tsx`; after the qualified fix it reports CCN 3 on each extension.
- Given nested interpolations
  `` `v: ${x ? `w: ${y ? 1 : 2}` : ''}` ``, analysis counts both conditions
  (CCN 3) without breaking the following function's boundary.
- Given interpolations whose apparent operators are only in string/regex/comment
  literals (`` `if (x) ${'&& ?'} ${x /* || */}` `` and similar controls), those
  literals do not inflate CCN; a real condition in a regex `.test` still counts.
- Given the same ternary/`&&` expressions outside a template, CCN stays correct
  (control that interpolation counting does not regress ordinary paths).
- Given a candidate that restores CCN 3 but whose incremental names/counts
  paired 95% upper bound is ≥3% (as attempt8 showed), the story does not merge;
  it either trims within ≤15 minutes to pass all six gates or records a linked
  further deferral.

**Evaluation:** Reproduce target red → corrected green on the pr545 fixtures;
focused JS/TS suite including reached consumers; full suite/`make`; six-gate
harness SHA256
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`; required CI;
exact-head merge and thanks — or an evidenced further deferral.

**Completion:** Merged acknowledgement on #545, or a linked further follow-up
when the gate still fails after an eligible trim.
