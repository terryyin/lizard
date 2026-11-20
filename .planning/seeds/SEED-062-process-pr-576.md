---
id: SEED-062
status: active
planted: 2026-11-20
planted_during: lizard process pr
scope: cognitive complexity nested JS/TS brackets from PR #576
---

# SEED-062: Process PR #576

<a id="process-pr-576"></a>

### Process PR #576 — Fix cognitive complexity of JS/TS functions that contain nested functions

**Identity:** SEED-062#process-pr-576
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"85e89c9c0048bc8421484443eb94b20b9eb24077d9b88bf522cac8d6a3585f5b"}}
```

**Goal:** Maintainers disposing
[PR #576](https://github.com/terryyin/lizard/pull/576), and users measuring
JavaScript or TypeScript with nested callbacks, receive one complete
disposition: `-Ecognitive` restores outer-function scores after nested
functions open and close brackets between the outer function's brackets, when
the contribution remains valid on current trunk, or the PR stays unmerged with
an evidence-backed reason and any required follow-up linked.

**Source.** Dashboard instruction `lizard process pr` for PRs created or
updated in the past 24 hours. Full process-PR scope: review, reproduce against
current trunk, correct on the contributor branch when that route exists,
qualify, and merge when the skill's gates hold.

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
users of `-Ecognitive` on JS/TS whose nested callbacks currently under-count
the outer function.

**Scope.** This pull request only. Compare the claimed nested-callback case and
nearby controls (template interpolations, JSX text brackets, logical operators)
on current trunk through `analyze_file` / the cognitive extension. Correct
remaining defects on a reachable contributor branch, preserving contributor
history. Qualify the final candidate with the full suite, style checks, and
performance screening for the cognitive counter path. Merge only when those
gates and required hosted checks hold; otherwise record the precise unmet gate.

Deferred: PR #577's abandoned-arrow case, the separate ternary-comma bug named
in the PR body, and non-JS/TS readers. Rejection constraints: do not merge an
inadequate, obsolete, or conflicted candidate; do not weaken discriminating
expectations; do not accept a material runtime regression without an explicit
recorded decision.

**Key examples.** Claims to verify on current trunk and on the proposal.

- Given a JS function with three sibling `if`s after a nested `.some(role =>
  ...)`, cognitive complexity is 3 (trunk reports 1).
- Given nested functions with template interpolations or JSX text that contain
  brackets, outer-function scores remain correct and those brackets do not
  close code blocks.
- CCN and other non-cognitive metrics stay unchanged for the same inputs.
- Already-correct trunk, an unworthy change, or a correction that cannot be
  published on the contributor branch receives a recorded reason instead of a
  merge.

**Direction:** Within "fix or improve existing functionality"; no external
signature change.

Intake metadata: head `62f23c8f079567a03fb69e8be36a9691cd6cc583` on
`AbdelrahmanHafez:fix/cognitive-js-nested-function-brackets`; base `master`
at `e8d18d2e`; `maintainer_can_modify` true; merge state MERGEABLE; evidence
under `.planning/evidence/pr-576/`.

**Disposition (2026-11-20):** Review completed; **not merged**. Correctness accepted; hosted CI green on `62f23c8f`. Default 3% paired `-Ecognitive` API screening failed on a nested-callback-heavy corpus (median ~+12%, 95% CI ~+4%…+21%); mixed corpus inconclusive under high load. Review comment: https://github.com/terryyin/lizard/pull/576#issuecomment-7030458547. Follow-up: quiet-machine remeasure / cheap `_route` tightening, then merge.
