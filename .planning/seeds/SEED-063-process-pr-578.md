---
id: SEED-063
status: active
planted: 2026-11-20
planted_during: lizard process pr
scope: JS/TS nesting without semicolons from PR #578
---

# SEED-063: Process PR #578

<a id="process-pr-578"></a>

### Process PR #578 — Fix nesting depth and nested structures for JS/TS code without semicolons

**Identity:** SEED-063#process-pr-578
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"5f5d964620e977caa57264593d0a5c388919eb15a727ab23284e0843c7cb9c29"}}
```

**Goal:** Maintainers disposing
[PR #578](https://github.com/terryyin/lizard/pull/578), and users measuring
JavaScript or TypeScript that omits semicolons, receive one complete
disposition: `-END` and `-ENS` treat sibling braceless statements ended by
implied semicolons as one nesting level, when the contribution remains valid
on current trunk, or the PR stays unmerged with an evidence-backed reason and
any required follow-up linked.

**Source.** Dashboard instruction `lizard process pr` for PRs created or
updated in the past 24 hours. References closed issue #573's braceless nesting
theme for the no-semicolon variant. Full process-PR scope applies.

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
users comparing ND/NS across semicolon and ASI styles.

**Scope.** This pull request only. Reproduce the claimed ASI braceless-`if`
case and controls that must keep a statement open across a line break.
Inspect `ImpliedSemicolons` and the ND/NS extension hooks. Correct remaining
defects on a reachable contributor branch, preserving contributor history.
Qualify with the full suite, style checks, and performance screening for the
JS statement path. Merge only when gates and required hosted checks hold.

Deferred: the separate reader bug for `ready = (minor < 2)` without `;` named
in the PR, minified-bundle outliers, and non-JS/TS languages. Rejection
constraints as for other process-PR owners.

**Key examples.**

- Given four sibling braceless `if`s without semicolons, ND and NS are 1
  (trunk reports higher ND).
- Given the same code with explicit semicolons, ND and NS match the pre-PR
  numbers.
- Given a statement that must continue after a line break (binary operator,
  etc.), nesting does not close early.
- Already-correct trunk or an unworthy change receives a recorded reason
  instead of a merge.

**Direction:** Within "fix or improve existing functionality"; no external
signature change.

Intake metadata: head `82fc44cb78d4e50676633868e11f1dbf5ac344ba` on
`AbdelrahmanHafez:fix/nesting-js-braceless-if-no-semicolon`; base `master`
at `e8d18d2e`; `maintainer_can_modify` true; merge state MERGEABLE; evidence
under `.planning/evidence/pr-578/`.

**Disposition (2026-11-20):** Review completed; **not merged**. Correctness accepted; hosted CI green on `82fc44cb`. Paired `-End`/`-Ens` API screening failed default 3% gate (ND ~+15%, NS ~+23%). Review comment: https://github.com/terryyin/lizard/pull/578#issuecomment-7030458862. Follow-up: cheaper shared `ImpliedSemicolons` path, then remeasure and merge.
