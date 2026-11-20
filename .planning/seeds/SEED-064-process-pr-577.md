---
id: SEED-064
status: active
planted: 2026-11-20
planted_during: lizard process pr
scope: cognitive abandoned arrow groups from PR #577
---

# SEED-064: Process PR #577

<a id="process-pr-577"></a>

### Process PR #577 — Fix cognitive complexity of JS/TS expressions in parentheses after `=`

**Identity:** SEED-064#process-pr-577
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"1df59fae2397803e50e4c16d5d4840bd0478f770c2d7b429c1ddd9a54980efea"}}
```

**Goal:** Maintainers disposing
[PR #577](https://github.com/terryyin/lizard/pull/577), and users measuring
grouped expressions after `=` that are not arrow functions, receive one
complete disposition: `-Ecognitive` keeps the ternary or other structures
inside those parentheses on the surrounding function when the reader abandons
the arrow candidate, when the contribution remains valid on current trunk
(and after #576 when that is the intended base), or the PR stays unmerged with
an evidence-backed reason.

**Source.** Dashboard instruction `lizard process pr` for PRs created or
updated in the past 24 hours. Builds on #576; draft at intake. Full
process-PR scope applies; do not merge while draft unless converted after
qualification, and prefer landing #576 first when history depends on it.

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
users of `-Ecognitive` whose parenthesized assignment expressions lose score.

**Scope.** This pull request only, including the #576 commits it stacks.
Reproduce the abandoned-arrow case and controls that must keep holding
(`=>`, non-empty braces). Correct on a reachable contributor branch when
needed. Qualify and merge only when gates hold, including draft readiness.

Deferred: the separate ternary-comma bug named from #576. Rejection
constraints as for other process-PR owners.

**Key examples.**

- Given `const key = (prefix ? prefix + name : name);` inside a function,
  cognitive complexity is 1 (trunk / #576-alone reports 0).
- Given `fn = (v => v && ready)` or `x = ({ a: b ? 1 : 2 })`, scores stay as
  before this PR's new commits.
- Already-correct trunk, draft unreadiness, or an unworthy change receives a
  recorded reason instead of a merge.

**Direction:** Within "fix or improve existing functionality"; no external
signature change.

**Depends on:** Prefer [Process PR #576](SEED-062-process-pr-576.md#process-pr-576)
landed or otherwise accounted for when stacking history.

Intake metadata: head `0e6ec9f2ebd2930db35d7055f033386835ab29e8` on
`AbdelrahmanHafez:fix/cognitive-js-abandoned-arrow`; base `master` at
`e8d18d2e`; draft; `maintainer_can_modify` true; MERGEABLE; evidence under
`.planning/evidence/pr-577/`.

**Disposition (2026-11-20):** Review completed; **not merged**. Correctness accepted for post-#576 commits; still draft; CI green on `0e6ec9f2`. Sequencing depends on #576. Review comment: https://github.com/terryyin/lizard/pull/577#issuecomment-7030459177.
