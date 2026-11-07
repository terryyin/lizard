---
id: SEED-049
status: active
planted: 2026-11-07
planted_during: lizard process pr
scope: PHP trait multi-function boundaries from PR #448
---

# SEED-049: Process PR #448

<a id="process-pr-448"></a>

### Process PR #448 — Fix PHP trait multi-function body detection

**Identity:** SEED-049#process-pr-448
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"54417ba3920564ccfa4e19acd15e566105e3bfa98001e879d58d8f2f5aed458c"}}
```

**Goal.** Maintainers disposing
[PR #448](https://github.com/terryyin/lizard/pull/448), and PHP users analyzing
traits with more than one method, receive one complete disposition: each trait
method is reported as its own function with correct boundaries when the fix
remains valid on current trunk, or the PR stays unmerged with an evidence-backed
reason and any required follow-up linked.

**Source.** Dashboard instruction `lizard process pr`
https://github.com/terryyin/lizard/pull/448. Full process-PR scope: review,
reproduce against current trunk, correct on the contributor branch when that
route exists, qualify, and merge when the skill's gates hold. Resolves
[issue #441](https://github.com/terryyin/lizard/issues/441).

**Beneficiary.** Maintainers deciding whether to accept this contribution, and
PHP users whose trait methods currently collapse into a single spanning function.

**Scope.** This pull request only. Compare the claimed trait brace-level end
condition (`in_trait` alongside `in_class`) on current trunk through
`lizard_languages/php.py` and PHP tests. Correct remaining defects on a
reachable contributor branch, preserving contributor history, including
conflict resolution against current master when needed. Qualify the final
candidate with the full suite, style checks, and applicable performance
screening. Merge only when those gates and required hosted checks hold;
otherwise record the precise unmet gate.

Deferred promises: unrelated PHP features, other PRs, and language-reader
refactors beyond the trait body-end condition. Rejection constraints: do not
merge an inadequate, obsolete, or conflicted candidate to finish the cycle; do
not weaken discriminating PHP trait/class expectations to disguise a failure.

**Key examples.** Claims to verify on current trunk and on the proposal.

- Given a PHP trait with two or more methods, each method is a separate
  function with its own start/end rather than one function spanning the trait.
- Given an equivalent multi-method class, boundaries stay correct (control).
- Given free functions outside traits/classes, reporting is unchanged.
- Already-correct trunk, an unworthy change, or a correction that cannot be
  published on the contributor branch receives a recorded reason instead of a
  merge.

Intake metadata: head `7a788b2d1a4e57b6a532ef3b31011c2daf1adcd1` on
`Ollec:#441_trait_with_multiple_functions`; base
`dd1f9fc51733ded210024a9760d6821997ec97d5`; `maintainer_can_modify` true;
merge state CONFLICTING/DIRTY; evidence under `.planning/evidence/pr-448/`.

**Outcome.** Already fixed on trunk — not merged. Current `master` reports
separate `A::foo` / `A::bar` for the issue #441 trait reproduction (and the
class control). The production fix landed as
`69b29cdf9a241baadd129cc0990ac9efef24c783` for
[#498](https://github.com/terryyin/lizard/issues/498) via `function_brace_level`
in `php_states.py`, with regression
`test_each_trait_method_ends_at_its_own_brace`. PR head conflicts with the
rewritten reader, so the PR was closed rather than merged. Acknowledgement:
https://github.com/terryyin/lizard/pull/448#issuecomment-6702369846.
