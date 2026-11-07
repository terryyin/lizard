---
id: SEED-043
status: active
planted: 2026-11-07
planted_during: Lizard Process PR https://github.com/terryyin/lizard/pull/558
scope: Swift enum case declarations as decisions
---

# SEED-043: Process PR #558

<a id="process-pr-558"></a>

### Process PR #558 — Do not count Swift enum case declarations as decisions

**Identity:** SEED-043#process-pr-558
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"16525dd810fdd363c0fc8d690e9015349db1dc32f17955f06b8c4b9bfcdd0673"}}
```

**Goal.** Swift users whose types declare enum cases, and maintainers disposing
[PR #558](https://github.com/terryyin/lizard/pull/558), receive one complete
disposition: the change is landed when `case` inside an enum body does not add
decisions while `case` in a `switch` still does (including switches in enum
methods and files that mix enums with top-level switches), or the PR stays
unmerged with an evidence-backed reason and any required follow-up linked.

**Source.** `/lizard-process-pr https://github.com/terryyin/lizard/pull/558`.
Full process-PR scope: review, reproduce, correct on the contributor branch
when that route exists, qualify, and merge when the skill's gates hold. This
invocation does not select planless execution or a separate landing of
lifecycle records.

**Beneficiary.** Swift users who declare enums (including nested, generic,
`indirect`, and local enums) and use `-Eoutside` or function-local enum
scopes, and maintainers deciding whether to accept this contribution.

**Scope.** This pull request only. Reproduce the claimed behavior on current
trunk through the public analyzer or CLI, compare it with the proposal, and
check nearby Swift controls and shared consumers of the decision count. Correct
a worthwhile change only on a reachable contributor branch, preserving
contributor history. Qualify the final candidate with discriminating tests, the
full suite, style checks, and a performance screening of the token path this
change touches. Merge only when those gates and required hosted checks hold;
otherwise record the precise unmet gate.

Deferred promises: other Swift syntax, other languages, and pull requests that
are not #558. Rejection constraints: do not merge an inadequate candidate to
finish the cycle; do not apply the Fabio cohort's 15-minute deferral or its
strict 3% bound as this story's policy. The paired runner's default screening
result is evidence to record, including a consequential regression.

**Key examples.** Claims to verify on current trunk and on the proposal through
the analyzer or CLI. Already-correct trunk, an unworthy change, or a correction
that cannot be published on the contributor branch receives a recorded reason
instead of a merge.

- Given an enum in an extension such as
  `extension Foo { enum Kind { case a, b; case c } }` analyzed with
  `-Eoutside`, global CCN is 1 (enum cases are not decisions).
- Given an `indirect` generic enum whose cases are declarations only, global
  CCN with `-Eoutside` is 1.
- Given an enum and a top-level `switch` with two cases in the same file,
  global CCN with `-Eoutside` is 3 (the two switch cases count; enum cases do
  not).
- Given a function that declares a local enum and also contains a `switch`,
  the function CCN is not inflated by the enum cases; switch cases still count.
- Given a method with a `switch` inside an enum type, those switch cases still
  count as decisions.

Intake metadata: head `c727d5704acee2d3ff6219f4e1ac8617e50ce823` on
`cpheinrich:codex/swift-enum-case-declarations`; `maintainer_can_modify` true;
mergeable state at intake was `CONFLICTING`.

**Outcome.** Merged as `596ec144543d94a990bd42811736525782dc45f0` with
contributor head `77afb888134df499c654858b4edb905cf11aaa6a` (master merge
resolving `CHANGELOG.md`, plus blank lines before the enum-case test helper).
On trunk before the merge, an enum in an extension had global CCN 3 with
`-Eoutside` (expected 1); after the merge it is 1. An enum beside a two-case
top-level switch is global CCN 3; a local enum no longer inflates its
function; switch cases inside enum methods still count. Hosted checks on
`77afb8881` passed; `web` was skipped. Acknowledgement:
https://github.com/terryyin/lizard/pull/558#issuecomment-6699366461

Swift API on a dense enum/switch corpus, 20 pairs: paired change about
+1.6%, 95% interval about +1.0% to +2.2% (medians 1.21s and 1.24s). API with
parameters about +1.4% (95% about +0.7% to +2.0%). Broad CLI on
`lizard_languages` / `lizard_core` / `lizard_ext` Python sources about +0.5%
(95% about −0.2% to +1.2%). Evidence under `.planning/pr-558-evidence/`.
