---
id: SEED-043
status: active
planted: 2026-11-07
planted_during: Lizard Process PR https://github.com/terryyin/lizard/pull/559
scope: Swift string-interpolation decisions
---

# SEED-043: Process PR #559

<a id="process-pr-559"></a>

### Process PR #559 — Count decisions inside Swift string interpolation

**Identity:** SEED-043#process-pr-559
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/029-process-pr-559/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"9cd11d7e2251ca3b70635f6474f242702665275c3ed0968fbc3703e57bafa4d0","plan":"561881d35e5f65689b6f39eef60c90e1bf4f8155ddaa4e0df07a3da7fef68ea8"}}
```

**Goal.** Swift users whose string literals embed decisions in `\(...)`, and
maintainers disposing [PR #559](https://github.com/terryyin/lizard/pull/559),
receive one complete disposition: the change is landed when interpolations
contribute the same decisions as equivalent code outside the literal, nearby
controls stay correct, and brackets that scanners track stay balanced, or the
PR stays unmerged with an evidence-backed reason and any required follow-up
linked.

**Source.** `/lizard-process-pr https://github.com/terryyin/lizard/pull/559`.
Full process-PR scope: review, reproduce, correct on the contributor branch
when that route exists, qualify, and merge when the skill's gates hold.
Executable plan: `.planning/slice-plans/029-process-pr-559/PLAN.md`.

**Beneficiary.** Swift users who put ternaries, boolean operators, or closures
inside string interpolations, and maintainers deciding whether to accept this
contribution.

**Scope.** This pull request only. Reproduce the claimed behavior on current
trunk through the public analyzer or CLI, compare it with the proposal, and
check nearby Swift controls and shared consumers of decision count, token
count, NLOC, and condition-list scanning after #557. Correct a worthwhile
change only on a reachable contributor branch, preserving contributor history.
Qualify the final candidate with discriminating tests, the full suite, style
checks, and a performance screening of the Swift token path this change
touches. Merge only when those gates and required hosted checks hold;
otherwise record the precise unmet gate. Resolve the current merge conflict
with trunk before any merge disposition.

Deferred promises: other Swift syntax, other languages, and pull requests that
are not #557. Rejection constraints: do not merge an inadequate candidate to
finish the cycle; do not apply the Fabio cohort's 15-minute deferral or its
strict 3% bound as this story's universal policy. The paired runner's default
screening result is evidence to record, including a consequential regression.
Do not force-push the contributor branch.

**Key examples.** Claims to verify on current trunk and on the proposal through
the analyzer or CLI. Already-correct trunk, an unworthy change, or a
correction that cannot be published on the contributor branch receives a
recorded reason instead of a merge.

- Given
  `func f(x: Int) -> String { return "v \(x > 0 ? "pos" : "neg") \(x == 1 || x == 2)" }`,
  analysis reports CCN 3 (two decisions inside interpolations), not CCN 1.
- Given nested interpolations and decisions inside them, those decisions are
  counted; escaped `\(` and raw strings that do not open interpolation stay
  text; `#\"\#(x)\"#` still tokenizes the interpolated expression.
- Given `\"\\(1 /* ) */ + (flag ? 2 : 3))\"`, parentheses inside line, block,
  and nested block comments do not close the interpolation, and the ternary
  still counts.
- Given a multiline interpolated literal, line numbers and NLOC stay
  consistent with the source, and tokens from one literal still join back to
  the original text.
- Given a condition list that embeds a trailing closure inside an
  interpolation, such as
  `if \"\\(xs.map { $0 > 1 ? 1 : 0 })\".isEmpty, ok { }`, the list keeps both
  conditions (the comma after the interpolated expression still counts), and
  the map/ternary decisions inside the interpolation are counted.

Intake metadata: head `e613bda0142f016b5d8628b120e16655dc78ea79` on
`cpheinrich:codex/swift-string-interpolation`; PR base
`4f6979da10faabc72f1564f9341588afbb2924f0`; workspace trunk at intake
`1bbc6fce34235b810f44493a54435f6e273c952f`; `maintainer_can_modify` true;
merge state `CONFLICTING` / `DIRTY` at intake. Refresh head, base, and checks
before any dependent action.

**Outcome.** Merged as `b4ee88d3b6efb3fbb8015242e038000d19d8c412` with
contributor head `0f6dfae9a0a181f304e7406f84deea36d7c0e212` (merge of master
into the PR, preserving contributor commits `462c4c75f` and `e613bda01`).
On trunk before the merge, the story interpolation example had CCN 1; after
the merge it reports CCN 3, nested interpolations CCN 3, and the condition-list
example CCN 7. Computed-property reporting from #556 stays intact. Hosted
builds on `0f6dfae9a` passed after a Windows website-smoke flake rerun; `web`
was skipped. Acknowledgement:
https://github.com/terryyin/lizard/pull/559#issuecomment-6699314477

Paired API on Alamofire 6c2892e (101 Swift files), 20 pairs, vs trunk
`1bbc6fce`: paired change about −0.26%, 95% interval about −1.30% to +0.78%
(medians ~1.65s). Full local pytest 3052 passed, 7 skipped; `make` clean.
