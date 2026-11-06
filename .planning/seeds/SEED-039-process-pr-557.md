---
id: SEED-039
status: active
planted: 2026-11-06
planted_during: Lizard Process PR https://github.com/terryyin/lizard/pull/557
scope: Swift condition-list commas as decisions
---

# SEED-039: Process PR #557

<a id="process-pr-557"></a>

### Process PR #557 — Count Swift condition-list commas as decisions

**Identity:** SEED-039#process-pr-557
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"unselected"}
```

**Goal.** Swift users who write short-circuiting condition lists, and maintainers
disposing [PR #557](https://github.com/terryyin/lizard/pull/557), receive one
complete disposition: the change is landed when top-level `if` / `guard` /
`while` commas add the same decisions as `&&` and nearby controls stay correct,
or the PR stays unmerged with an evidence-backed reason and any required
follow-up linked.

**Source.** `/lizard-process-pr https://github.com/terryyin/lizard/pull/557`.
Full process-PR scope: review, reproduce, correct on the contributor branch
when that route exists, qualify, and merge when the skill's gates hold. This
invocation does not select planless execution or a separate landing of
lifecycle records.

**Beneficiary.** Swift users whose conditions use comma lists (`guard a, b, c`,
`if a, b`, `while a, b`) and maintainers deciding whether to accept this
contribution.

**Scope.** This pull request only. Reproduce the claimed behavior on current
trunk through the public analyzer or CLI, compare it with the proposal, and
check nearby Swift controls and shared consumers of the decision count. Correct
a worthwhile change only on a reachable contributor branch, preserving
contributor history. Qualify the final candidate with discriminating tests, the
full suite, style checks, and a performance screening of the token path this
change touches. Merge only when those gates and required hosted checks hold;
otherwise record the precise unmet gate.

Deferred promises: other Swift syntax, other languages, and pull requests that
are not #557. Rejection constraints: do not merge an inadequate candidate to
finish the cycle; do not apply the Fabio cohort's 15-minute deferral or its
strict 3% bound as this story's policy. The paired runner's default screening
result is evidence to record, including a consequential regression.

**Key examples.** Claims to verify on current trunk and on the proposal through
the analyzer or CLI. Already-correct trunk, an unworthy change, or a correction
that cannot be published on the contributor branch receives a recorded reason
instead of a merge.

- Given `guard a, b, c else { return false }` and `guard a && b && c else { return false }`
  with the same surrounding function, analysis reports the same CCN.
- Given `if a, b { }` and `while a, b { }`, each top-level comma adds one
  decision, as `&&` does.
- Given commas inside calls, arrays, tuples, closures, patterns, and generic
  argument lists, those commas do not add decisions. A comparison such as
  `if a < b, c > d` still counts its condition-list comma. `repeat { } while`
  does not start a condition list.

Intake metadata: head `f47571842005d2a64d8b9846306f1c4029dada66` on
`cpheinrich:codex/swift-condition-list-commas`; PR base
`b6b6b8c671bab79da1a6c28c61a2076e4735998d`; `maintainer_can_modify` true.

**Outcome.** Merged as `861494afaee7130e819824919aea02c97e7183e3` with
contributor head `f47571842005d2a64d8b9846306f1c4029dada66` and correction
`6a774049d5ce43dd79a84eae6e6d9ec5c68766ea`. On current trunk, `guard a, b, c`
had CCN 2 while `guard a && b && c` had CCN 4; the merged analyzer reports 4
for both, and the same for `if` and `while` lists. Nested commas, `repeat`
/`while`, the following function, and nesting stay as before. A same-line
trailing closure keeps the following comma; a brace that continues on the next
line is the body. Hosted builds on `6a77404` passed; `web` was skipped.
Acknowledgement: https://github.com/terryyin/lizard/pull/557#issuecomment-6673165357

Swift API on a condition-dense corpus, 20 pairs, batches calibrated to 4
seconds, load about 7 to 4: paired change about +18%, 95% interval about +8%
to +29% (medians 3.27s and 3.60s, standard deviation about 2.3s). A quiet
profile of that corpus was 1.38s without the scanner and 1.64s with it; the
contributor's original scanner profiled at 1.62s. Broad CLI on the Python
language, core, extension, and language-test sources passed, interval about
-1.4% to +0.7%. Cognitive complexity still follows boolean operators, so the
comma form and the `&&` form can differ there.
