---
id: SEED-044
status: active
planted: 2026-11-07
planted_during: lizard process pr
scope: Rust char literals and parameterless closure ||
---

# SEED-044: Process PR #563

<a id="process-pr-563"></a>

### Process PR #563 — Read Rust char literals as one token, and do not count the || of a closure without parameters

**Identity:** SEED-044#process-pr-563
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"ac6244752ae77e132815fcff76b0635d00203ca90253b4e59baf661a544ecdd5"}}
```

**Goal.** Rust users who rely on Lizard CCN limits in CI, and maintainers disposing
[PR #563](https://github.com/terryyin/lizard/pull/563), receive one complete
disposition: the change is landed when char literals no longer swallow following
functions and a parameterless-closure `||` is not counted as a logical or while
true logical-or and lifetime/label controls stay correct, or the PR stays
unmerged with an evidence-backed reason and any required follow-up linked.

**Source.** Dashboard ad-hoc instruction `lizard process pr` (no URL). Sibling
launches already own #558, #559, #560 and #562; this session takes the remaining
recent open PR [#563](https://github.com/terryyin/lizard/pull/563). Full
process-PR scope: review, reproduce, correct on the contributor branch when that
route exists, qualify, and merge when the skill's gates hold.

**Beneficiary.** Rust users whose code uses char literals and parameterless
closures, and maintainers deciding whether to accept this contribution.

**Scope.** This pull request only. Reproduce the claimed behavior on current
trunk through the public analyzer or CLI, compare it with the proposal, and
check nearby Rust controls and shared consumers of tokenizing and decision
counts. Correct a worthwhile change only on a reachable contributor branch,
preserving contributor history. Qualify the final candidate with discriminating
tests, the full suite, style checks, and a performance screening of the Rust
token path this change touches. Merge only when those gates and required hosted
checks hold; otherwise record the precise unmet gate.

Deferred promises: other Rust syntax, other languages, and pull requests that
are not #563. Rejection constraints: do not merge an inadequate candidate to
finish the cycle; do not apply the Fabio cohort's 15-minute deferral or its
strict 3% bound as this story's policy. The paired runner's default screening
result is evidence to record, including a consequential regression.

**Key examples.** Claims to verify on current trunk and on the proposal through
the analyzer or CLI. Already-correct trunk, an unworthy change, or a correction
that cannot be published on the contributor branch receives a recorded reason
instead of a merge.

- Given `fn is_call(code: char) -> bool { code == 'C' }` followed by later
  functions that use `if` / `||` / `match` on char literals, analysis reports
  those functions (for example `is_call` CCN 1, `either` 3, `label` 3) instead
  of swallowing the file after `'C'`.
- Given escaped char literals next to a lifetime and a loop label, and a
  function after them, lifetimes/labels remain tokens and the following
  function is still reported.
- Given `unwrap_or_else(|| 0)`, `move || x`, and similar parameterless closures
  in arrays or as arguments, those `||` forms do not add a decision.
- Given `||` after an identifier, call, index, parenthesis, or literal, the
  logical or is still counted.

Intake metadata: head `fc9c2bb201c68f915389f78b5e2d92ba3b103ef7` on
`nvp152:rust-char-literals-and-closures`; PR base
`6744374debb3f36e5872e7f1d503a42dce9b2ea5`; `maintainer_can_modify` true;
mergeable; evidence packet under `.planning/evidence/pr-563/`.

**Outcome.** Merged as `453b9cdc3748263d78febbc008d67615a39f7956` with
contributor head `fc9c2bb201c68f915389f78b5e2d92ba3b103ef7` (no local
correction). On trunk before the change, the char-literal example reported no
functions and `closures` had CCN 6; after merge it reports `is_call` /
`either` / `label` at CCN 1 / 3 / 3 and `closures` at 1, while logical-or
controls stay at CCN 5. Escaped chars with lifetimes/labels, byte literals,
raw strings with quotes, and `a || (|| true)()` keep expected boundaries.
Merge-candidate `pytest` / `make` were green (3050 passed, 7 skipped). Hosted
checks were absent on the fork head; master was not branch-protected via the
API. Acknowledgement:
https://github.com/terryyin/lizard/pull/563#issuecomment-6699430038

Paired screening versus `origin/master` `1bbc6fce` on a frozen anyhow+chrono
corpus (~2.3 MB, 134 files), 20 pairs, batches calibrated to 4 seconds, load
about 5.4 to 5.7: API paired change about +3.31%, 95% interval about +2.78%
to +3.85%; CLI about +3.31%, interval about +2.65% to +3.97%. Retained under
`.planning/evidence/pr-563/perf/`. This story records that screening and does
not treat the runner's default 3% bound as a merge gate. The contributor's
larger-corpus intervals under quieter conditions were under 3%.
