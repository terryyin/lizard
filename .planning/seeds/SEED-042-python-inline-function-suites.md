---
id: SEED-042
status: active
planted: 2026-11-06
planted_during: Maintainer request to queue Python inline function suites without accepting PR 530
trigger_when: A maintainer can judge whether reporting Python inline suites is worth a reader change whose slowdown stays within 3%
scope: story
---

# SEED-042: Python inline function suites

## Stories

<a id="python-inline-function-suites"></a>

### Judge Python inline function suites

**Identity:** SEED-042#python-inline-function-suites
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/027-python-inline-function-suites/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"f949f0bbd40140cbe929e4587404c0ba8009abee764660fcc405ab2f5327f69a","plan":"7780453013a91edc5df2ca6eb4494aae568f7b8e1f8c10e50d1470dcbbedaa8e"}}
```

**Goal.** Python users and maintainers get a recorded decision about functions
whose suite is on the `def` line. The reader changes only when those functions
are shown to be missing and the slowdown stays within 3%. Until both are
shown, trunk stays as it is and
[PR #530](https://github.com/terryyin/lizard/pull/530) stays unmerged.

**Beneficiary.** People who analyze Python that uses one-line function suites,
and maintainers who would carry a slower Python reader.

**Source.** PR #530, head `2948b38ed0bf70c5a4c39163b45dfa50c3020314` at review
on 2026-11-06. The process execution SEED-040#process-pr-530 is dropped.
Its uncommitted reader experiment is not product code and is not a commitment
to accept the pull request. That head alone still reports
`def value(): """A value."""; return 1` with NLOC 0; a disposition that
lands must correct that as well.

**Scope.** Decide whether inline Python suites belong in Lizard. Change the
Python reader only after both conditions below hold. PR #530 is a candidate
and a source of examples, not an acceptance. Function discovery and suite
boundaries stay in the existing Python reader (`python.py` /
`python_structure.py`); do not add a second parser.

Required behavior:

- On current trunk, confirm the examples below before any reader change.
  The 2026-11-06 check still found no functions for the `Stream` methods.
- A reader change lands only when its paired 95% upper slowdown bound is
  strictly below 3% on the affected API, on public parameter names and
  counts, and on broader CLI analysis, each compared with current trunk.
  A result at 3% or above does not land. The 2026-11-06 attempt did not
  meet that bound: six alternating passes of a correct candidate were about
  9.7% slower on 281 Python files, and a paired API interval was +2.2% to
  +13.6%. One bracket-depth lookup per token, alone, was about +3.1%.
- The missing functions have to be real on the examples below. A performance
  pass does not by itself accept a reader change, and a behavior pass does
  not by itself accept a slower reader.
- Land a reader change only when both conditions hold, preserving contributor
  history if the pull request is still the vehicle. Otherwise record the
  judgment on the pull request and leave it unmerged.
- If a change is accepted, a shared-line docstring still counts the `def`
  line: `def value(): """A value."""; return 1` reports `value` with NLOC 1.
  A block function keeps the metrics it already has. The multiline f-string
  `end_line` limit stays out of this story.

Deferred promises: the multiline f-string `end_line` limit; closed PRs #526
and #529; other languages. Rejection constraints: do not merge PR #530 to
close a review; do not treat the dropped experiment as accepted code; do not
add a second Python parser.

**Key examples.**

- Given `class Stream` with `def read(self) -> str: ...` and
  `def close(self) -> None: ...` on current trunk, analysis lists no
  functions. A block function such as `def kept(x):` with an `if` still
  reports `kept` with CCN 2. Given only
  `def outer():` / `def inner(): return 1`, analysis lists `outer.inner` and
  drops `outer`.
- Given those `Stream` methods, when both the missing functions and a
  slowdown within 3% are shown, analysis reports `read` and `close`, a
  nested inline keeps both the outer and inner functions, and
  `def value(): """A value."""; return 1` reports `value` with NLOC 1.
- Given a performance result whose upper bound is not strictly below 3%,
  the reader on trunk stays unchanged and PR #530 stays open.
