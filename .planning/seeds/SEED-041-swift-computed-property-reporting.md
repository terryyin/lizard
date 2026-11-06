---
id: SEED-041
status: active
planted: 2026-11-06
planted_during: Maintainer request to queue Swift computed-property reporting for a later decision
trigger_when: A maintainer can judge whether reporting Swift computed properties and closure-initialized properties is worth a reader change whose slowdown stays within 3%
scope: story
---

# SEED-041: Swift computed-property reporting

## Stories

<a id="swift-computed-property-reporting"></a>

### Judge Swift computed-property reporting

**Identity:** SEED-041#swift-computed-property-reporting
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/028-swift-computed-property-reporting/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"d12cf9f09131ac2971047d29c3b5eb5902b1b89d6f1268dec705de0ab4ae612a","plan":"f8af8e613ed74ae36c9e09b0fdcbabfb775b0b678cf098b4d3e24a0aef7b036a"}}
```

**Goal.** Swift users and maintainers get a recorded decision about computed
properties with an implicit getter and properties initialized by a closure.
Those properties become functions only when a maintainer accepts that the
reporting is worth its cost and every required paired 95% upper slowdown is
strictly below 3%. Until both hold, the reader stays as it is and
[PR #556](https://github.com/terryyin/lizard/pull/556) stays unmerged.

**Beneficiary.** People who analyze Swift types that hold logic in computed
properties or closure-initialized properties, and maintainers who would carry
the cost of a slower reader.

**Source.** PR #556, review head `0f323dbfb22ad4bc999563bfb1e84480148cb2ac`
on 2026-11-06; current open head `363cfb19adde9cb10ed0a1d0a40085c9df7cd574`
after merging master. The process execution SEED-038#process-pr-556 is
abandoned. Its unpublished candidate
`8c0f3b7202eb1e66c3bf06c5045df058d5af542e` is not product code and is not a
commitment to accept the pull request.

**Scope.** Decide whether this reporting belongs in Lizard. Change the Swift
reader only after both conditions below hold. PR #556 is a candidate and a
source of examples, not an acceptance. Function discovery stays in the
existing Swift reader (`swift.py` / `SwiftStates`); do not add a second
parser.

Required behavior:

- On current trunk, confirm the examples below before any reader change.
  The 2026-11-06 check still found no functions for `score` or `handler`,
  while an explicit `get` / `willSet` stayed reported.
- A reader change lands only when its paired 95% upper slowdown bound is
  strictly below 3% on the affected Swift API, on public parameter names and
  counts, and on broader CLI analysis, each compared with current trunk.
  A result at 3% or above does not land. The 2026-11-06 paired runs, kept
  under host load of about 12–28 on 16 cores, did not meet that bound:
  mixed-corpus Swift `api-parameters` upper bounds were 11.4% and 23.8%, and
  the contributor patch alone was 13.3%. The broad Python `cli` run did pass,
  with an upper bound of 2.79%. Revisit those measurements on a quieter host
  before relying on them.
- The worth of the reporting has to be greater than its cost. A maintainer
  makes that judgment. Clearing the &lt;3% bound does not by itself accept
  the change, and a worth judgment does not waive a failed bound.
- Land a reader change only when both conditions hold, preserving contributor
  history if the pull request is still the vehicle. Otherwise record the
  judgment on the pull request and leave it unmerged.
- If the change is accepted, a later binding in the same declaration
  (`let a = 1, handler = { ... }`) is the same kind of property. Commas inside
  calls and generics are not new functions. Accessor blocks, stored
  properties, protocol requirements, and locals keep today's behavior.

Deferred promises: other languages and other pull requests. Rejection
constraints: do not merge PR #556 to close a review; do not treat the
abandoned candidate as accepted code; do not add a second Swift reader beside
`SwiftStates`.

**Key examples.**

- Given `var score: Int { if flag { return 1 }; return 0 }` and
  `let handler: (Int) -> Int = { x in if x > 0 { return 1 }; return 0 }` on
  current trunk, analysis lists no function for either property, and those
  branches count in the global scope. An explicit `get` or `willSet` stays
  reported, and the global count around those accessors stays unchanged.
- Given those same properties, when a maintainer has accepted both the worth
  and a slowdown whose paired 95% upper bound is strictly below 3%, analysis
  reports `score` and `handler`, and those branches are not left only in the
  global scope.
- Given a performance result whose upper bound is not strictly below 3%, or a
  judgment that the reporting is not worth its cost, the reader on trunk stays
  unchanged and PR #556 stays open.
