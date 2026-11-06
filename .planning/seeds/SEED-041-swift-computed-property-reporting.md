---
id: SEED-041
status: active
planted: 2026-11-06
planted_during: Maintainer request to queue Swift computed-property reporting for a later decision
trigger_when: A maintainer can judge whether this Swift reporting is worth its cost
scope: story
---

# SEED-041: Swift computed-property reporting

## Stories

<a id="swift-computed-property-reporting"></a>

### Judge Swift computed-property reporting

**Identity:** SEED-041#swift-computed-property-reporting
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"unselected"}
```

**Goal.** Swift users and maintainers get a recorded decision about computed
properties with an implicit getter and properties initialized by a closure.
Those properties become functions only when the reporting is worth more than
its cost and its performance is good enough. Until both are shown, the reader
stays as it is and
[PR #556](https://github.com/terryyin/lizard/pull/556) stays unmerged.

**Beneficiary.** People who analyze Swift types that hold logic in computed
properties or closure-initialized properties, and maintainers who would carry
the cost of a slower reader.

**Source.** PR #556, head `0f323dbfb22ad4bc999563bfb1e84480148cb2ac` at review
on 2026-11-06. The process execution SEED-038#process-pr-556 is abandoned.
Its unpublished candidate `8c0f3b7202eb1e66c3bf06c5045df058d5af542e` is not
product code and is not a commitment to accept the pull request.

**Scope.** Decide whether this reporting belongs in Lizard. Change the Swift
reader only after both conditions below hold. PR #556 is a candidate and a
source of examples, not an acceptance.

Required behavior:

- On current trunk, confirm the examples below before any reader change.
- Performance has to be good enough for the affected Swift analysis and for
  broader CLI analysis. A timing result alone does not accept the change.
  The 2026-11-06 paired runs, kept under host load of about 12–28 on 16
  cores, did not show that: mixed-corpus Swift `api-parameters` upper bounds
  were 11.4% and 23.8%, and the contributor patch alone was 13.3%. The broad
  Python `cli` run did pass, with an upper bound of 2.79%. Revisit those
  measurements on a quieter host before relying on them.
- The worth of the reporting has to be greater than its cost. A maintainer
  makes that judgment. Clearing a performance number does not make the change
  worth shipping.
- Land a reader change only when both conditions hold, preserving contributor
  history if the pull request is still the vehicle. Otherwise record the
  judgment and leave the pull request unmerged.
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
  and the performance, analysis reports `score` and `handler`, and those
  branches are not left only in the global scope.
- Given a performance result that is not good enough, or a judgment that the
  reporting is not worth its cost, the reader on trunk stays unchanged and
  PR #556 stays open.
