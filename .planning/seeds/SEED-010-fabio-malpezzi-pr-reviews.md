---
id: SEED-010
status: active
planted: 2026-10-27
planted_during: Maintainer request to review, correct and merge FabioMalpezzi's open PRs
trigger_when: A maintainer processes the open contributions from FabioMalpezzi
scope: large
---

# SEED-010: FabioMalpezzi PR reviews

<a id="review-fabio-malpezzi-prs"></a>

### Review, correct and merge FabioMalpezzi's open PRs

**Identity:** SEED-010#review-fabio-malpezzi-prs
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/004-fabio-malpezzi-pr-reviews/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"0274d82cd71a46f37b151b5fbb3d926166f0fb2956c47d3facf8211c4686f71c","plan":"7d222a18cebb37c32428f8f2426b25d9239853013e74acfabc09a6f38f559670"}}
```

**Goal:** Lizard users receive worthwhile fixes with correct metrics and
acceptable performance, and maintainers can account for the disposition of
every open PR from FabioMalpezzi.

**Scope:** Review every open PR in the
[author's PR queue](https://github.com/terryyin/lizard/pulls?q=is%3Apr+state%3Aopen+author%3AFabioMalpezzi).
Refresh the queue when execution starts and record that review set. The
snapshot below was fetched on 2026-10-27; its problem descriptions and
dependencies remain claims to verify during review.

For each PR:

- Reproduce the reported problem on the current target branch through an
  observable analyzer or CLI result. Establish the expected result, whether
  the problem remains real, and whether fixing it benefits users enough to
  justify the change. Record evidence and a disposition when a report is
  invalid, already fixed, duplicated, or not worth pursuing.
- Review whether the proposed fix solves the actual problem completely.
  Check code correctness, regressions, tests, readability, duplication,
  complexity and other code smells. Examine the overall affected design and
  shared behavior across languages, rather than judging only added lines.
  Verify PR dependencies and interactions against earlier merges.
- Measure runtime before and after the change using the same environment,
  input corpus and options, with repeated runs on representative affected
  inputs and a broader analysis workload. Record revisions, commands,
  corpus, timings, variability and percentage change. Profile when needed to
  explain a slowdown or locate a correction. A slowdown **strictly below
  3%** is acceptable; a result at or above 3%, or noise that prevents judging
  that threshold, does not satisfy the merge gate. Also check the cumulative
  effect of the accepted set against the starting baseline.
- Correct defects and refactor the overall affected changes into a coherent,
  minimal solution. Cover observable behavior and relevant edge cases with
  regression tests, following the project's language-reader and architecture
  guidance when applicable. Verify the resulting changes together with
  earlier accepted PRs.
- If the problem is real but the necessary correction is estimated to take
  **more than 15 minutes**, add a separate product backlog story with the
  beneficiary, reproducer, expected result, remaining work, and source PR
  link. Link that story from this review's disposition and leave the PR
  unmerged pending a satisfactory fix. The limit applies to correction work,
  not to the time needed for review or measurement; record a revised estimate
  if the correction proves larger during work. Reuse an existing story if it
  already covers the same problem.
- Merge a PR only when it is worthwhile and its final corrected revision
  passes correctness, code quality and performance review, the full
  `nix develop -c python -m pytest` suite, applicable format checks and
  required CI. Recheck the final revision after any correction or upstream
  change. After a successful merge, post a thank-you message to FabioMalpezzi
  on that PR, acknowledging the contribution and any material correction.

**Review boundary:** The execution-start queue snapshot defines the review
set for this delivery; record each PR number and head revision then. Later
PRs are outside this delivery's commitment. Refresh a selected PR's state
and revision before acting on it, and retain a disposition if it closes or
is superseded during the review. A dependency claim does not establish that
the dependency is needed or correct: verify it against the current target.
An earlier merge invalidates evidence where it changes the behavior or
measurement baseline of a remaining candidate.

**Deferred promises:** Corrections estimated above fifteen minutes belong
to the linked follow-up stories, not to this review's implementation scope.
This story does not commit to unrelated parser redesign, other authors'
queues, or a permanent benchmark service. The performance evidence must
still be reproducible and retained with each disposition.

**Key examples:**

1. A reproducible defect has a sound fix, needs ten minutes of cleanup, and
   shows a measured 2% slowdown with sufficient measurement confidence →
   correct and refactor it, verify the final revision, merge, and thank the
   contributor.
2. A proposed fix passes its tests but slows an affected workload by 3% →
   profile and correct it before merging; passing tests alone is insufficient.
3. A real defect needs thirty minutes of correction → queue a linked story
   and record the deferral instead of merging the inadequate fix.
4. The current target branch already produces the expected result → record
   that evidence and why the proposed change is unnecessary.
5. Two PRs pass separately but their combined changes miscount a function or
   exceed the performance limit → resolve the interaction and revalidate, or
   queue the correction under the same fifteen-minute rule.
6. A correction is estimated at exactly fifteen minutes → it remains eligible
   for this review; if new evidence raises the estimate above fifteen minutes,
   retain the reproducer and queue the remaining correction. The threshold
   does not waive any correctness or performance gate.
7. Repeated timings cannot distinguish a slowdown below 3% from one at or
   above 3% → retain the inconclusive evidence and withhold the merge until
   the performance gate can be judged; do not label the candidate passed.

**Preparation decision:** Terry confirmed ADR-0001 as Accepted on 2026-10-27;
its in-file status is aligned with the index. Its metric meanings and naming
guidance govern the reviews.

**Initial PR set (19 open PRs):**

| PR | Reported problem |
| --- | --- |
| [#527](https://github.com/terryyin/lizard/pull/527) | Python nested structures counted without indentation boundaries |
| [#528](https://github.com/terryyin/lizard/pull/528) | JavaScript/TypeScript regex literals after arrows and keywords |
| [#531](https://github.com/terryyin/lizard/pull/531) | Objects in expression bodies of JavaScript arrow functions |
| [#532](https://github.com/terryyin/lizard/pull/532) | Parameters named with `_`, `$` or a TypeScript type keyword |
| [#533](https://github.com/terryyin/lizard/pull/533) | End lines of JavaScript arrow functions without braces |
| [#534](https://github.com/terryyin/lizard/pull/534) | Arrow function boundaries at the colon of an enclosing ternary |
| [#535](https://github.com/terryyin/lizard/pull/535) | TypeScript `this` declarations counted as parameters |
| [#536](https://github.com/terryyin/lizard/pull/536) | JavaScript/TypeScript arrow function parameter counts |
| [#537](https://github.com/terryyin/lizard/pull/537) | Arrow function parameter counts after member access |
| [#538](https://github.com/terryyin/lizard/pull/538) | Function boundaries after immediately invoked functions |
| [#539](https://github.com/terryyin/lizard/pull/539) | Lost conditions in parenthesized expressions after assignment |
| [#540](https://github.com/terryyin/lizard/pull/540) | Case-label colons mistaken for type annotations |
| [#541](https://github.com/terryyin/lizard/pull/541) | Members named `get` or `set` mistaken for accessor prefixes |
| [#542](https://github.com/terryyin/lizard/pull/542) | Array property values mistaken for computed names |
| [#543](https://github.com/terryyin/lizard/pull/543) | Object spreads mistaken for methods |
| [#544](https://github.com/terryyin/lizard/pull/544) | JavaScript line comments continued after a backslash |
| [#545](https://github.com/terryyin/lizard/pull/545) | Missing conditions in template interpolations |
| [#546](https://github.com/terryyin/lizard/pull/546) | Keyword member names counted as conditions |
| [#547](https://github.com/terryyin/lizard/pull/547) | Ternaries without spaces and optional parameter markers |

**Completion:** Every PR in the refreshed review set has a recorded,
evidence-backed disposition: merged with a thank-you message, unnecessary
with a reason, or deferred to a linked backlog story for a real problem.
Merged revisions have correctness, quality, test and performance evidence;
the accepted set has also been checked for combined regressions. PR reviews,
corrections, measurements, messages and merges occur when this queued story
is executed.
