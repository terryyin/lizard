---
id: SEED-034
status: active
planted: 2026-11-04
planted_during: Maintainer request to process all open PRs from FabioMalpezzi
scope: FabioMalpezzi's open Lizard contributions
---

# SEED-034 — FabioMalpezzi's open PRs

<a id="process-fabio-malpezzi-prs"></a>

### Process FabioMalpezzi's open PRs

**Identity:** SEED-034#process-fabio-malpezzi-prs
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/022-fabio-malpezzi-open-prs/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"f91d37d2310d948a80a6a76c32fb2a0bfd436cbe3b656fbb3a99dedece5a288e","plan":"644a060c8b253d0f5eabfcdc8deea5dfdbbddd9b9f39920b4f6ce0f8950f3be7"}}
```

**Goal.** For JavaScript and TypeScript analyzer users who need accurate
function detection, CCN and nesting metrics at acceptable analysis speed, and
for maintainers disposing FabioMalpezzi's open contributions, each PR in the
execution-start frozen set receives one complete, independently useful
disposition: a worthwhile change verified and landed with acknowledgement, or
an evidence-backed unnecessary / inadequate conclusion with any required
follow-up linked and left unmerged.

**Scope.** Process every PR by FabioMalpezzi that is open against
`terryyin/lizard` when execution starts. At that moment refresh the
[author's open PRs][queue], freeze that review set and the starting trunk
revision, and keep later arrivals for separate intake. At refinement
(2026-11-04) the open set is #551–#554; those numbers are the expected starting
queue, not a frozen contract until execution starts.

Every frozen PR owns the full [Lizard PR workflow][workflow] with the same
correction and performance limits as the [previous review batch][previous]:

- Reproduce the claimed behavior on the current target through the public
  analyzer or CLI; check nearby controls and shared-language consumers; record
  when trunk already produces the expected result.
- Correct and independently refactor worthwhile changes only when the complete
  correction stays within the 15-minute budget (review and measurement time
  excluded). Above that budget, leave the PR open and link a backlog story with
  the reproducer, expected result and remaining work.
- Qualify the final revision with discriminating regressions, the full suite,
  style checks and required CI. For affected runtime workloads, require a paired
  95% upper slowdown bound strictly below 3% on the affected API and broader CLI
  workloads, both against the current target and cumulatively against the frozen
  baseline. Resolve interactions between PRs before accepting them.
- Merge qualified PRs guarded by exact head, thank the contributor when
  authorized during execution, and record the final revision and disposition for
  each PR, including the specific gate for any unfinished decision.

Deferred promises: PRs that arrive after the freeze; other authors; unrelated
language readers. Rejection constraints: do not invent an S/M/L sizing policy;
do not merge an inadequate candidate to finish a cycle; do not treat queue order
as a supplier dependency without a justified agreement; do not defer correction,
performance, refactor, CI, merge or acknowledgement of a frozen PR to another
story.

Assumptions: the previous-batch complete-cycle contract remains the product
policy for this cohort; sibling order is impact priority, not a technical
prerequisite.

**Key examples.** Claims to verify on the current target through the analyzer or
CLI; not automatic acceptance. Already-correct trunk, unworthy changes, or
corrections estimated above 15 minutes receive a recorded reason or linked
follow-up instead of a merge.

- Given a class field written without `;` followed by a member that starts with
  `async`, `static`, `get` or `set` (PR [#551](https://github.com/terryyin/lizard/pull/551)
  claim), public analysis reports those methods with correct boundaries in both
  JavaScript and TypeScript, while a multiline field value such as
  `handler =` followed by `async () => {}` remains one field.
- Given a TypeScript function declaration with type parameters such as
  `export function first<T>(list: T[]) { ... }` (PR
  [#552](https://github.com/terryyin/lizard/pull/552) claim), analysis reports
  the function with its correct end line and parameters; generic methods and
  generic arrows already reported stay intact.
- Given a quoted TypeScript optional member such as
  `{ 'retry-after'?: string }` or `{ "a-b"?: number }` (PR
  [#553](https://github.com/terryyin/lizard/pull/553) claim), CCN does not count
  that `?` as a decision, while a genuine ternary between strings such as
  `c ? 'a' : 'b'` still adds one.
- Given a promise chain that calls `.catch(...)` or `?.catch(...)` under `-ENS`
  (PR [#554](https://github.com/terryyin/lizard/pull/554) claim), the nested
  structure metric does not treat those member names as control structures,
  while a real `if` or `try`/`catch` statement still raises NS.

[queue]: https://github.com/terryyin/lizard/pulls?q=is%3Apr+is%3Aopen+author%3AFabioMalpezzi
[workflow]: ../../.agents/skills/lizard-process-pr/SKILL.md
[previous]: ../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md
