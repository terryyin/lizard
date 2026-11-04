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

**Goal.** Lizard users get accurate JavaScript and TypeScript metrics at
acceptable analysis speed, and maintainers have an evidence-backed outcome
for each contribution.

**Scope.** Process all PRs by FabioMalpezzi open against `terryyin/lizard` when
execution starts. Refresh the [author's open PRs][queue] at that point and freeze
the review set and starting trunk revision. Route later arrivals through a
separate intake. Follow the [Lizard PR workflow][workflow] and use the same
correction and performance limits as the [previous review batch][previous].

**Initial set as of 2026-11-04.** The expected results below are claims to
verify on the current target through the analyzer or CLI.

| PR | Expected behavior to verify |
| --- | --- |
| [#551](https://github.com/terryyin/lizard/pull/551) | Report JavaScript and TypeScript methods with modifiers after a class field that omits its semicolon. Preserve multiline field values such as an async arrow. |
| [#552](https://github.com/terryyin/lizard/pull/552) | Report TypeScript generic function declarations with their correct boundaries and parameters. |
| [#553](https://github.com/terryyin/lizard/pull/553) | Exclude quoted TypeScript optional member markers from CCN while still counting genuine ternaries. |
| [#554](https://github.com/terryyin/lizard/pull/554) | Exclude member calls such as `.catch()` and `?.catch()` from NS while still counting actual control structures. |

**Completion.** Record a disposition and supporting evidence for every PR in
the frozen set.

- Reproduce the reported behavior and check nearby controls and shared
  consumers. Record when trunk already produces the expected result.
- Correct and refactor worthwhile changes within the 15-minute correction
  budget. Review and measurement time are separate. If a real defect needs
  more correction time, leave the PR open and link a backlog story with the
  reproducer, expected result and remaining work.
- Qualify final revisions with regression coverage, the full suite, style
  checks and required CI. For affected runtime workloads, require a paired
  95% upper slowdown bound strictly below 3% on the affected API and broader
  CLI workloads, both against the current target and cumulatively against the
  frozen baseline. Resolve interactions between PRs before accepting them.
- Merge qualified PRs and thank the contributor when authorized during
  execution. Record the final revision and disposition for each PR, including
  the specific gate for any pending decision.

[queue]: https://github.com/terryyin/lizard/pulls?q=is%3Apr+is%3Aopen+author%3AFabioMalpezzi
[workflow]: ../../.agents/skills/lizard-process-pr/SKILL.md
[previous]: ../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md
