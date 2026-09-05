---
name: story-refinement
description: >-
  Clarify selected stories before slice planning: goal, scope, and key examples,
  with UI and architectural concerns only when needed. Expand each story in its
  home seed. Use story-decomposition for broad problems or candidate selection;
  use slice-plan-refinement for execution-leaf sizing.
---

# Story Refinement

Build shared understanding of one selected story, or several related stories
whose boundaries need discussion. Follow `.cursor/rules/planning.mdc` for scope
discipline and lifecycle. Resolve repository paths from the checkout containing
this skill.

## Refine through conversation

Read the selected stories and relevant prior discussion. Reuse answers already
given; ask only questions that change understanding, with a concise proposed
answer. Do not turn the following into a questionnaire or mandatory approval
ceremony. Mark unresolved decisions explicitly; do not present proposals as
developer decisions.

For each story, establish:

- **Goal:** beneficiary, desired change, and its contribution to the business
  goal. Keep this story's observable outcome distinct from the broader ambition.
- **Scope:** included behavior, relevant exclusions, and boundary assumptions.
  Prefer the smallest useful outcome; exclude uncertain additions and report
  them. Clarify when exclusion would prevent the stated outcome from working.
- **Key examples:** concrete pre-condition → trigger → result situations that
  explain the scope. Include boundaries or exceptions when they resolve
  ambiguity; do not enumerate a complete test suite.

Add **UI** descriptions or sketches only when interaction or presentation needs
agreement. Add **Architecture** only for a new consequential concern; consult
`adr-awareness` and relevant Accepted ADRs. Inspect existing behavior or code
only to resolve a concrete question, without turning refinement into technical
planning. Omit unused optional sections.

A story may cross features. When refining several stories, keep each outcome
and boundary separate; do not merge them into one delivery by implication.
Use story-decomposition if the parent problem or candidate selection needs
reconsideration.

## Keep the understanding in the story's home

Expand each existing story section in its home seed with **Goal**, **Scope**,
and **Key examples**, plus optional details above. Replace overlapping detail;
preserve story anchors, sibling stories, and seed metadata. Record only open
questions that affect this story. If no home exists, establish one under
`.planning/seeds/` using the story-decomposition seed format.

Keep one home even for cross-feature journeys; link related seeds. The product
backlog remains ordered story links. Do not create a separate refinement file.
This is current understanding, revisable with the developer as evidence changes.

Report the story links, material exclusions, and unresolved decisions. When
slice planning is requested, hand off one selected story and continue without
repeating answered questions. Refinement alone does not authorize planning or
implementation. After implementation, reduce refinement detail to goal and
scope as specified in `planning.mdc`.
