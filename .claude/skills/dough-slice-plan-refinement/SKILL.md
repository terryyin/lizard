---
name: dough-slice-plan-refinement
description: >-
  Refines an existing executable plan in place into smaller, proof-owned
  Behavior/Structure slices. Use after dough-slice-planning when slices are
  complex, sizing confidence is low, or execution overruns. Creates no new plan
  and does not change the selected story outcome.
---

# Slice-plan refinement

Refine an existing executable plan in place. Do not create another plan,
implement product code, or change the selected story outcome.

## Require a refinable plan

Require an existing executable plan and this project's context required by
[dough-slice-planning](../dough-slice-planning/SKILL.md), especially its target,
hard limit, exceptions, overrun policy, and plan lifecycle.

- If no plan exists, use `dough-slice-planning`.
- If a plan is marked as awaiting story refinement after resplitting, use
  [dough-story-refinement](../dough-story-refinement/SKILL.md) on its mapped
  story and realign the plan before treating it as refinable or executable.
- If the selected story's goal, scope, or examples must change, use
  [dough-story-refinement](../dough-story-refinement/SKILL.md).
- If the parent problem, candidate selection, or sibling ordering must change,
  use [dough-story-decomposition](../dough-story-decomposition/SKILL.md).
- If all remaining slices are already cohesive, single-proof-loop,
  target-sized, and free of unexplained hard-limit paths, report
  `ready for direct execution`; no further refinement is required. Execution
  still requires separate authorization from the invoking workflow.

## Refine the plan

Read the plan and only the code and tests needed to judge execution boundaries.
Read and apply:

- [slice decomposition](../dough-story-decomposition/references/problem-decomposition.md#decompose-slices),
  including its sizing and escalation rules; and
- [active-plan refinement](../dough-story-refinement/references/planning.md#refine-the-active-plan),
  including executable proof ownership.

Preserve completed slices, applicable evidence, and the selected story's goal
and scope.

Classify each remaining slice:

| Result | Decision |
| --- | --- |
| **Ready** | One Behavior/Structure gate, one proof loop, cohesive path, and a plausible target-sized hypothesis |
| **Refine** | Same story, but the slice has multiple beats, low confidence, or a target or hard-limit concern |
| **Escalate** | Learning requires selected-story or parent-story review |

Route Escalate through the input gate. Refine every Refine slice.

After an overrun, confirm attempt-owned work is safely parked or reverted before
editing the plan. Stop for human judgment when ownership is unclear. Do not
commit, push, implement, or verify product behavior unless the invoking workflow
separately authorizes it.

Count the resulting plan's slices, including completed slices but excluding
obsolete replaced slices. If the count is greater than 15, report
`story resplit recommended: <count> slices; use dough-resplit-story` and link to
[dough-resplit-story](../dough-resplit-story/SKILL.md). This is a recommendation,
not an automatic invocation or a new execution-readiness gate. Do not resplit
the story or change its backlog placement during slice-plan refinement.

Report the plan path, replaced slices, resulting slice count, sizing exceptions,
any resplit recommendation, and whether execution can resume. End with:

`## SLICE PLAN REFINED`
