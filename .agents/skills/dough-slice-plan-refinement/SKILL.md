---
name: dough-slice-plan-refinement
description: >-
  Refines an existing executable plan's slices in place when boundaries fragment
  cohesive work, combine independent concerns, accumulate special cases or
  overrun. Use during or after slice planning.
---

# Slice-plan refinement

Refine one executable plan in place. Keep its selected outcome and leave
implementation to execution. [Resplit story](../dough-resplit-story/SKILL.md)
owns any split between stories.

## Require a refinable plan

Require an existing executable plan and this project's context required by
[dough-slice-planning](../dough-slice-planning/SKILL.md), especially the plan
lifecycle.

Identify whether the plan's source is a selected feature story or a bounded
retrospective correction under
[planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level).
For a complete correction, use its correction input directly from its story
and plan, or from a plan-homed correction's plan alone; create no seed. Name
whichever required correction field is missing and stop. A complete plan permits
refinement, not execution; preserve the invoking instruction's authorization
boundary. Before revising boundaries, apply the shared
[reassessment decision](../dough-execute-plan/references/execution-decisions.md#reassess-before-extending-work).

- If no plan exists, use `dough-slice-planning`.
- If a plan is marked as awaiting story refinement after resplitting, use
  [dough-story-refinement](../dough-story-refinement/SKILL.md) on its mapped
  story and realign the plan before treating it as refinable or executable.
- If the selected story's goal, scope, or examples must change, use
  [dough-story-refinement](../dough-story-refinement/SKILL.md).
- If a correction's beneficiary, bounded outcome, scope, or proof must change,
  stop through the shared execution-decision handoff; do not turn it into a
  feature story. A disputed product constraint uses the plan-conflict handoff
  below and remains human-owned.
- If the parent problem, candidate selection, or sibling ordering must change,
  use [dough-story-decomposition](../dough-story-decomposition/SKILL.md).
- Treat the plan as `ready for direct execution` only when cumulative design is supported,
  every boundary preserves useful progress or learning, and all slices are
  cohesive, single-proof-loop, and fit the
  [slice's active time](../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices).
  Record that outcome through
  [assess readiness at preparation completion](../dough-product-backlog/references/record-preparation.md#assess-readiness-at-preparation-completion)
  when the work item has a recorded identity. Execution still requires separate
  authorization from the invoking workflow.

## Refine the plan

Before writing to the plan, establish or reuse the required workspace under
[preparation workspace](../dough-story-refinement/references/preparation-workspace.md),
then, for a queued story,
[announce the preparation assignment](../dough-story-refinement/references/preparation-assignment.md#announce-the-preparation-assignment);
reading the plan and the code and tests needed to judge execution boundaries
needs neither on its own. Read and apply:

- [slice decomposition](../dough-story-decomposition/references/problem-decomposition.md#decompose-slices),
  including its cumulative design assessment, sizing, and escalation rules; and
- [active-plan refinement](../dough-story-refinement/references/planning.md#refine-the-active-plan),
  including executable proof ownership.

When refinement reconsiders the plan's selected existing solution or the
architectural direction supporting it, reapply
[architectural thinking](../dough-slice-planning/references/architectural-thinking.md),
including its authoritative PFE handoff. Otherwise carry the decision forward;
do not repeat PFE or create a new direction topic merely because slices are
being refined.

Preserve completed slices, applicable evidence, and the selected story's goal
and scope or the correction's bounded outcome and scope.

Assess the cumulative design, including how remaining examples build on
completed slices, then classify each remaining boundary:

| Result | Decision |
| --- | --- |
| **Retain** | One gate and proof loop yield cohesive progress within the slice's active time |
| **Consolidate** | Adjacent slices are useful only together; combining hides no consequential risk and preserves safe recovery |
| **Split** | One slice has independent outcomes or proof loops, hidden preparation, credible risk, special-case design, or a sizing concern |
| **Escalate** | Learning requires review of the source outcome, story, or parent problem |

A suspected accidental contractual restriction uses the shared
[plan-conflict handoff](../dough-execute-plan/references/execution-decisions.md#resolve-a-disputed-plan-restriction)
before a conflicting plan edit; classify it as Escalate, not an automatically
correctable special case. Route other Escalate findings through the input gate.
Before applying a proposed change during execution, count the resulting slices.
Route an over-limit proposal and its evidence to the coordinator's
[stop or split decision](../dough-execute-plan/references/execution-decisions.md#stop-or-split-before-more-work)
with the active plan intact. Otherwise apply every Consolidate or Split decision.

After an overrun, confirm attempt-owned work is safely parked or reverted before
editing the plan. Stop for human judgment when ownership is unclear. Do not
commit, push, implement, or verify product behavior unless the invoking workflow
separately authorizes it.

In preparation, apply the [slice limit](../dough-story-decomposition/references/problem-decomposition.md#limit-slices-per-plan)
before assessment. A settled resplit supplies the replacement assessments.

For a plan that stays intact, record its readiness reassessment through
[assess readiness at preparation completion](../dough-product-backlog/references/record-preparation.md#assess-readiness-at-preparation-completion):
for a recorded identity, blockers become `not-ready` reasons. Record `ready`
when the design and slices satisfy the checks above. Readiness grants no Take
or execution authority.

Return the plan path, replaced slices, resulting slice count, sizing
exceptions, any resplit performed, whether execution can resume, and the
reassessment to an invoking workflow, such as slice planning or execution, that
continues from them. An Escalate finding or another
decision a person must make needs a response: report it with the expected
response, who can give it when known, and what waits for it.

When this session ends, follow
[land at the end of preparation](../dough-story-refinement/references/preparation-journey.md#land-at-the-end-of-preparation),
then close or retain the workspace under
[preparation workspace](../dough-story-refinement/references/preparation-workspace.md#close-or-retain-the-workspace).
A session that invoked this skill directly finishes under
[finish with only what needs a response](../dough-story-refinement/references/preparation-journey.md#finish-with-only-what-needs-a-response).
An execution instruction for this story that arrives while its preparation is
unlanded follows
[hand off to execution in the same session](../dough-story-refinement/references/preparation-journey.md#hand-off-to-execution-in-the-same-session)
instead.
