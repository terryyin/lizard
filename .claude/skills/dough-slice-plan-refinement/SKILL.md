---
name: dough-slice-plan-refinement
description: >-
  Refines an executable plan in place into proportionate, proof-owned
  Behavior/Structure slices. Use during or after dough-slice-planning when boundaries
  fragment cohesive work, combine independent concerns, accumulate special cases,
  or overrun. Creates no new plan and preserves the selected story or bounded
  correction outcome. After rewriting the plan, reassess readiness through the
  shared preparation procedure without granting Take or execution.
---

# Slice-plan refinement

Refine an existing executable plan in place. Do not create another plan,
implement product code, or change its selected story or bounded correction
outcome.

## Require a refinable plan

Require an existing executable plan and this project's context required by
[dough-slice-planning](../dough-slice-planning/SKILL.md), especially any supplied
target, hard limit, exceptions, overrun policy, and the plan lifecycle.

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
- Report `ready for direct execution` only when cumulative design is supported,
  every boundary preserves useful progress or learning, and all slices are
  cohesive, single-proof-loop, meet supplied targets, and have no unexplained
  path beyond a hard limit. Record that outcome through
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
| **Retain** | One gate and proof loop yield cohesive progress within supplied limits |
| **Consolidate** | Adjacent slices are useful only together; combining hides no consequential risk and preserves safe recovery |
| **Split** | One slice has independent outcomes or proof loops, hidden preparation, credible risk, special-case design, or a sizing concern |
| **Escalate** | Learning requires review of the source outcome, story, or parent problem |

A suspected accidental contractual restriction uses the shared
[plan-conflict handoff](../dough-execute-plan/references/execution-decisions.md#resolve-a-disputed-plan-restriction)
before a conflicting plan edit; classify it as Escalate, not an automatically
correctable special case. Route other Escalate findings through the input gate.
Apply every Consolidate or Split decision.

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
any resplit recommendation, whether execution can resume, and — when the work
item has a recorded identity — the readiness reassessment through
[assess readiness at preparation completion](../dough-product-backlog/references/record-preparation.md#assess-readiness-at-preparation-completion):
remaining Refine or Escalate findings, or any other blocking concern, become
`not-ready` reasons; when the cumulative design is supported and remaining
slices are ready under the table above, record `ready`. Do not Take, move the
queue, or start execution from this reassessment.

Apply
[preparation workspace](../dough-story-refinement/references/preparation-workspace.md)'s
keep or discard decision, then close or retain the workspace, when this
session ends.
End with:

`## SLICE PLAN REFINED`
