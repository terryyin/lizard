---
name: dough-slice-planning
description: >-
  Plans one understood, bounded story or retrospective correction as an
  executable sequence of Behavior/Structure slices with outside-in proof and
  safe stopping points. Use when a selected story is ready for implementation
  planning or an execution retrospective has one bounded correction to plan.
  Stays within the triggering instruction's execution authority: finish after
  writing and reporting the plan unless that instruction explicitly also
  requests execution.
  Reports remaining slice-specific concerns or a limited no-concerns finding;
  does not prescribe the next workflow action or certify execution readiness.
  Concern evidence does not authorize execution.
---

# Slice planning

Write one sufficient executable plan for one understood story or a bounded
retrospective correction as described below. Stay within the
triggering human or parent-agent instruction's explicit execution authority.
Do not implement product code or invoke execution unless that instruction
explicitly also requests execution after planning.

## Require understood planning input

Require one user or stakeholder outcome, its value, evaluable key examples, and
boundaries from later stories. Use
[dough-story-refinement](../dough-story-refinement/SKILL.md) when the selected
story's goal, scope, or examples are unresolved. Use
[dough-story-decomposition](../dough-story-decomposition/SKILL.md) when the
parent problem, candidate selection, or story ordering is unresolved. Never
turn a decomposition seed directly into an execution plan.

For a correction handed off by
[dough-execution-retrospective](../dough-execution-retrospective/SKILL.md#reconcile-findings-with-current-truth),
use its evidenced current findings, one bounded correction outcome, affected
concepts, preserved product promises and constraints, and evaluable proof as the
planning input. Cite the original story and reviewed commits for provenance;
do not invent a new feature story or require the correction to fit the old
story's implementation footprint. The retrospective owns current-truth checks,
constraint disputes, and whether to amend an unfinished plan or create a
follow-up. Missing correction scope or proof stops this planning path.

Write the resulting correction plan with the authoritative correction input
defined by
[planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level)
so later refinement and execution do not need a seed.

## Resolve execution context

Before writing, identify from the user's instructions and this project's guidance:

- the selected story and its seed, when one exists, or the retrospective
  correction input and its source execution;
- the executable-plan root, filename layout, format additions, status
  vocabulary, and lifecycle;
- any supplied slice target and hard limit, including their permitted
  exceptions and overrun escalation;
- required verification, refactoring, commit, and review gates;
- relevant code, tests, stack rules, and Accepted ADRs;
- this project's established North Star location, when one exists; and
- any phase or quick-task conventions that own the plan.

Resolve these from this project, not this skill's location. First reuse a plan
that is active under this project's status vocabulary and identifies the
selected story or correction. Honor the retrospective's unfinished-plan
amendment destination. Otherwise, inspect the established plan entries in the known
root: use the number after the highest allocated entry, preserving its numeric
padding and path layout rather than filling an old gap. Immediately before
writing, recheck the candidate path. If it is occupied, leave it unchanged,
advance to the next number, and check again. Do not add allocation or locking
tooling.

If the canonical plan root is unavailable, name that missing context and stop
before writing; do not ask for a plan number or invent a location. A missing
numeric limit alone is not missing context: apply the linked sizing guidance
without inventing a timing policy. Do not create a new plan under a deprecated
or merely inferred location.

## Write the plan

Record the source, goal, included scope, material exclusions, assumptions, and
key examples without enlarging the story or bounded correction. Read and apply:

- [architectural thinking](references/architectural-thinking.md) to carry
  PFE findings, relevant accepted decisions, and only warranted short-term
  direction into the plan;
- [slice decomposition](../dough-story-decomposition/references/problem-decomposition.md#decompose-slices),
  including its cumulative design assessment, sizing, and escalation rules; and
- [executable-plan decisions](../dough-story-refinement/references/planning.md#write-an-executable-plan),
  including executable proof ownership.

Inspect only the code and tests needed to find the stable outside-in proof entry
point, behavior to extend or preserve, genuine dependencies, and any Structure
justified under the linked slice decomposition rules.

For a concrete uncertain infrastructure or storage assumption, reuse matching
evidence or require one isolated representative proof against the relevant
engine and version. Record the assumption, literal command, critical
postcondition, and result in the plan. Failed proof changes the plan before
broad implementation. Keep experiments off shared and production systems.

During construction, apply those decomposition, cumulative design, and sizing checks: correct
obvious defects such as an independent second outcome before reporting, and
preserve proof ownership and any supplied sizing constraints on every resulting
slice. [dough-slice-plan-refinement](../dough-slice-plan-refinement/SKILL.md)
owns resolving remaining concerns when the coordinator or invoking workflow
separately requests it; do not invoke it as part of writing this plan.

## Report concern evidence

After constructing the plan, report remaining concerns rather than a workflow
verdict:

- Name each remaining slice-specific concern with the affected slice, the
  reason (for example an integration assumption or repeated special-case
  design), and its consequence (for example uncertain sizing or duplicated
  domain rules). Include concerns spanning successive slices. Do not prescribe refinement or certify execution
  readiness.
- When no concerns were identified in this assessment, say so narrowly. Do not
  claim that no further refinement is required or treat that finding as
  permission to execute.

The recipient chooses the next action under the triggering instruction's
authority and project policy. Concern evidence is an assessment of the plan, not
authorization to execute. It does not grant, expand, or replace the triggering
instruction's execution authority.

## Stay within the triggering instruction

After writing and reporting the plan, the next action remains within the
triggering human or parent-agent instruction:

- Planning-only request: report the plan path, ordered slices,
  considered-but-excluded additions, and remaining concerns or the limited
  no-concerns finding, then stop. Do not implement and do not invoke execution.
- Parent-agent delegation that asks only for slice planning: return the plan
  and remaining concerns or the limited no-concerns finding to the parent. The
  parent's broader implementation task is not an explicit execution request to
  this planner.
- Explicit plan-and-execute request: after reporting, the authorized workflow
  may continue into execution without asking again for the same authorization,
  subject to this project's gates and any unresolved concerns that still block
  progress. Prefer the project's established execution path (for example
  [dough-execute-plan](../dough-execute-plan/SKILL.md)) when that path applies.

After the matching case above, end with:

`## SLICE PLAN WRITTEN`
