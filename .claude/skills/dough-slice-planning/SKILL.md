---
name: dough-slice-planning
description: >-
  Plans one understood, bounded story as an executable sequence of
  Behavior/Structure slices with outside-in proof and safe stopping points. Use
  when a selected story is ready for implementation planning. Recommends
  dough-slice-plan-refinement only for complex, low-confidence, or over-budget
  slices; does not implement the plan.
---

# Slice planning

Write one sufficient executable plan for one understood story. Do not implement
product code.

## Require an understood story

Require one user or stakeholder outcome, its value, evaluable key examples, and
boundaries from later stories. Use
[dough-story-refinement](../dough-story-refinement/SKILL.md) when the selected
story's goal, scope, or examples are unresolved. Use
[dough-story-decomposition](../dough-story-decomposition/SKILL.md) when the
parent problem, candidate selection, or story ordering is unresolved. Never
turn a decomposition seed directly into an execution plan.

## Resolve execution context

Before writing, identify from the user's instructions and this project's guidance:

- the selected story and its seed, when one exists;
- the executable-plan path, format additions, status vocabulary, and lifecycle;
- the project's slice target and hard limit, including permitted
  exceptions and overrun escalation;
- required verification, refactoring, commit, and review gates;
- relevant code, tests, stack rules, and Accepted ADRs; and
- any phase or quick-task conventions that own the plan.

Resolve these from this project, not this skill's location. If the
plan destination or slice budget is unavailable, name the missing
context and stop before writing or claiming sizing readiness. Do not create a
new plan under a deprecated or merely inferred location.

## Write the plan

Record the source, goal, included scope, material exclusions, assumptions, and
key examples without enlarging the story. Read and apply:

- [slice decomposition](../dough-story-decomposition/references/problem-decomposition.md#decompose-slices),
  including its sizing and escalation rules; and
- [executable-plan decisions](../dough-story-refinement/references/planning.md#write-an-executable-plan),
  including executable proof ownership.

Inspect only the code and tests needed to find the stable outside-in proof entry
point, behavior to extend, genuine dependencies, and any Structure needed
immediately before the first Behavior.

For a concrete uncertain infrastructure or storage assumption, reuse matching
evidence or require one isolated representative proof against the relevant
engine and version. Record the assumption, literal command, critical
postcondition, and result in the plan. Failed proof changes the plan before
broad implementation. Keep experiments off shared and production systems.

Use [dough-slice-plan-refinement](../dough-slice-plan-refinement/SKILL.md) only
when the reference's refinement conditions apply:

- If all slices are cohesive, have one proof loop, meet this project's target,
  and have no unexplained hard-limit path, report `ready for direct execution`.
- If any trigger remains, report
  `refinement recommended: <affected slices>`. Do not claim an execution-time
  guarantee.

Report the plan path, ordered slices, considered-but-excluded additions, and the
readiness result. End with:

`## SLICE PLAN WRITTEN`
