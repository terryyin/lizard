---
name: slice-planning
description: >-
  Turn one selected story into an executable GSD-aligned plan of stop-safe
  Behavior/Structure leaves. Use when the story’s value, outcome, and boundaries
  are clear, or when an execution attempt needs finer re-slicing. Use
  story-decomposition for unresolved requirements or multiple candidate stories.
---

<objective>
Write an executable PLAN for one selected story. Use
`.cursor/rules/problem-decomposition.mdc` for slice decisions and
`.cursor/rules/planning.mdc` for the artifact and lifecycle.
</objective>

<input_gate>
Proceed only when the input names:

- one user or stakeholder outcome;
- why it matters;
- how the result can be evaluated;
- boundaries separating it from later stories.

If any is unresolved, or several outcomes remain, use **story-decomposition**.
Never pass a story-decomposition seed directly to execution.
</input_gate>

<locations>
- Existing GSD phase: `.planning/phases/NN-slug/*-PLAN.md`
- Ad-hoc selected story: `.planning/quick/NNN-slug/PLAN.md`
- Never create a new plan under `ongoing/`.
</locations>

<process>

<step name="record_the_story_contract">
Record the source seed/story when present, beneficiary, value, boundaries, and
one representative pre-condition → trigger → post-condition.

If this reveals a value, outcome, or boundary choice, stop and return to
**story-decomposition**.
</step>

<step name="inspect_execution_context">
Read only the code, tests, stack rules, and relevant Accepted ADRs needed to
decide:

- the stable outside-in test or demonstration entry point;
- existing behavior and tests to extend;
- genuine dependencies;
- whether one Structure slice is needed before the first Behavior.

Do not create slices per discovered file, component, or layer.
</step>

<step name="cut_and_order_leaves">
Apply the execution-leaf gate and sizing rules from
`problem-decomposition.mdc`.

For every leaf:

1. Choose Behavior or Structure.
2. Record the required fields from the gate.
3. Name focused verification.
4. Split if it contains multiple independent post-conditions.
5. Place Structure immediately before its Behavior.
6. Order Behaviors by user value, then learning value, then genuine
   prerequisites.

Do not end a slice on CI-breaking red; do not commit failing pytest.
Keep product and test artifacts capability-named.
</step>

<step name="replan_an_oversized_attempt">
When called after an execution overrun:

1. Preserve discoveries and identify attempt-owned WIP.
2. Never discard pre-existing developer changes or ambiguous dirty state.
3. Apply the five- and ten-minute decisions in `problem-decomposition.mdc`.
4. Replace the oversized remainder with smaller leaves in the current PLAN.
5. If evidence changes the selected story rather than only its leaves, mark the
   PLAN as awaiting story decomposition review and stop.
</step>

<step name="write_the_plan">
Use the required PLAN contents and slice format from `planning.mdc`.

If a GSD plan task bundles multiple Behaviors or speculative Structure, rewrite
it before execution. Do not implement feature code while planning.
</step>

</process>

<success_criteria>
- Input is one selected and bounded story.
- Every leaf passes the Behavior/Structure gate and names proof.
- Leaves satisfy the execution sizing decisions.
- The plan is under `.planning/phases/` or `.planning/quick/`.
- Final response ends with `## SLICE PLAN WRITTEN`.
</success_criteria>

<output>
Report the plan path, ordered leaves, key execution decisions, and any evidence
that may require story-decomposition review.

```text
## SLICE PLAN WRITTEN
```
</output>

<out_of_scope>
- Broad requirement exploration.
- Feature implementation.
- Direct execution of a story-decomposition seed.
</out_of_scope>
