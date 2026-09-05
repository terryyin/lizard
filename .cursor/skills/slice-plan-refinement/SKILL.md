---
name: slice-plan-refinement
description: >-
  Refine an existing slice PLAN in place into smaller commit-sized
  Behavior/Structure leaves. Use after slice-planning when slices are complex or
  sizing confidence is low, and whenever execution overruns the five/ten-minute
  budget. Creates no new plan. Skip when the existing plan is already simple and
  plausibly commit-sized.
---

<objective>
Edit an existing PLAN in place so each remaining slice is plausibly completable
within the execution-leaf target and has no unexplained path beyond its hard
limit, including focused verification. The thresholds are defined in
`problem-decomposition.mdc`. This is a planning hypothesis; execution enforces
the budget.

Apply `.cursor/rules/problem-decomposition.mdc` and
`.cursor/rules/planning.mdc`. Do not create another plan file or change the
selected story outcome.
</objective>

<input_gate>
Require an existing PLAN under `.planning/phases/` or `.planning/quick/`.

- No PLAN yet → use **slice-planning**.
- Story understanding must change → use **story-refinement**; use
  **story-decomposition** for parent-problem or candidate-ordering changes.
- Existing slices are already clear, cohesive, single-proof-loop, meet the
  target, and have no unexplained hard-limit path → execute directly;
  refinement is optional.
</input_gate>

<refinement_triggers>
Refine when any remaining slice:

- contains multiple independent post-conditions or proof loops;
- requires several separable implementation beats before any green result;
- hides preparation not tied to the immediate next Behavior;
- has low sizing confidence because the execution path or integration boundary
  is unclear;
- is likely to exceed the target or could plausibly exceed the hard limit,
  excluding a focused test whose runtime alone explains the duration;
- has already exceeded the target without converging; or
- has exceeded the hard limit without a stated exception.
</refinement_triggers>

<process>

<step name="inspect_the_existing_plan">
Read the PLAN and only the code/tests needed to judge execution boundaries.
Preserve completed slices and the selected story's goal and scope.
Apply planning.mdc's Proof decisions to the existing promise ownership and
completed evidence.

Classify each remaining slice:

| Result | Decision |
|--------|----------|
| **Ready** | One Behavior/Structure gate, one proof loop, cohesive change, meets the target |
| **Refine** | Same story, but the leaf violates its gate, has multiple beats, has low confidence, or could exceed the budget |
| **Escalate** | A smaller viable cut would change the selected story or sibling-story order |

Route **Escalate** using the input gate above. Refine every **Refine** slice here.
</step>

<step name="split_remaining_slices">
Use the permitted splitting moves in `problem-decomposition.mdc`.

For every replacement leaf:

1. Keep one Behavior, or one Structure immediately before its Behavior.
2. Keep one outside-in proof loop and a CI-safe commit boundary.
3. Include implementation, focused verification, and slice-local cleanup in the
   sizing hypothesis.
4. Preserve value/learning order and genuine prerequisites.
5. Split again when the leaf still has separable beats or a plausible
   hard-limit path.

Do not split tests from the Behavior they prove, end a leaf on red, or create
horizontal layer slices.
</step>

<step name="handle_execution_overrun">
When refinement follows an execution attempt:

1. Record elapsed time, completed evidence, failure/thrash point, and the sizing
   assumption that proved false.
2. Confirm attempt-owned WIP was safely parked or reverted before editing the
   PLAN. Preserve developer changes; if ownership is unclear, stop for human
   judgment.
3. Return the failed slice to `planned` and replace it with smaller leaves.
4. Refine later slices only when the same disproved assumption applies to them.
5. Keep a stated exception when elapsed time came from one focused test or
   external wait that decomposition cannot reduce.
</step>

<step name="update_in_place">
Edit the same PLAN using the slice format in `planning.mdc`.

- Do not create a new file or directory.
- Preserve completed-slice history needed for resume.
- Replace obsolete planned detail rather than appending a second competing
  breakdown.
- Reconcile promise ownership and observations against the original leaves
  under planning.mdc's Proof decisions before declaring the revised PLAN ready.
- Record only learnings that changed the refinement.
- Do not implement, commit, or push unless the invoking workflow separately
  authorizes those actions.
</step>

</process>

<success_criteria>
- The same PLAN was updated in place.
- Every remaining leaf passes the Behavior/Structure gate and has one proof
  loop.
- Each remaining leaf is a plausible target-sized hypothesis with no
  unexplained hard-limit path; execution time is not claimed as guaranteed.
- Completed slices and the selected story outcome are unchanged.
- Final response ends with `## SLICE PLAN REFINED`.
</success_criteria>

<output>
Report the PLAN path, replaced slices, resulting leaves, sizing exceptions, and
whether execution can resume.

```text
## SLICE PLAN REFINED
```
</output>

<out_of_scope>
- Creating a plan or decomposition artifact.
- Changing the selected story outcome or sibling-story order.
- Implementing or verifying product code.
</out_of_scope>
