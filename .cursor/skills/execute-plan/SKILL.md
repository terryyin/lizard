---
name: execute-plan
description: >-
  Autonomously execute a plan under .planning/phases/ or
  .planning/quick/ .
  Applies local wrap-up on every slice: Jidoka, post-change-refactor,
  selective formatting, plan update, commit, and push. Parallel waves OK when
  safe.
  Triggers on: execute plan, run plan, execute slices, start plan,
  do .planning, execute .planning, run .planning.
---

<objective>
Execute a GSD-aligned PLAN autonomously with the per-slice delivery contract in
`.cursor/rules/gsd-coexistence.mdc`, including when using `/gsd-execute-phase`.
</objective>

<context>
**Mandatory first read:** `.cursor/agent-map.md` (navigation + focused test commands).
Before executing, also read [delegation.md](references/delegation.md),
[disposable-research.md](references/disposable-research.md), and
[destructive-later-outcome-check.md](references/destructive-later-outcome-check.md),
and [wrap-up.md](references/wrap-up.md) in full.

**Input:** A PLAN under `.planning/phases/NN-slug/` or `.planning/quick/NNN-slug/`
(`PLAN.md` or `*-PLAN.md`) for one selected story or bounded GSD phase; reject
seeds. Every slice/wave must pass `problem-decomposition.mdc`'s stop-safe
Behavior/Structure gate. Refine coarse or low-confidence leaves on the same
PLAN before implementation; straightforward leaves may execute directly.

**Git does not use the Nix prefix.** All other repo tooling does:
`nix develop -c …`

**Ownership:** Delegate each slice to a fresh agent under `delegation.md`;
implement locally only for a single interactive slice. The coordinator owns
the mandatory `wrap-up.md` sequence, including independent refactor, formatting,
plan update, commit, and push. Implementers stop before wrap-up.

**Resume:** Use the PLAN's status, learnings, and adjusted leaves as execution
state; reconcile ownership and evidence under planning.mdc's Proof decisions.
Do not write `.planning/STATE.md` or use it as execution/resume state.

**Parallelism:** Only when touch sets and PLAN writes do not overlap. Each unit
completes its own coordinator-owned wrap-up before dependent work starts.
</context>

<process>

<preflight_gate name="jidoka_stop_conditions">
Check before delegation and after both implementer and refactor returns,
even when tests pass. Stop for:

- unresolved user-facing value trade-offs or structural choices affecting
  future slices/architecture;
- missing credentials/permissions or ambiguity that could waste a commit;
- failures unresolved by focused diagnosis;
- evidence changing the selected story's beneficiary, outcome, evaluation,
  boundary, or sibling delivery/order.

Explain the finding and required decision, then wait for the developer. At a
post-slice stop, first deliver safe work as specified in `wrap-up.md`.
Resolve routine naming/placement/test choices, minor refactoring, and failures
caused by your own change without stopping.
</preflight_gate>

<step name="coordinator_loop">
```
1. Read the PLAN (slice headings/status or GSD tasks per planning.mdc).
2. Find the next slice whose status is NOT "done"
3. Check Jidoka, Behavior/Structure, refinement triggers, and planning.mdc's Proof
   decisions; for destructive work, run the [named later-outcome check](references/destructive-later-outcome-check.md)
   → If Jidoka stop condition → report & STOP
   → If the selected outcome is valid but a refinement trigger applies, invoke
     slice-plan-refinement on this PLAN, then reread it before continuing
4. Delegate implementation under references/delegation.md.
5. When implementer finishes:
   a. If Jidoka stop / REVERT & REFINE → handle as below; do not wrap up
   b. Apply the proof acceptance/reuse gate in references/wrap-up.md.
      Verify git status shows uncommitted work (or an explained empty slice).
   c. If the implementer already committed → process failure: stop and report
      (do not continue as if wrap-up succeeded). Prefer fixing by soft-resetting
      an unpushed commit only when safe and the developer has not forbidden it;
      otherwise wait for developer judgment.
6. Run references/wrap-up.md end-to-end; recheck Jidoka after refactor.
7. Go to step 1 (next slice)
8. All slices done → clean up spent plan history (planning.mdc) → report & STOP
```

</step>

<step name="revert_and_refine">
A slice is too big when changes lack one coherent behavior, tests fail to
converge, or it exceeds `problem-decomposition.mdc`'s budget: scrutinize after
~5 minutes; after >10 minutes, refinement/retry is required unless a reason is
stated to the coordinator/developer. Include implementation and test runtime.

When this happens:

1. Inventory attempt-owned tracked/untracked paths and safely park or revert
   only that WIP. Preserve pre-existing changes; never use broad `git checkout .`
   or `git clean -fd`. Unclear ownership requires developer judgment.
2. Invoke **slice-plan-refinement** on the same PLAN for smaller leaves.
3. Have the coordinator commit and push the updated PLAN.
4. Return "reverted and refined" with elapsed time and whether the hard trigger
   applied.
</step>

</process>

<output>
Report completed slices and cleanup, or the active PLAN, next undone slice,
and required developer decision. Emit `## PLAN EXECUTION COMPLETE` only when all
slices are done; a Jidoka stop waits without that marker.
</output>
