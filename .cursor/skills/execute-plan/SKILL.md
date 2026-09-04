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
Autonomously execute a GSD-aligned plan with **local wrap-up on every
slice**: Jidoka gates, post-change-refactor, selective formatting, plan update,
commit, and push.

Purpose: Local execution overlay for GSD plans — complements
`/gsd-execute-phase` but **requires** this repo's wrap-up per **slice** per
`.cursor/rules/gsd-coexistence.mdc`.

Output: Slices completed with commits pushed, or a Jidoka stop report ending
with `## PLAN EXECUTION COMPLETE` (all slices done) or a stop summary when
waiting on the developer.
</objective>

<context>
**Mandatory first read:** `.cursor/agent-map.md` (navigation + focused test commands).
Before executing, also read [delegation.md](references/delegation.md) and
[wrap-up.md](references/wrap-up.md) in full.

**Plan locations:**

1. `.planning/phases/NN-slug/*-PLAN.md`
2. `.planning/quick/NNN-slug/PLAN.md` (or `*-PLAN.md`)

Every executable unit (slice, or GSD plan wave that is one slice) must obey
**Behavior | Structure**, stop-safe, one observable behavior or its immediately
enabling Structure
(`.cursor/rules/problem-decomposition.mdc`). If it does not, stop and re-plan with
**slice-planning** before implementing.

Reject a story-decomposition seed as execution input. Require a PLAN for one
selected story or a GSD phase whose tasks already pass the execution-leaf gate.

**Git does not use the Nix prefix.** All other repo tooling does:
`nix develop -c …`

**Coordinator role:** You are a thin coordinator. You do **not** implement slices
yourself (except a single interactive slice). Delegate each slice to a **fresh
sub-agent** so context does not accumulate.

**Wrap-up ownership (hard):** The **coordinator** owns post-change-refactor,
selective formatting, plan update, commit, and push. Implementers must **not**
commit and must **not** run post-change-refactor themselves (nested agents
routinely skip spawning a second Task). The coordinator spawns fresh refactor
and formatting agents and must see their completion markers (or handle their
Jidoka stops) before committing.

**Resume:** The PLAN file being executed is the source of truth for remaining
slices (status, learnings, adjusted later slices). Do **not** write
`.planning/STATE.md`, and do not treat it as execution or resume state.

**Parallelism:** Run multiple independent plans/slices in parallel (GSD waves or
Task agents) when `files_modified` / touch sets do not overlap and they do not
contend on the same PLAN file. Otherwise run sequentially. Each parallel
unit still gets its own coordinator-owned refactor → commit before the next
dependent unit starts.
</context>

<process>

<preflight_gate name="jidoka_stop_conditions">
Run with full autonomy **but stop the line** when something requires a
developer's brain.

**Stop and wait when:**

- **Value decision** — multiple valid directions with different user-facing
  trade-offs; the plan says "TBD", "decide", "option A / B", or you discover
  such a fork during implementation.
- **Design decision** — a structural choice that affects future slices or
  overall architecture.
- **Authentication / credentials** — secrets, API keys, login flows, or
  permissions the agent cannot supply.
- **Unexpected failure you cannot diagnose** — test fails for reasons unrelated
  to the current change, CI breaks on something external, etc.
- **Ambiguity** — the slice description is unclear and guessing wrong would
  waste a commit.
- **Stale story decomposition** — evidence changes the selected story's
  beneficiary, outcome, evaluation, or boundary; or changes whether/when a
  sibling story should be delivered.

When stopping: explain **what** you learned, **why** you stopped, and **what
decision** the developer needs. Then wait.

**Do NOT stop for:**

- Routine implementation choices (naming, file placement, test structure) where
  existing rules and conventions give a clear answer.
- Minor refactoring needed to make the slice fit.
- Test failures caused by your own change (fix them).

**Check Jidoka both before and after each slice:**

- **Before** (coordinator, on the slice *description*) — safe to start
  autonomously? Value/design forks, ambiguity, missing credentials, Behavior/Structure
  grammar.
- **After** (implementer return + refactor return) — did work reveal something the
  plan did not anticipate? Stop even if the slice succeeded.
</preflight_gate>

<step name="coordinator_loop">
```
1. Read the plan (GSD phase dir PLAN.md / GSD *-PLAN.md / quick PLAN.md)
2. Find the next slice whose status is NOT "done"
3. Pre-slice Jidoka + Behavior/Structure check
   → If stop condition → report & STOP
4. DELEGATE implementation only to a fresh sub-agent (see delegation)
5. When implementer finishes:
   a. If Jidoka stop / REVERT & SPLIT → handle as below; do not wrap up
   b. Verify relevant tests were reported green (no intentional CI-breaking
      red) and `git status` shows uncommitted
      work (or a deliberate empty slice with a stated reason). Do not require
      a full CI run before wrap-up.
   c. If the implementer already committed → process failure: stop and report
      (do not continue as if wrap-up succeeded). Prefer fixing by soft-resetting
      an unpushed commit only when safe and the developer has not forbidden it;
      otherwise wait for developer judgment.
6. COORDINATOR WRAP-UP (required — do not skip): follow `<step name="wrap_up">`.
7. Go to step 1 (next slice)
8. All slices done → clean up spent plan history (planning.mdc) → report & STOP
```

Recognize slices by headings/status or GSD plan tasks. Typical local section:

```markdown
### Short capability description
Type: Behavior | Structure
Status: planned / in-progress / done

Pre-condition / trigger / post-condition (Behavior)
— or —
Structure change + immediate next Behavior it unlocks
```

When the **entire** plan is complete: actively clean spent planning history per
`planning.mdc` (keep product/code; drop disposable diary under `.planning/`).
</step>

<step name="delegation">
Delegate exactly as specified in [delegation.md](references/delegation.md).
</step>

<step name="wrap_up">
Run the coordinator-owned sequence in [wrap-up.md](references/wrap-up.md).
</step>

<step name="revert_and_split">
A slice is **too big** when:

- Changes span many unrelated files with no clear single behavior emerging.
- Tests are not converging after reasonable effort.
- Wall-clock for the slice (implementation + test runs) exceeds the
  **time budget** in `problem-decomposition.mdc`: scrutinize after **~5 min**; after
  **>10 min**, finer decompose and retry is **required** unless a good reason
  is stated (and reported to the coordinator / developer).

When this happens:

1. Identify the exact tracked and untracked paths created or changed by this
   attempt. Preserve every pre-existing developer change.
2. Safely park or revert only attempt-owned WIP. Never use broad
   `git checkout .`, `git clean -fd`, or another command that can discard
   unrelated dirty state. If ownership cannot be isolated, stop for developer
   judgment.
3. Invoke **slice-planning** to split into Behavior/Structure slices
   sized for the ~5 minute fuzzy goal (including test execution).
4. Update the PLAN in the GSD phase or quick dir.
5. Commit and push the updated plan.
6. Return "reverted and split" to the coordinator (include elapsed time and
   whether the 10-minute hard trigger applied).
</step>

</process>

<success_criteria>
- Each slice implemented by a fresh sub-agent (coordinator does not accumulate implementation context)
- Coordinator owns wrap-up: fresh post-change-refactor Task →
  `## REFACTOR COMPLETE` → fresh format-changed Task →
  `## FORMAT CHANGED COMPLETE` → full pytest → plan update → commit → push
- Pre- and post-slice Jidoka checks applied
- Stale story decomposition stops execution after the current safe wrap-up
- Parallel waves only when touch sets and PLAN writes do not conflict
- Spent planning history cleaned when entire plan is done
- Final output includes `## PLAN EXECUTION COMPLETE` when all slices finish
</success_criteria>

<output>
When the loop ends (all slices done or a stop condition):

1. **Summary** — which slices were completed this run.
2. **Current state** — the PLAN being executed and next undone slice for resume
   (if stopped). Do not report GSD `STATE.md` as execution state.
3. **Next action** — developer decision needed, or confirm cleanup done.

```
## PLAN EXECUTION COMPLETE
```

(Use when all slices are done. For Jidoka stops, report the stop reason and wait
— do not emit the completion marker until the developer resolves and work resumes.)
</output>

<out_of_scope>
- Do not implement slices in the coordinator agent (except single interactive slice).
- Do not skip coordinator-owned post-change-refactor, commit, or push per slice.
- Do not accept an implementer self-refactor or a missing `## REFACTOR COMPLETE` as wrap-up.
- Do not pass full plan history to sub-agents.
- Do not continue past a Jidoka stop without developer input.
- Do not commit on TDD red alone, or close a slice with deliberate CI-breaking
  failures. Do not skip full pytest at wrap-up.
- Do not stage or commit before the fresh format-changed agent completes.
- Do not write `.planning/STATE.md`; execution state lives in the PLAN file.
- Do not treat the lint-only pre-commit hook as a formatter or let it mutate Git
  state.
</out_of_scope>
