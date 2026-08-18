---
name: execute-plan
description: >-
  Autonomously execute a plan under .planning/phases/ or
  .planning/quick/ (GSD PLAN.md files, or legacy flat/ongoing plans).
  Applies local wrap-up on every slice: Jidoka, post-change-refactor,
  plan update, commit, and push. Parallel waves OK when safe.
  Triggers on: execute plan, run plan, execute slices, start plan,
  do .planning, execute .planning, run .planning.
---

<objective>
Autonomously execute a GSD-aligned plan with **local wrap-up on every
slice**: Jidoka gates, post-change-refactor, plan update, commit, and push.

Purpose: Local execution overlay for GSD plans — complements
`/gsd-execute-phase` but **requires** this repo's wrap-up per **slice** per
`.cursor/rules/gsd-coexistence.mdc`.

Output: Slices completed with commits pushed, or a Jidoka stop report ending
with `## PLAN EXECUTION COMPLETE` (all slices done) or a stop summary when
waiting on the developer.
</objective>

<context>
**Mandatory first read:** `.cursor/agent-map.md` (navigation + focused test commands).

**Plan locations (preferred → legacy):**

1. `.planning/phases/NN-slug/*-PLAN.md`
2. `.planning/quick/NNN-slug/PLAN.md` (or `*-PLAN.md`)
3. Legacy flat `.planning/*.md` or `ongoing/*.md`

Every executable unit (slice, or GSD plan wave that is one slice) must obey
**Behavior | Structure**, stop-safe, one observable behavior
(`.cursor/rules/planning.mdc`). If it does not, stop and re-plan with
**slice-planning** before implementing.

**Git does not use the Nix prefix.** All other repo tooling does:
`nix develop -c …`

**Coordinator role:** You are a thin coordinator. You do **not** implement slices
yourself (except a single interactive slice). Delegate each slice to a **fresh
sub-agent** so context does not accumulate.

**Wrap-up ownership (hard):** The **coordinator** owns post-change-refactor,
plan update, commit, and push. Implementers must **not** commit and must **not**
run post-change-refactor themselves (nested agents routinely skip spawning a
second Task). The coordinator spawns a **fresh** refactor agent and must see
`## REFACTOR COMPLETE` (or handle `## REFACTOR JIDOKA STOP`) before committing.

**Resume:** The PLAN file is the source of truth for remaining slices (status,
learnings, adjusted later slices).

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
1. Read the plan (GSD phase dir PLAN.md / GSD *-PLAN.md / quick PLAN.md / legacy flat file)
2. Find the next slice whose status is NOT "done"
3. Pre-slice Jidoka + Behavior/Structure check
   → If stop condition → report & STOP
4. DELEGATE implementation only to a fresh sub-agent (see delegation)
5. When implementer finishes:
   a. If Jidoka stop / REVERT & SPLIT → handle as below; do not wrap up
   b. Verify relevant tests were reported green and `git status` shows uncommitted
      work (or a deliberate empty slice with a stated reason). Do not require
      a full CI run before wrap-up.
   c. If the implementer already committed → process failure: stop and report
      (do not continue as if wrap-up succeeded). Prefer fixing by soft-resetting
      an unpushed commit only when safe and the developer has not forbidden it;
      otherwise wait for developer judgment.
6. COORDINATOR WRAP-UP (required — do not skip):
   a. Spawn a fresh Task (`generalPurpose`) whose prompt is: read and follow
      `.cursor/skills/post-change-refactor/SKILL.md` end-to-end on the current
      uncommitted change. Pass slice text, plan path, nix prefix. Instruct:
      do not commit; return must include `## REFACTOR COMPLETE` or
      `## REFACTOR JIDOKA STOP`.
   b. Accept only when the refactor agent output contains `## REFACTOR COMPLETE`.
      On `## REFACTOR JIDOKA STOP`, relay to the developer and STOP (leave
      working tree as the refactor agent left it).
      On missing marker → re-dispatch refactor once; if still missing, STOP.
   c. Format/style: `nix develop -c make pep8`. Fix remaining issues.
   d. Run the full pytest suite: `nix develop -c python -m pytest`
      (`basic-development.mdc` — lizard's suite is cheap enough for wrap-up).
   e. Update PLAN (and SUMMARY if present): mark slice done; brief
      learnings; prune obsolete detail; adjust future slices if warranted.
   f. Post-slice Jidoka — if learnings need developer judgment: commit and push
      work so far, then STOP.
   g. Commit only when CI-safe (no intentional red tests) then `git push`.
   h. Verify: `## REFACTOR COMPLETE` was observed this slice AND commit is pushed.
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
Use the **Task tool** (`subagent_type: "generalPurpose"`; or GSD `gsd-executor`
when inside `/gsd-execute-phase` — still require **coordinator-owned** wrap-up
below; do not rely on `gsd-executor` to run local post-change-refactor).

The implementer prompt **must** include:

1. **Plan file path** and **which slice** to implement (paste the
   slice text).
2. **Jidoka stop conditions** (copy the list above).
3. **Implementation rules**: `planning.mdc` (Behavior/Structure, TDD, slice
   discipline, **time budget** ~5 min fuzzy / >10 min hard finer-decompose,
   **no commit on red**, **do not deliberately break CI**; run tests relevant
   to the change during the slice), `gsd-coexistence.mdc`, and other applicable
   rules (`basic-development.mdc`, `issue.mdc`, `lizard-rule.mdc`). **Naming:**
   permanent artifacts by **capability/domain**, never GSD phase number.
4. **Hard stop before wrap-up:** Do **not** commit. Do **not** push. Do **not**
   update PLAN to `done`. Do **not** run post-change-refactor (and do not
   "apply the refactor skill yourself"). Leave the working tree uncommitted
   with relevant tests green for the coordinator.
5. **Revert & split** instructions (see `revert_and_split` step).
6. **Nix prefix**: `nix develop -c <command>`. **Git commands do not need the Nix prefix.**
7. **Return**: short summary — implementation ready for wrap-up (tests run),
   Jidoka stop, or reverted and split. Do not claim slice "done" in git terms.

**Do NOT pass entire plan history** — only the current slice. Resume context
lives in the PLAN file on disk.
</step>

<step name="wrap_up">
**Coordinator-owned** (after implementer returns with relevant tests green,
uncommitted):

1. **Spawn post-change-refactor** — Fresh Task (`generalPurpose`) that reads
   `.cursor/skills/post-change-refactor/SKILL.md` and runs it end-to-end on the
   current uncommitted change. Pass:
   - Slice text being closed
   - Plan file path (for immediate-next-slice justification)
   - Nix prefix rule
   - Do **not** commit
   - Return must end with `## REFACTOR COMPLETE` or `## REFACTOR JIDOKA STOP`
2. **Gate** — Proceed only on `## REFACTOR COMPLETE`. On Jidoka stop or missing
   marker, follow the coordinator_loop rules above (do not commit).
3. **Format** — `nix develop -c make pep8`. Fix any remaining issues.
4. **Full tests** — `nix develop -c python -m pytest`. All must pass.
5. **Reflect & re-plan** — update PLAN (and SUMMARY if present):
   - Brief learnings that change remaining work.
   - Mark slice **done**; prune obsolete detail from that slice.
   - Adjust future slices when warranted.
6. **Post-slice Jidoka** — if learnings need developer judgment: commit and push
   work so far, then return a Jidoka stop (do not silently continue).
7. **Commit** — only when the tree would not intentionally break CI: no
   failing tests from this change. Do **not** skip the full pytest suite at
   this wrap-up (lizard's suite is cheap). Stage all changes; message may use
   GSD-style `{type}({phase}-{plan}): …` or the repo's recent convention.
8. **Push** — `git push`.
</step>

<step name="revert_and_split">
A slice is **too big** when:

- Changes span many unrelated files with no clear single behavior emerging.
- Tests are not converging after reasonable effort.
- Wall-clock for the slice (implementation + test runs) exceeds the
  **time budget** in `planning.mdc`: scrutinize after **~5 min**; after
  **>10 min**, finer decompose and retry is **required** unless a good reason
  is stated (and reported to the coordinator / developer).

When this happens:

1. `git checkout .` — revert all uncommitted changes.
2. `git clean -fd` — remove untracked files from the attempt.
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
- Coordinator owns wrap-up: fresh post-change-refactor Task → `## REFACTOR COMPLETE` → pep8 → full pytest → plan update → commit → push
- Pre- and post-slice Jidoka checks applied
- Parallel waves only when touch sets and PLAN writes do not conflict
- Spent planning history cleaned when entire plan is done
- Final output includes `## PLAN EXECUTION COMPLETE` when all slices finish
</success_criteria>

<output>
When the loop ends (all slices done or a stop condition):

1. **Summary** — which slices were completed this run.
2. **Current state** — PLAN.md path and next undone slice for resume (if stopped).
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
</out_of_scope>
