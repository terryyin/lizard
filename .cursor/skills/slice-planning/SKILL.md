---
name: slice-planning
description: >-
  Decompose work into GSD-aligned plans whose executable units are
  Behavior/Structure slices (one observable behavior per slice, stop-safe).
  Use when planning new features, breaking down large tasks, or when a fix /
  make-test-pass breakdown has overrun the time-box (see planning.mdc).
  Triggers on: plan, decompose, slice, break down, task too large, stuck.
---

<objective>
Decompose work into stop-safe **slices**: each is **Behavior** or **Structure**,
with one observable behavior (or one Structure change for the immediate next
Behavior only). Full grammar: `.cursor/rules/planning.mdc`.

GSD **phase** is a roadmap capability (`/gsd-plan-phase`,
`.planning/phases/NN-slug/`). This skill writes **slices** inside a plan.

Use when planning, splitting an oversized slice, or when a fix /
make-test-pass attempt has overrun the breakdown time-box. If
`/gsd-plan-phase` output violates Behavior/Structure — rewrite until every
plan unit is one slice.

Output: Written plan under `.planning/` + summary ending with
`## SLICE PLAN WRITTEN`.
</objective>

<context>
**Hard grammar (non-negotiable):** Every slice is **Behavior** or **Structure**,
**stop-safe**, and carries **one** observable behavior (or one structure change
for the **immediate next** behavior only). Full rules: `.cursor/rules/planning.mdc`.

**Where to put plans (GSD-aligned):**

| Location | Use |
|----------|-----|
| `.planning/phases/NN-slug/` | GSD roadmap phases — `*-CONTEXT.md`, `*-PLAN.md`, `*-SUMMARY.md`, … |
| `.planning/quick/NNN-slug/` | Ad-hoc plans not yet on the roadmap |
| `ongoing/` | Legacy only — do not add new plans |

Primary executable file: `*-PLAN.md` or `PLAN.md`. Sub-decomposition: additional
`*-PLAN.md` in the same directory or clearly marked slice sections — each
still Behavior/Structure.

**History:** keep resume-useful status and brief learnings while in progress; when
the whole plan is done and shipped as code/permanent docs, **clean up** spent
planning history (`.cursor/rules/planning.mdc`).

**Git does not use the Nix prefix.**
</context>

<process>

<step name="time_box_recompose">
When a fix / make-test-pass / one-problem attempt overruns the time-box
(`planning.mdc`):

| Elapsed | Action |
|---------|--------|
| > 5 min | Scrutinize the breakdown — too coarse? Prefer finer decompose, revert WIP, retry on a smaller slice |
| > 10 min | **Hard trigger** unless good reason (long suite, external wait): stop implementing, revert/stash WIP, cut a smaller Behavior/Structure slice |

For the hard trigger:

1. **Stop** implementing.
2. Summarize learnings (discoveries, blockers, partial progress).
3. `git stash -m "WIP: <brief description>"` (or revert uncommitted WIP).
4. Decompose remaining work (steps below).
5. Write `.planning/quick/NNN-slug/PLAN.md` (next free `NNN`). Promote to
   `phases/` when roadmap-bound.
6. Report and wait (or execute the first smaller slice if already authorized).
</step>

<step name="decompose">
**Default:** Split by **user scenarios and outcomes**, not by layers
(tokenizer → reader → CLI) or "build the abstraction first."

**Order scenarios** from **common / general** toward **more specific** preconditions.

**Solutions:** First slices implement a **narrow, concrete** outcome. Later slices
**generalize or reuse** only after real repetition — not a big generic framework up front.

**Regression:** If behavior **already exists** but has **no automated test**, prefer
a **dedicated slice**: add a regression test and make it pass.

**Extending tests:** If similar behavior **already has** tests, extend them; avoid
duplicate test code. Fold "test fails → pass" into the feature slice, **or** use a
short slice where the **new** test fails first. **Keep at most one intentionally
failing test** while driving a change.

**Big refactor:** If making the test pass needs a **large structural** change, plan
**that structure as its own slice** before (or as the first cut of) the feature.

**Observable-behavior slices:** Each slice maps to an **externally observable**
scenario (CLI output, `analyze_file` / `analyze_source_code` result, warning,
extension output). Different slices may use **different preconditions**. Add or
extend tests in **capability-named** files (e.g. `testPython.py`,
`testHalstead.py`) — **never** name files or tests after the GSD phase.

**Still too big:** Split by **one small part of the outcome** per slice.

**Time budget when sizing slices:** Each Behavior/Structure unit should be
achievable by an agent/sub-agent in about **5 minutes** wall-clock including
targeted test runs (`planning.mdc`). Prefer many small slices over ones that
routinely blow past 5–10 minutes.

### Testing strategy

| Layer | Role |
|--------|------|
| **Observable (integration)** | Each slice: tests covering the **main user behavior** for that slice via `analyze_file.analyze_source_code`, `analyze_file`, or CLI output (`testApplication.py`). |
| **Narrow unit tests** | Formatting, errors, invalid input, edge paths — black-box, minimal, full coverage of those concerns. Mock only the filesystem. |

**Tests are owned by capability; slices only schedule work.**

**Observable behavior first:**

- Prefer tests driving **high-level entry points** (`analyze_file`,
  `analyze_source_code`, CLI `lizard` output) — not internal helpers unless
  that API **is** the deliberate isolated contract.
- **Minimum tests for same coverage** — fewer boundary tests over many scattered
  tests pinning private functions.
- **Cohesion** — assertions for one user-visible behavior live together when practical.
- **Pure-contract unit tests** — pure functions, validation, error messages: inputs → outputs
  when that API is the stable contract.

- **Test-driven:** tests first or alongside implementation.
- **Slice-complete:** everything in a slice justified and tested inside that slice.
- **No dead code:** production code used by current CLI / `analyze_file` **or**
  unit tests for non–happy-path behavior. Normal user paths need an observable
  test, not unit tests of internals alone.

**Focused tests:** "tests for this slice" = the relevant pytest file(s), not
the entire suite until wrap-up (`basic-development.mdc`).

### Test-driven workflow

1. Add or change a test; run and confirm it **fails**.
2. Confirm failure for the **right reason** (not typo or env issue).
3. Improve assertion/message if unclear.
4. Smallest change that makes the test pass.
5. Refactor with tests green.

### Slice discipline

Before closing a slice: `.cursor/rules/planning.mdc` (clean up, tests,
Jidoka, plan update, deploy gate, parallelism).

### Interim behavior

- **Allowed** when it gets the feature to users faster or gives earlier
  observable-test feedback.
- **Remove** when a later slice replaces it with the intended design.
</step>

<step name="write_plan_document">
Document **important structure and intent**. **Update** when learnings change
remaining work. Remove text that no longer helps the current snapshot.

Include:

- Slices with status (done / in-progress / planned) and type (Behavior | Structure)
- Key design decisions and rationale
- Discoveries that affect remaining work

**Naming rule:** Test files, modules, classes, directories reflect **domain
capability** (e.g. `testPython.py`, `lizardhalstead.py`), not GSD
phase or slice number. Those numbers belong only under `.planning/`. In
`PLAN.md`, number slices for progress (`### 1. Capability heading`) and keep
the capability name.

If GSD `/gsd-plan-phase` / discuss produced a plan violating Behavior/Structure —
**rewrite or split** until every plan unit is one slice.
</step>

</process>

<success_criteria>
- Every slice is Behavior or Structure, stop-safe, one observable behavior
- Plan written to `.planning/quick/` or `.planning/phases/` (not `ongoing/`)
- Scenario-first ordering; capability-named permanent artifacts
- Final output includes `## SLICE PLAN WRITTEN`
</success_criteria>

<output>
Report to the developer:

1. Plan location and slice summary.
2. Key design decisions.
3. Discoveries affecting remaining work.

```
## SLICE PLAN WRITTEN
```

Then wait for their decision.
</output>

<out_of_scope>
- Do not implement feature code during planning (except tiny fixes from retrospective).
- Do not add new plans under `ongoing/`.
- Do not encode GSD phase numbers in product file/test names.
- Do not plan a slice that deliberately ends with CI-breaking red tests — commit
  only after a full TDD cycle (green).
</out_of_scope>
