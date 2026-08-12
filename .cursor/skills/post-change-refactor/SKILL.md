---
name: post-change-refactor
description: >-
  Refactor concepts implicated by the current uncommitted change before commit.
  Use concept-bounded scope even when completion requires untouched code, but
  Jidoka-stop before unapproved cross-subsystem refactoring. Remove duplication,
  unclear naming, shotgun surgery, dead / test-only / redundant code, and
  oversized files; run related tests. Use after finishing implementation work,
  before commit, or on: refactor change, clean up change, post-change
  refactor, before commit cleanup, tidy current change.
---

<objective>
Clean concepts implicated by the **current uncommitted change** so they are
cohesive, capability-named, and non-speculative, then return control for commit.

Purpose: Local wrap-up gate before commit — structure only, no new behavior.

Output: Refactored tree + `## REFACTOR COMPLETE`, or an impact report +
`## REFACTOR JIDOKA STOP`. **Do not commit** — the caller commits after success.
</objective>

<context>
**Mandatory first read:** `.cursor/agent-map.md` (navigation + focused test commands).

**Scope is concept-bounded, not file-bounded.** A candidate must be triggered
by an issue introduced, exposed, or materially aggravated by the current change
or highly related code. Such code represents the same concept, duplicates the
same knowledge, or must change to leave it coherent. Dependency adjacency alone
is neither required nor sufficient.

Find the smallest complete set of representations, callers, tests, fixtures,
and configuration needed for coherence, including untouched code when needed.
Every edit must serve that candidate; do not initiate nearby cleanup.

Discover scope:

```bash
git status
git diff
git diff --cached
```

**Git does not use the Nix prefix.** All other repo tooling does:
`nix develop -c …`

**Plan justification (decision boundary):**
Keep code justified by the **current change** or the **immediate next**
plan unit when `.planning/` exists (read `STATE.md` and the active phase
or quick dir). Anything justified only by a later phase, or by "we might
need it later", is speculative — remove it. No plan → justification comes
only from the current change. The immediate next plan unit may justify
retaining code, but does not independently trigger unrelated refactoring.

**Subsystem boundary:** Core analyzer (`lizard.py`), language readers
(`lizard_languages/`), and extensions (`lizard_ext/`) are separate subsystems.
Tests, fixtures, and configuration following one production seam do not alone
create a crossing. Existing behavior work spanning subsystems also does not
trigger the gate; the **refactoring itself** must require coordinated production
edits across boundaries.

**Invokers:** On-demand developer request, or any workflow that finishes
implementation and hands off to commit (issue fix, language support, extension
work, etc.).
</context>

<process>

<preflight_gate name="discover_scope">
Run the git discovery commands above. If there is no uncommitted change,
report empty scope and emit `## REFACTOR COMPLETE` with no edits.
</preflight_gate>

<preflight_gate name="map_concept_impact">
Before editing, perform a fast read-only pass over every check. For each
candidate, record:

1. The triggering issue and its connection to the current change.
2. The minimum concept-bounded edit set needed for coherent completion.
3. The production subsystems that edit set would touch.

Use references as navigation, not automatic scope. Do not inventory general
repository cleanup.
</preflight_gate>

<preflight_gate name="cross_subsystem_jidoka">
If a candidate requires production refactoring in more than one subsystem,
stop before editing unless the human explicitly authorized that named concept
and those subsystems. Generic "clean up" or "refactor" requests do not qualify.

Return the `<output>` Jidoka report; do not enter the edit/test pass or
substitute a partial refactoring. If discovered while editing, stop before the
first cross-subsystem edit and leave no partial candidate: reverse only this
agent's edits for that candidate, never pre-existing user changes.
</preflight_gate>

After the gate passes, execute the recorded candidates **in check order**.
Do not repeat broad discovery. After all checks pass, return to the caller —
**do not commit** from inside this skill.

<step name="duplication">
- **"New" duplication** means at least one copy is newly introduced or
  closely related to newly introduced code — not that every copy is new.
  Collapse it even when the other side already existed.
- Look for copy-pasted blocks and parallel structures with cosmetic
  differences that the change introduced or made visible (new code
  repeating logic that already lived elsewhere).
- The same concept in two representations counts as duplication, not just
  literal copies.
- **Action:** collapse onto a single representation. Prefer reusing an
  existing helper in the right layer (language reader, extension, shared
  tokenizer utility in `code_reader.py`) over inventing a new one.
</step>

<step name="domain_naming">
- Read every new or renamed identifier — files, modules, classes, functions,
  variables, tests, fixtures.
- Ask: does the name match what a domain reader expects? Does it match
  Lizard's language (complexity metrics, language readers, tokens, state
  machines, CCN, NLOC, extensions)?
- **Action:** rename when intent is unclear, misleading, mixes layers, or
  leaks phase numbers / sequence info. Names describe **capability**, not
  development history.
</step>

<step name="shotgun_surgery">
- Shotgun surgery: **one logical concept** (e.g. a default threshold, a
  language keyword list, an extension option name) forces edits in many
  places for one purpose.
- Give the concept **one** representation. The next change of that shape
  should touch that place — not be scattered again.
- Acceptable extra touchpoints: tests that assert the concept. Do not
  hardcode the same value in reader, extension, CLI options, and test
  fixtures in parallel.
- **Action:** consolidate now behind one seam (one constant, config, or
  module). Leave only low-likelihood one-offs unabstracted.
</step>

<step name="dead_redundant_code">
Remove aggressively whatever the change introduced or exposed that is not
justified by the current change or the immediate next plan unit:

- Code with no caller.
- Unreachable branches.
- Pairs of edits that cancel each other (added then worked around, flags
  that never flip).
- Production code only exercised by unit tests — no real caller from the
  CLI (`lizard` command), `analyze_file` API, language reader pipeline,
  or extension hook.
- Unit tests that overlap another test on the same observable surface
  (same input/output, same entry point).
- Tests that pin internal structure rather than observable behavior — prefer
  the test that drives a stable boundary (`analyze_file`,
  `analyze_source_code`, CLI output) per `basic-development.mdc`.

When in doubt, **delete**. The next phase will reintroduce only what it needs.
</step>

<step name="file_size">
For every file in the current diff and every file proposed for editing:

```bash
wc -l <path>
```

- Files **over 250 lines** must be split (applies to test code too).
- Split along **cohesive seams** — one concept per module, not arbitrary
  line cuts.
- Update imports. Keep the public API stable for callers outside the change.
</step>

<step name="confirm_related_tests">
Run **related** tests for the changed files — not the whole suite.
Use `nix develop -c …` for all commands except `git`.

| Area touched | Focused command |
|--------------|-----------------|
| Core analyzer (`lizard.py`) | `nix develop -c python -m pytest test/test_analyzer.py test/testOutput.py` (add other top-level tests if those modules changed) |
| Language reader (`lizard_languages/`) | `nix develop -c python -m pytest test/test_languages/test<Lang>.py` for each affected language |
| Extension (`lizard_ext/`) | `nix develop -c python -m pytest test/test_extensions/test<Name>.py` for each affected extension |
| CLI / options | `nix develop -c python -m pytest test/testApplication.py test/test_options.py` |
| Broad or cross-cutting change | `nix develop -c python -m pytest` |

Prefer `analyze_file` / `analyze_source_code` integration tests and CLI
output tests over tests that only exercise internal helpers.

All related tests must pass before returning. If a test breaks because of
the refactor (not the original change), fix it now.
</step>

</process>

<success_criteria>
- Every candidate is triggered by the current change or highly related code
- Edits are the smallest coherent concept-bounded set, including untouched files
- No cross-subsystem refactoring without concept-specific human authorization
- No speculative structure beyond current change / immediate next plan unit
- Duplication, naming, shotgun, dead-code, and 250-line checks applied
- Related focused tests green
- No commit created by this skill
- Final output includes `## REFACTOR COMPLETE`
</success_criteria>

<output>
On successful completion, report a short summary to the caller:

1. Which checks led to changes — duplication / naming / shotgun / dead code /
   file size (or "none — already clean").
2. Files renamed, extracted, split, or deleted.
3. Which related tests were run and confirmed passing.

```
## REFACTOR COMPLETE
```

Hand control back. **Do not commit** — the caller commits.

On a cross-subsystem gate, report only decision-relevant facts:

1. Triggering issue and its connection to the current change.
2. Concept requiring refactoring.
3. Affected subsystems and representative files.
4. Why a single-subsystem edit would be partial or misleading.
5. Expected risk and focused validation.
6. Choices: authorize it, defer it, or approve a described narrow exception.

End with:

```
## REFACTOR JIDOKA STOP
```

Do not emit `## REFACTOR COMPLETE`. The caller must surface the decision and
must not consider refactoring complete or commit until the human decides.
</output>

<out_of_scope>
- Do not initiate unrelated refactoring discovered during concept tracing.
- Do not apply cross-subsystem refactoring without explicit, concept-specific
  human authorization.
- Do not start a new phase or add new behavior — structure only.
- Do not run the entire test suite or trigger CI unless the change is broad
  enough that focused tests cannot cover it.
</out_of_scope>
