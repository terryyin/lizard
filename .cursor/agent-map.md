# Agent Map

Short navigation index — start here before diving into large modules. Skill contracts: `.cursor/skills/`.

## Work Areas

- **Core analyzer & CLI:** `lizard.py` — `analyze()`, `analyze_file`, `analyze_source_code`, option parsing, file discovery, output orchestration.
- **Language readers:** `lizard_languages/` — one module per language; shared bases in `code_reader.py`, `clike.py`, `golike.py`, `rubylike.py`. Registration in `lizard_languages/__init__.py` (`languages()` list).
- **Extensions:** `lizard_ext/` — output formatters (`htmloutput`, `csvoutput`, `xmloutput`, …), metric plugins (`lizardmccabe`, `lizardduplicate`, …), and `extension_base.py`. Wired from `lizard.py` and `lizard_ext/__init__.py`.
- **Tests:** `test/` — mirror production layout:
  - `test/test_languages/test<Lang>.py` per language reader
  - `test/test_extensions/test<Name>.py` per extension
  - Top-level integration: `test/test_analyzer.py`, `test/testApplication.py`, `test/testOutput.py`, `test/test_options.py`

## Domain Language

Canonical glossary: [ADR-0001](../docs/adrs/0001-ubiquitous-language-accepted.md). Lizard measures **CCN**, **NLOC**, **token count**, **parameter count**, and **nesting depth**. Parsing uses **token generators** and **state machines** (`CodeStateMachine`, `CLikeStates`, **language readers**). Prefer capability names over phase or ticket numbers in product code.

## Commands

Run repo tooling through Nix. For AI agents, prefix every command except `git`:

```bash
nix develop -c <command>
```

**Exception:** `git` commands do not need the Nix prefix — run them directly (`git status`, `git diff`, `git commit`).

Useful focused checks:

| Area | Command |
|------|---------|
| All tests + coverage | `nix develop -c make` |
| Full pytest suite | `nix develop -c python -m pytest` |
| Core analyzer | `nix develop -c python -m pytest test/test_analyzer.py test/testOutput.py` |
| Language reader | `nix develop -c python -m pytest test/test_languages/test<Lang>.py` |
| Extension | `nix develop -c python -m pytest test/test_extensions/test<Name>.py` |
| CLI / options | `nix develop -c python -m pytest test/testApplication.py test/test_options.py` |
| Style | `nix develop -c make pep8` or `nix develop -c make pylint` |
| Style before commit | `./scripts/run.sh make format-changed` (format-changed skill; script owns component mapping) |
| Lint staged | `./scripts/run.sh make lint-changed` |

## Rules

- Development & tests: `.cursor/rules/basic-development.mdc`
- Problem, story, and execution-leaf splits: `.cursor/rules/problem-decomposition.mdc`
- Planning artifacts and lifecycle: `.cursor/rules/planning.mdc`
- GSD vs local wrap-up: `.cursor/rules/gsd-coexistence.mdc`
- Adding / modifying language support: `.cursor/rules/lizard-rule.mdc`
- Fixing issues (test-first workflow): `.cursor/rules/issue.mdc`
- ADRs: `.cursor/rules/architecture-decisions.mdc`

## Skills

| Skill | When |
|-------|------|
| **story-decomposition** | Broad or unclear requirements; ordered 3V candidate stories in one seed |
| **slice-planning** | Turn one selected story into Behavior/Structure leaves |
| **slice-plan-refinement** | Edit an existing PLAN in place when leaves are complex, low-confidence, or overrun |
| **execute-plan** | Run a plan under `.planning/` with per-slice wrap-up |
| **post-change-refactor** | Concept-bounded cleanup before commit (coordinator-owned) |
| **format-changed** | Fresh wrap-up agent: run selective pep8 on affected working-tree components before staging |
| **adr-awareness** | Load / cite / conflict-check Accepted ADRs |

## Architectural decisions (ADRs)

- Human propose / discuss / approve: `docs/adrs/README.md`
- Current recommendations: `docs/adrs/*-accepted.md` (read explicitly)
- Agent use / cite / conflict / maintain: `.cursor/skills/adr-awareness/SKILL.md`

## Planning modes (GSD vs local)

| Mode | Artifacts | Orchestrator |
|------|-----------|--------------|
| Story shaping | `.planning/seeds/SEED-NNN-slug.md` containing ordered candidate stories | **story-decomposition** |
| Formal milestone | `.planning/phases/NN-slug/*-PLAN.md`, STATE, ROADMAP | `/gsd-plan-phase` → `/gsd-execute-phase` → `/gsd-ship` (+ local wrap-up) |
| Ad-hoc | `.planning/quick/NNN-slug/PLAN.md` | **slice-planning** + **execute-plan** |
| Optional refinement | Existing phase/quick PLAN; no new artifact | **slice-plan-refinement** |
| Legacy | `ongoing/*.md` | **execute-plan** only; do not migrate |

Story-decomposition seeds are not executable: select one contained story, then
use slice-planning. Run slice-plan-refinement only when the resulting PLAN is
complex, sizing confidence is low, or execution overruns; straightforward plans
may execute directly. **Hard decomposition quality:** one evaluable outcome at the
current resolution; 3V stories; Behavior/Structure execution leaves —
`problem-decomposition.mdc`. Plan artifact and lifecycle rules: `planning.mdc`.
Do not write new flat `.planning/<name>.md` when `phases/` or `quick/` fits.
**Per-slice wrap-up:** Jidoka → post-change-refactor → fresh **format-changed**
agent → full pytest → update plan → commit → push (**execute-plan**). The
pre-commit hook lints staged components without mutation. Skills emit completion
markers (e.g. `## REFACTOR COMPLETE`) for handoff.

No `.planning/` yet → justification for retained code comes only from the current uncommitted change.

## Test Style

Prefer end-to-end observable behavior via `analyze_file.analyze_source_code`, `analyze_file` integration tests, and CLI output tests (`testApplication.py`). Do not mock parsing logic; mock only filesystem boundaries when needed. See `basic-development.mdc`.
