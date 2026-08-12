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

Lizard measures **cyclomatic complexity (CCN)**, **NLOC** (non-comment lines of code), **function length**, **nesting depth**, and related metrics across many languages. Parsing uses **token generators** and **state machines** (`CodeStateMachine`, `CLikeStates`, language-specific readers). Prefer capability names (reader, token, CCN, extension) over phase or ticket numbers in product code.

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

## Rules

- Development & tests: `.cursor/rules/basic-development.mdc`
- Adding / modifying language support: `.cursor/rules/lizard-rule.mdc`
- Fixing issues (test-first workflow): `.cursor/rules/issue.mdc`

## Planning

No active GSD milestone by default. If `.planning/` exists, read `STATE.md` and the active phase dir for the immediate next unit. Otherwise justification for retained code comes only from the current uncommitted change.

## Test Style

Prefer end-to-end observable behavior via `analyze_file.analyze_source_code`, `analyze_file` integration tests, and CLI output tests (`testApplication.py`). Do not mock parsing logic; mock only filesystem boundaries when needed. See `basic-development.mdc`.
