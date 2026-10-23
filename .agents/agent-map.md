# Agent Map

Short navigation index for the Lizard repository.

## Work areas

- **Core analyzer and CLI:** `lizard_core/` is the core analyzer's home:
  `file_discovery.py` (finding source files, `.gitignore`, duplicates),
  `function_info.py` (function, file and result data model),
  `file_info_builder.py` (building context readers drive),
  `file_analyzer.py` (per-file analysis and built-in token processors),
  `extensions.py` (extension loading), `analysis.py` (batch `analyze`),
  `warning_filter.py` (thresholds and whitelist), `output_scheme.py` (column
  schema), `text_output.py` (default and warning printers), `options.py`
  (option table and resolution), `cli.py` (`main`). `lizard.py` is the public
  facade: it re-exports those names and is the script and console entry point.
  `lizard_core` modules never import `lizard`.
- **Language readers:** `lizard_languages/` — one module per language, with
  shared bases in `code_reader.py`, `clike.py`, `golike.py`, and `rubylike.py`.
  Registration is in `lizard_languages/__init__.py`.
- **Extensions:** `lizard_ext/` — output formatters, metric extensions, and
  `extension_base.py`.
- **Tests:** `test/` — language, extension, CLI, option, and analyzer coverage.
- **Planning records:** `.planning/` — the product backlog, seeds under
  `seeds/`, and executable slice plans under
  `slice-plans/<NNN>-<slug>/PLAN.md`, numbered from `001` in allocation order.

## Domain language

Lizard measures CCN, NLOC, token count, parameter count, and nesting depth.
Parsing uses token generators, state machines, and language readers. Prefer
capability names over phase or ticket numbers in product code.

## Commands

Run repository tooling through Nix. Git commands are the exception.

| Area | Command |
|------|---------|
| All checks | `nix develop -c make` |
| Full pytest suite | `nix develop -c python -m pytest` |
| Core analyzer (`lizard_core/`) | `nix develop -c python -m pytest test/test_analyzer.py test/testOutput.py` |
| Language reader | `nix develop -c python -m pytest test/test_languages/test<Lang>.py` |
| Extension | `nix develop -c python -m pytest test/test_extensions/test<Name>.py` |
| CLI and options | `nix develop -c python -m pytest test/testApplication.py test/test_options.py` |
| Style | `nix develop -c make pep8` or `nix develop -c make pylint` |

## Project rules

- Development and tests: `AGENTS.md`
- Adding or modifying language support: `.agents/skills/lizard-language-support/SKILL.md`

Open Dough lifecycle skills are installed under `.agents/skills/dough-*` and
discovered directly by supported agents. This repository does not maintain a
second lifecycle-skill catalog or implementation.

## Test style

Prefer observable behavior through `analyze_file.analyze_source_code`,
`analyze_file`, and CLI output. Do not mock parsing logic; mock only filesystem
boundaries when necessary.
