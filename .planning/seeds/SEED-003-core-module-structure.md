---
id: SEED-003
status: active
planted: 2026-10-23
planted_during: Backlog request to restructure the core analyzer module
trigger_when: A maintainer changes analysis, CLI, file discovery, or output code in lizard.py
scope: large
---

# SEED-003: Core module structure

`lizard.py` is the most important source file in the project and has grown to
about 1,200 lines. It mixes several responsibilities in one module: the public
analysis API (`analyze`, `analyze_file`), CLI option parsing, the analysis
data model (`FunctionInfo`, `FileInformation`, nesting), the file-info
builder and token-stream processors, file discovery with gitignore handling,
warning filtering and whitelists, output schemes and printers, extension
loading, and `main`. This project's refactoring standard keeps source files
below 250 lines.

<a id="core-module-architecture"></a>

### Change the core analyzer through modules shaped by its architecture

**Identity:** SEED-003#core-module-architecture
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/002-core-module-architecture/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"992dcc2ed6ce22cdf417594a80c9e7bddb50920cc5b893d9d241f4de7b0e825a","plan":"9873c2d2acd0af253b14a1a06f5de350a183fcc4529d88cd73e7131735d58616"}}
```

**Goal:** A maintainer or contributor changing how Lizard analyzes a file,
discovers files, decides warnings, prints results, or parses options finds
that responsibility in one module of a `lizard_core` package whose boundaries
follow the analyzer's architecture, while `lizard.py` stays the stable public
entry point. The change makes the most-changed file of the project cheaper
and safer to maintain, fix, and review, which is the backlog's near-future
direction.

**Scope:**

Required:

- A new top-level package `lizard_core/`, a sibling of `lizard_ext/` and
  `lizard_languages/`, holds the core analyzer as one module per
  responsibility named in **Architecture** below. It is listed in `setup.py`
  `packages` and in the `Makefile` pep8, pylint, and coverage targets.
- `lizard.py` remains the one supported public module: `py_modules=['lizard']`,
  the console entry `lizard = lizard:main`, and `python lizard.py` script mode
  including the `sys.path` insertion for embeddable Python (issue #460). Every
  name that `import lizard` exposes today, apart from the standard-library
  modules it happens to import, stays importable from `lizard` with the same
  behavior. The names extensions and external users rely on are among them:
  `main`, `analyze`, `analyze_files`, `analyze_file`, `FunctionInfo`,
  `FileInformation`, `FileInfoBuilder`, `FileAnalyzer`, `get_extensions`,
  `get_warnings`, `OutputScheme`, `parse_args`, and `arg_parser`.
- Observable behavior is unchanged: analysis results from `analyze`,
  `analyze_files`, and `analyze_file.analyze_source_code`; CLI stdout for the
  default, `-w` (clang-style), `--msvs`, `-X`, `--csv`, `-H`, and
  `.checkstyle.xml` outputs; stderr messages; exit codes; option parsing
  including options that extensions add; `-t` threads; `.gitignore` and
  `-x` exclusion; whitelist and `#lizard forgives` handling.
- Extensions under `lizard_ext/` keep importing from `lizard` as they do now.
- Module and package names follow the vocabulary in
  `docs/adrs/0001-ubiquitous-language-accepted.md`.
- `nix develop -c make` is green at every slice boundary, and the built wheel
  contains the new package.
- Where the most cohesive structure leaves a module above 250 lines, the
  module stays whole and the plan records what remains for a later story.

Deferred promises (not built or verified by this story):

- No behavior change, option, metric, or output format, and no new public
  API beyond re-exporting what `lizard` already exposes.
- `lizard_core` is internal. Its module paths are not a supported import
  surface, so no deprecation shims or stability promise are made for them;
  the package docstring says so.
- Test patch targets such as `patch('lizard.auto_read')` are not a public
  promise. Tests that patch a filesystem boundary move the patch to the
  module that now looks the name up.
- `lizard_ext/` and `lizard_languages/` are not reorganized. The default text
  printer stays in the core.
- The pylint `max-module-lines` setting is left as it is.
- Writing an ADR is not part of this story; see **Architecture**.

Boundary assumptions:

- Supported interpreters are Python 3.8 and above (`setup.py` classifiers,
  CI matrix). Dead Python 2 branches met while moving code may be dropped
  instead of copied; this changes nothing on a supported interpreter.
- Private names (`_map_in_pool`, `_extension_arg`, `_build_gitignore_spec`,
  `_script_dirs`) are re-exported for safety but are not promised.

**Key examples:**

1. A maintainer wants to change how `.gitignore` filtering works → opens
   `lizard_core/file_discovery.py` → finds `get_all_source_files`, the
   gitignore spec builder, and the md5 duplicate check there, and nothing
   about options or output; `lizard.py` no longer defines them.
2. An external script runs `import lizard` and
   `lizard.analyze_file.analyze_source_code("a.cpp", "int foo(){ if(a) b; }")`
   → one function `foo` with CCN 2, NLOC 1, as before the change.
3. A clean checkout at the slice's starting commit → `lizard lizard_languages`
   and `lizard -C 3 -w lizard_languages/python.py` and
   `lizard --csv lizard_ext` are run before and after the slice → stdout and
   exit codes are byte-identical.
4. The nesting-depth extension does `from lizard import FileInfoBuilder,
   FunctionInfo` → `lizard -END lizard_languages/clike.py` → prints the ND
   column with the same values as before.
5. `python -m build --wheel` → the wheel lists `lizard.py`, `lizard_core/`,
   `lizard_ext/`, and `lizard_languages/`, and running the installed `lizard`
   console script analyzes a file.
6. A shell in another directory runs `python /path/to/lizard.py some.c` →
   the file is analyzed, because the script directory insertion still finds
   `lizard_core` beside `lizard.py`.
7. The option table in `arg_parser` is about 170 lines of one cohesive unit
   → the options module ends up near or above 250 lines → it stays whole, and
   the plan's learnings record it instead of splitting it.

**Architecture:**

The core analyzer is a pipeline, and the package follows it. Dependency
direction is `lizard.py` → `lizard_core.*` → `lizard_languages` and the
`lizard_ext` glue (`auto_read`, `auto_open`, `version`, the XML, CSV, HTML,
and checkstyle printers). No `lizard_core` module imports `lizard`, so the
facade can import the package without a cycle; extension modules keep their
lazy `from lizard import ...` as today.

| Module in `lizard_core/` | Responsibility (ADR-0001 terms) | Moves from `lizard.py` |
| --- | --- | --- |
| `function_info.py` | Analysis data model: **Function** and file results | `Nesting`, `BARE_NESTING`, `Namespace`, `FunctionInfo`, `FileInformation`, `AllResult` |
| `file_info_builder.py` | The building context language readers drive | `NestingStack`, `FileInfoBuilder` |
| `file_analyzer.py` | Per-file analysis: built-in **token** processors incl. **forgive** comments, and **FileAnalyzer** | `preprocessing`, `comment_counter`, `line_counter`, `token_counter`, `condition_counter`, `FileAnalyzer` |
| `extensions.py` | **Extension** loading and ordering | `get_extensions`, `print_extension_results` |
| `analysis.py` | Batch **analyze** entry points and worker mapping | `analyze`, `analyze_files`, `map_files_to_analyzer`, `_map_in_pool`, the `analyze_file` instance |
| `file_discovery.py` | Finding source files: language support, exclusion, `.gitignore`, duplicates | `get_all_source_files`, `_build_gitignore_spec`, `md5_hash_file` |
| `warning_filter.py` | **Warning** policy: thresholds, forgiven metrics, **whitelist** | `warning_filter`, `whitelist_filter`, `get_warnings`, `DEFAULT_WHITELIST` |
| `output_scheme.py` | Column schema shared by every printer and extension | `OutputScheme` |
| `text_output.py` | Default text printers and warning styles | `print_result`, `print_and_save_modules`, `print_warnings`, `print_no_warnings`, `print_total`, `silent_printer`, `print_clang_style_warning`, `print_msvs_style_warning` |
| `options.py` | CLI option definitions and resolution | `arg_parser`, `_extension_arg`, `parse_args`, `infer_printer_from_file_ext`, `DEFAULT_CCN_THRESHOLD`, `DEFAULT_MAX_FUNC_LENGTH` |
| `cli.py` | Command entrance and output-file handling | `main`, `open_output_file` |

`lizard.py` keeps the license header, module docstring, `_script_dirs` and
the `sys.path` insertion, the import guards with their messages and exit
code, explicit re-export imports (no `import *`), and the `__main__` guard.
Expected sizes from today's line counts: every module under 250 lines except
`options.py`, which lands near 250 because `arg_parser` is one option table;
the story's rule keeps it whole.

ADR: this layout is cross-cutting and will be re-asked ("where does core code
live, what is the supported import surface?"), so it meets the criteria in
`docs/adrs/README.md` for an ADR. Drafting and accepting one is human-owned
(ADR-0000, decision 3); this refinement recommends that the developer record
it as ADR-0002 "Core analyzer package and public facade", stating the facade
rule and the dependency direction above. Execution does not wait for it.
