# Plan: Change the core analyzer through modules shaped by its architecture

**Identity:** SEED-003#core-module-architecture

**Source:** [Change the core analyzer through modules shaped by its architecture](../../seeds/SEED-003-core-module-structure.md#core-module-architecture)

## Goal and scope

A maintainer changing how Lizard analyzes a file, discovers files, decides
warnings, prints results, or parses options finds that responsibility in one
module of a new `lizard_core` package, while `lizard.py` stays the public
entry point with every name it exposes today.

Included: the `lizard_core/` package with the eleven modules the story's
**Architecture** table names; `lizard.py` reduced to the script-directory
`sys.path` insertion, import guards, explicit re-exports, and the `__main__`
guard; `setup.py` `packages` and the `Makefile` pep8, pylint, and coverage
targets extended with the package; test patch targets moved to the module that
now looks the patched name up; `.agents/agent-map.md` updated to name the
package as the core analyzer's home.

Excluded: any behavior, option, metric, or output change; reorganizing
`lizard_ext/` or `lizard_languages/`; shims or stability promises for
`lizard_core` module paths; changing pylint's `max-module-lines`; writing the
recommended ADR (developer-owned, see the seed's **Architecture**).

Assumptions: Python 3.8+ only, so dead Python 2 branches met while moving code
are dropped rather than copied. The promised public names are the 56 names
`import lizard` exposes at the starting commit excluding `division`,
`print_function`, `fnmatch`, and the loop variable `_script_dir` (see
**Decisive premises**). Every slice ends in a committed state where the full
suite and `nix develop -c make` are green, as `AGENTS.md` requires before
completing a change.

## Outside-in proof

| Promise (key example) | Owning slice | Observable proof |
| --- | --- | --- |
| File discovery lives in `lizard_core/file_discovery.py` and `lizard.py` no longer defines it (ex. 1) | 1 | `grep -cE '^def (get_all_source_files|md5_hash_file|_build_gitignore_spec)' lizard.py` prints 0; `nix develop -c python -c "import lizard; print(lizard.get_all_source_files.__module__)"` prints `lizard_core.file_discovery` |
| Packaging and checks include the package (ex. 5) | 1, 6 | `nix develop -c python -m build --wheel --no-isolation --outdir $CLAUDE_JOB_DIR/tmp/dist` then listing the wheel's top-level entries shows `lizard_core/`; `nix develop -c make` green with `lizard_core` in the pep8, pylint, and coverage lists |
| Script mode still finds the package beside `lizard.py` (ex. 6) | 1, 6 | From another directory, `<workspace>/.venv/bin/python <workspace>/lizard.py <workspace>/lizard_ext/auto_open.py` prints the same total line as at the starting commit (`17 7.0 2.0 67.5 2 0 0.00 0.00`) |
| Public API results unchanged (ex. 2) | 2, 3 | `nix develop -c python -c "import lizard; f = lizard.analyze_file.analyze_source_code('a.cpp', 'int foo(){ if(a) b; }').function_list[0]; print(f.name, f.cyclomatic_complexity, f.nloc)"` prints `foo 2 1`; full suite green |
| Extensions importing from `lizard` keep working (ex. 4) | 2, 3 | `lizard -END lizard_languages/clike.py` output before and after the slice is byte-identical |
| CLI stdout and exit codes unchanged (ex. 3) | every slice | At the slice's starting commit capture `lizard lizard_languages`, `lizard -C 3 -w lizard_languages/python.py`, `lizard --msvs -C 3 lizard_languages/python.py`, `lizard --csv lizard_ext`, `lizard -X lizard_ext`, `lizard -H lizard_ext/auto_open.py`, `lizard -C 1 -W nonexist.txt lizard_ext/version.py` (stdout and `$?` each) into `$CLAUDE_JOB_DIR/tmp/before/`; after the slice's edits run the same into `after/`; `diff -r before after` prints nothing |
| Every name `import lizard` exposed before is still exposed with the same behavior | 6 | `nix develop -c python -c "import lizard; missing = set('''<the 56 names>'''.split()) - set(dir(lizard)); print(sorted(missing))"` prints `[]`; full suite green |
| Modules follow the architecture and the 250-line rule, or the remainder is recorded (ex. 7) | 6 | `wc -l lizard.py lizard_core/*.py` shows every file under 250 lines except any the **Learnings** section records with its reason |
| Repository navigation names the new home | 6 | `grep -n lizard_core .agents/agent-map.md` prints the core analyzer entry |

Patch-target consumers (settled below): tests that patch `lizard.auto_read`,
`lizard.auto_open`, `lizard.md5_hash_file`, or `lizard.print_result` are moved
to the consuming module in the slice that moves the consumer, and the full
suite is the proof that the patch still takes effect.

## Decisive premises

| Premise | Consuming operation | Observation | Result |
| --- | --- | --- | --- |
| The suite is green at the starting commit | Every slice's "suite stays green" proof | `nix develop -c python -m pytest -q` in this workspace at `e81f002` | `1651 passed, 7 skipped in 2.75s`; settled |
| Moving a function out of `lizard.py` breaks tests that patch its collaborators on `lizard` | Slice 1's test repointing and sizing | Scratch move of `get_all_source_files`, `_build_gitignore_spec`, `md5_hash_file` into `lizard_core/file_discovery.py` with a facade re-export, then `pytest test/testFilesFilter.py test/testApplication.py ...`; reverted | 4 failures, all in `test/testFilesFilter.py` (`test_duplicates`, `test_gitignore_filter`, `test_tolerates_invalid_windows_gitignore_pattern`, `test_valid_gitignore_patterns_still_apply_with_invalid_lines`); the other patch-decorated tests passed because their assertions do not reach the moved lookups; settled |
| `python lizard.py` from another directory still imports a sibling package | Slice 1's script-mode proof | In the scratch state, `cd $CLAUDE_JOB_DIR/tmp && <workspace>/.venv/bin/python <workspace>/lizard.py <workspace>/lizard_ext/auto_open.py` | Printed the same total line as the unmodified checkout (`17 7.0 2.0 67.5 2 0 0.00 0.00`); settled |
| Importing an extension module before `lizard` does not cycle once core code sits in `lizard_core` | Dependency direction in every slice | Scratch state: `python -c "import lizard_ext.lizardnd, lizard; print(lizard.get_all_source_files.__module__)"` | `lizard_core.file_discovery`; settled |
| A wheel can be built offline here and its content follows `setup.py` | Slice 1 and 6 packaging proof | `nix develop -c python -m build --wheel --no-isolation --outdir $CLAUDE_JOB_DIR/tmp/dist` at the starting commit | Built `lizard-1.24.2-py2.py3-none-any.whl` containing `lizard.py`, `lizard_ext`, `lizard_languages`; `build/` and `lizard.egg-info` are gitignored leftovers to remove; settled |
| `import lizard` exposes a fixed set of non-module names at the starting commit | The public-name proof of slice 6 | `python -c` listing `dir(lizard)` without dunder and module entries | 60 names: `AllResult BARE_NESTING CLikeReader DEFAULT_CCN_THRESHOLD DEFAULT_MAX_FUNC_LENGTH DEFAULT_WHITELIST FileAnalyzer FileInfoBuilder FileInformation FunctionInfo Namespace Nesting NestingStack OutputScheme _build_gitignore_spec _extension_arg _map_in_pool _script_dir _script_dirs analyze analyze_file analyze_files arg_parser auto_open auto_read comment_counter condition_counter division fnmatch get_all_source_files get_extensions get_reader_for get_warnings html_output infer_printer_from_file_ext languages line_counter main map_files_to_analyzer md5_hash_file open_output_file parse_args preprocessing print_and_save_modules print_checkstyle print_clang_style_warning print_csv print_extension_results print_function print_msvs_style_warning print_no_warnings print_result print_total print_warnings print_xml silent_printer token_counter version warning_filter whitelist_filter`; the promised 56 exclude `division`, `print_function`, `fnmatch`, `_script_dir`; settled |
| `analyze_source_code('a.cpp', 'int foo(){ if(a) b; }')` yields `foo` with CCN 2, NLOC 1 today | Key example 2's expected values | `python -c` at the starting commit | `foo 2 1`; settled |

## Slices

Every slice: move the named definitions verbatim into their module with the
imports they need (`lizard_languages` and `lizard_ext` glue only, never
`lizard`), replace them in `lizard.py` with an explicit re-export import,
repoint the test patches whose patched name the moved code looks up, run the
full suite and the CLI comparison, then `nix develop -c make`, and commit.
The slice order has no backward dependency: each module imports only modules
that earlier slices created or that already exist.

### 1. File discovery lives in `lizard_core/file_discovery.py`, and the package is packaged and checked
Type: Behavior
Status: done
Proof: full suite green; `grep -c` on `lizard.py` prints 0 for the three definitions; `__module__` check prints `lizard_core.file_discovery`; wheel lists `lizard_core/`; script mode from another directory prints the baseline total line; CLI comparison empty; `nix develop -c make` green.

Behavior: `lizard.py` defines `get_all_source_files`, `_build_gitignore_spec`,
and `md5_hash_file` → the package `lizard_core/` is created with a docstring
stating it is internal, the three definitions move to
`lizard_core/file_discovery.py`, `setup.py` lists `lizard_core` in `packages`,
the `Makefile` pep8, pylint, and coverage `--source` lists gain `lizard_core`
(the coverage list's existing `lizard_language` misspelling is corrected in
the same list), and `test/testFilesFilter.py` and `test/testApplication.py`
patch `lizard_core.file_discovery.auto_read`, `.auto_open`, and
`.md5_hash_file` where the moved code looks them up → a maintainer opens
`file_discovery.py` and finds only discovery; `lizard` still exposes the three
names; CLI output, the wheel, and script mode behave as before.

### 2. The analysis data model and builder live in `function_info.py` and `file_info_builder.py`
Type: Behavior
Status: done
Proof: full suite green; key example 2 prints `foo 2 1`; `lizard -END lizard_languages/clike.py` byte-identical before and after; CLI comparison empty; `nix develop -c make` green.

Behavior: `lizard.py` defines `Nesting`, `BARE_NESTING`, `Namespace`,
`FunctionInfo`, `FileInformation`, `AllResult`, `NestingStack`, and
`FileInfoBuilder` → the first six move to `lizard_core/function_info.py` and
the last two to `lizard_core/file_info_builder.py`, re-exported from `lizard`
→ `lizard_ext/lizardnd.py`, `lizardns.py`, `lizardhalstead.py`, and
`lizardcognitive.py` keep importing them from `lizard`, `OutputScheme`'s
class patching still reaches the same `FileInformation` class, and results
are unchanged.

### 3. Per-file analysis, extension loading, and batch analysis live in `file_analyzer.py`, `extensions.py`, and `analysis.py`
Type: Behavior
Status: done
Proof: full suite green, including the threaded tests; `lizard -t 2 lizard_languages` exits 0 and prints the same `N file analyzed.` line as `-t 1`; `-END` comparison byte-identical; CLI comparison empty; `nix develop -c make` green.

Behavior: `lizard.py` defines the token processors, `FileAnalyzer`,
`get_extensions`, `print_extension_results`, `analyze`, `analyze_files`,
`map_files_to_analyzer`, `_map_in_pool`, and the `analyze_file` instance →
processors and `FileAnalyzer` move to `lizard_core/file_analyzer.py`,
`get_extensions` and `print_extension_results` to `lizard_core/extensions.py`,
the batch functions and the `analyze_file` instance to
`lizard_core/analysis.py`, all re-exported → `from lizard import analyze_file`
yields the same single instance, `-E` extensions still load through
`importlib` and import `lizard` lazily, worker processes still pickle the
analyzer by module path, and results are unchanged.

### 4. Warning policy lives in `warning_filter.py`
Type: Behavior
Status: done
Proof: full suite green; `lizard -C 1 -W nonexist.txt lizard_ext/version.py` stdout (including the two `WARNING:` lines) and exit code identical before and after; CLI comparison empty; `nix develop -c make` green.

Behavior: `lizard.py` defines `warning_filter`, `whitelist_filter`,
`get_warnings`, and `DEFAULT_WHITELIST` → they move to
`lizard_core/warning_filter.py`, re-exported, and `test/testOutput.py`'s
`check_whitelist` patches `lizard_core.warning_filter.auto_read` →
`lizard_ext/htmloutput.py` still imports `get_warnings` from `lizard`, and
threshold, forgiven-metric, and whitelist handling are unchanged.

### 5. The output schema and default printers live in `output_scheme.py` and `text_output.py`
Type: Behavior
Status: done
Proof: full suite green; CLI comparison empty for default, `-w`, `--msvs`, `--csv`, `-X`, `-H`; `nix develop -c make` green.

Behavior: `lizard.py` defines `OutputScheme` and the eight print functions →
`OutputScheme` moves to `lizard_core/output_scheme.py` and the printers to
`lizard_core/text_output.py`, re-exported; `main` stays in `lizard.py` for
this slice and imports `print_result` through the facade, so
`test/testApplication.py`'s `patch.object(lizard, 'print_result')` still
takes effect → every output style prints as before.

### 6. Options and the command entrance live in `options.py` and `cli.py`; `lizard.py` is the public facade
Type: Behavior
Status: done
Proof: full suite green; the 56-name check prints `[]`; wheel lists `lizard_core/`; script mode from another directory prints the baseline total line; `wc -l` shows every file under 250 lines except any recorded in **Learnings**; `grep -n lizard_core .agents/agent-map.md` prints the entry; CLI comparison empty; `nix develop -c make` green.

Behavior: `lizard.py` defines `arg_parser`, `_extension_arg`, `parse_args`,
`infer_printer_from_file_ext`, `DEFAULT_CCN_THRESHOLD`,
`DEFAULT_MAX_FUNC_LENGTH`, `main`, and `open_output_file` → the first six
move to `lizard_core/options.py` and the last two to `lizard_core/cli.py`,
`test/testApplication.py` patches `lizard_core.cli.print_result` and
`lizard_core.cli.auto_read` where `main` looks them up, `lizard.py` keeps
only the header, docstring, `_script_dirs` with the `sys.path` insertion, the
import guards with their messages and exit code, explicit re-exports of the
56 names, and `if __name__ == "__main__": sys.exit(main())`, and
`.agents/agent-map.md` names `lizard_core/` as the core analyzer's home with
`lizard.py` as the facade → `lizard`, `python lizard.py`, and
`import lizard` behave exactly as before, and each responsibility is found
in one module.

## Current decisions

- Existing solutions (PFE): every moved responsibility already lives in
  `lizard.py`; no other module owns one of them. `lizard_ext/` holds plugin
  output formats and `auto_open`, `lizard_languages/` holds readers, so a new
  sibling package is the only domain-correct home. The default text printer
  and `OutputScheme` stay in the core because every printer and extension
  consumes the schema and the CLI needs the text printer with no extension
  loaded; the `print_xml`/`print_csv`/`print_checkstyle` glue in
  `lizard_ext/__init__.py` is left where it is.
- `lizard` stays a module (`py_modules=['lizard']`), not a package, so the
  console entry, script mode, and `setup.py` are untouched apart from the
  added package.
- Dependency rule: no `lizard_core` module imports `lizard`; the facade
  imports the package. Extension modules keep their lazy `from lizard import`.
- Tests patch a filesystem boundary at the module that looks the name up.
  Patch targets on `lizard` are not a public promise.
- ADR-0001 (`docs/adrs/0001-ubiquitous-language-accepted.md`) supplies the
  module vocabulary. Hygiene mismatch for the developer: its in-file status
  reads `Proposed` while the filename and the README index say Accepted; not
  changed here.
- Local gate: `AGENTS.md` requires the full suite before completing a change
  and `nix develop -c make` for all checks; the suite runs in under three
  seconds, so both run at every slice boundary.
- Proof uses the full suite, never a subset: `test/testOutput.py::
  TestFileInformationOutput::test_print_and_save_detail_information_with_ext`
  fails on the clean tree when only six test files run in that order, because
  it relies on an earlier test having patched `FileInformation` through
  `OutputScheme.patch_for_extensions`.

## Learnings

Record here any module left above 250 lines and why (the seed expects
`options.py` to land near 250 because `arg_parser` is one option table), and
the test-order dependency above if execution confirms it is worth a follow-up.

- CLI comparison corrections: `--msvs` is not an option; the msvs style is
  `--warning-msvs`. `-t 2` output order is nondeterministic and is compared
  sorted; the `-H` report's generation time is normalized. The coordinator's
  capture also writes `.checkstyle.xml` through `-o` and observes extension
  compatibility through `-END`, `-Ens`, `-Ecognitive`, and `-Ehalstead` on
  `lizard_languages/clike.py`.
- The CLI comparison runs `lizard.main` through the module path, as the
  installed `lizard` console script does, not `python lizard.py`. In script
  mode the file runs as `__main__`, and an extension's `from lizard import`
  loads a second copy whose classes the extension patches; at the slice 1 and
  slice 2 starting commits `python lizard.py -END ...` therefore failed with
  `AttributeError: 'NestingStack' object has no attribute
  'set_in_condition'` while `lizard -END ...` printed ND values. Once a
  patched class lives in `lizard_core`, both copies share it, so script mode
  now matches the console script. From slice 2 on the comparison also checks
  that script-mode `-END`, `-Ens`, `-Ecognitive`, and `-Ehalstead` output
  and exit codes equal the module path's. Later slices that move a class or
  function an extension patches converge script mode the same way.
- Patch consumers are wider than the plan's named test files:
  `test/test_extensions/testCognitiveComplexity.py` also patched
  `lizard.md5_hash_file`. Trace each moved name across `test/` with the
  consumer-trace script rather than relying on the slice text.
- Tests that run `lizard.main` patch each filesystem read where it is looked
  up: `.gitignore` at `lizard_core.file_discovery.auto_read`, sources at
  `lizard_core.file_analyzer.auto_read`, and the whitelist at
  `lizard.auto_read` (slice 3). When slice 4 moves `whitelist_filter`, repoint
  the whitelist patches in `test/testApplication.py` (class decorator and
  `run_with_mocks`), `test/test_extensions/testCognitiveComplexity.py`
  (`TestCommandLine`), and `test/testOutput.py` (`check_whitelist`). Slice 4
  did so. Follow-up candidate (pre-existing, not caused by this story):
  `test/testOutput.py` `TestWarningOutput` tests `test_should_use_clang_format_for_warning`,
  `test_warning_when_max_nesting_depth_missing`, and `test_sort_warning`
  read a real `whitelizard.txt` from the working directory; patching
  `lizard_core.warning_filter.auto_read` in its `setUp` would isolate them.
- Final sizes (slice 6): `lizard.py` 97 lines; every `lizard_core` module
  under 250 lines except `options.py` (272), kept whole because
  `arg_parser` is one option table of about 185 lines (key example 7).
  Splitting the option table, for example by option group, is what remains
  for a later story if one is wanted.
- `arg_parser` used `lizard.py`'s `__doc__` as the `--help` description.
  Because `lizard_core` never imports `lizard`, `options.DESCRIPTION` copies
  that text, and `test/test_options.py`
  `test_help_description_is_the_lizard_module_docstring` keeps the two equal.
- ADR-0001 (`docs/adrs/0001-ubiquitous-language-accepted.md`), **Extension**
  entry, says extensions are "Wired from `lizard.py`"; they are now wired from
  `lizard_core/extensions.py` and `options.py`. ADRs are developer-owned, so
  this is left for the developer, for example with the recommended ADR-0002.

## Story obligations

### G1. `-END` crashes on every input before and after the move
Reported: slice 1 — "At the starting commit `lizard -END lizard_languages/clike.py` prints the header and exits 1 with AttributeError: 'NestingStack' object has no attribute 'set_in_condition' from lizard_ext/lizardnd.py; a small C file fails the same way."
Story clause: "prints the ND column with the same values as before"
Disposition: proved by slice 2: slice 1 ran script mode, not the `lizard` command the key example names; the module path `lizard.main(['lizard', '-END', 'lizard_languages/clike.py'])` exits 0 with ND values at the slice 2 starting commit `269bd9e` and is byte-identical after slice 2 (coordinator capture `before-s2m` vs `after-s2m`, `cli-compare.sh` identical), and script mode now prints the same output and exit code 0

## Concern evidence

Slice-plan refinement was not needed: each slice moves one cohesive set of
definitions with one proof loop (full suite plus CLI comparison), the order
has no backward dependency, and every boundary is a committed green state
with the facade keeping the public surface whole. This review identified no
remaining slice-specific concern.
