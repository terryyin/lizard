# Show a compact JavaScript complexity report

**Identity:** SEED-030#compact-callback-report
**Source:** [selected story](../../seeds/SEED-030-compact-javascript-callback-report.md#compact-callback-report)
**Source review:** [source-review.md](source-review.md)

## Goal and scope

Add the opt-in `--compact` option to the default text report so its function
table leaves out rows for anonymous nested CCN 1 callbacks that are not
warnings. Measurements, file/total summaries, the warning section, exit status,
the default report and every other formatter stay unchanged. Exclusions are the
story's: no complexity merging into the enclosing function, no omitted-row
marker, no compact behavior for other formatters, no language filter.

## Context and current decisions

- Observed in this checkout (probe through `analyze_file.analyze_source_code`
  wrapping `FileInfoBuilder.push_new_function`): when the JavaScript reader
  pushes a nested function, `current_function` is the enclosing function
  (`sum`, `f`); for a top-level `[1, 2].forEach(x => ...);` it is the
  `*global*` pseudo function. Recording "defined inside a function" on the
  pushed `FunctionInfo` at that point is enough; no reader change is needed.
  Note: without a terminating `;`, a following `[...]` line continues the
  previous arrow body in JS and is correctly attributed to it, so examples use
  explicit semicolons.
- The function table is printed in `lizard_core/text_output.py`
  `print_and_save_modules` before `get_warnings` runs. "Not a warning" uses the
  same policy as the warning section: `lizard_core/warning_filter.py`
  thresholds, forgiven metrics and whitelist. Reuse that policy per function;
  do not duplicate threshold logic.
- `FunctionInfo` crosses worker processes with `-t`; the new attribute must be
  a plain picklable value.
- The option belongs in `lizard_core/options.py` (long-only `--compact`) and is
  documented in `docs/cli.rst`'s help listing.
- ADR 0001 (docs/adrs/0001-ubiquitous-language-accepted.md): name the concept
  in domain terms (compact report, nested anonymous function), not
  issue/plan numbers. No other ADR applies.

## Outside-in proof

Real CLI tests in `test/testCompactReport.py` (subprocess against `lizard.py` on
a temporary `.js` file, like `TestVueSourceLineNumbersCli`), observing stdout
and the exit code:

1. `const sum = array => array.reduce((a, b) => a + b);` with `--compact`: one
   function row (`sum`, CCN 1); file function_cnt and total Fun Cnt are 2.
   Without `--compact`: both rows.
2. Ternary reducer: its CCN 2 `(anonymous)` row remains with `--compact`.
3. `function f() { function g() {} }` keeps `g`; top-level
   `[1, 2].forEach(x => console.log(x));` keeps its `(anonymous)` row.
4. `--compact -a 1`: the reducer row stays, the warning section lists it, and
   the exit code is 1.
5. `--compact --xml` (or `--csv`) output equals the same run without
   `--compact`.

Focused: `nix develop -c python -m pytest test/testCompactReport.py`. Before
completion: `nix develop -c make`.

## Ordered slices

### 1. Opt into a compact text report that omits trivial nested callbacks
Type: Behavior
Status: done
Proof: CLI examples 1-5 above; full `nix develop -c make`.

Behavior: Given a JavaScript file with nested anonymous callbacks, running the
CLI with `--compact` prints the function table without rows for anonymous
functions defined inside another function with CCN 1 and no warning under the
current thresholds and whitelist, while file/total counts, the warning section,
exit status, and non-text formatter output are the same as without the option.

## Concern review

Refinement not needed: one cohesive user-visible outcome with one small data
fact (enclosing scope), one option and one table filter. The decisive premise
(enclosing function known at push time; global scope distinguishable) was
observed in this checkout. No human decision remains.

## Learnings

- Lizard lists a nested callback before its enclosing function (it ends
  first), so `sum`'s table order is `(anonymous)`, `sum`.
- The warning policy (`warning_filter.warning_policy`) is built before the
  table only under `--compact`, so the default report still prints a missing
  whitelist notice after the table.
- Accepted proof: `nix develop -c python -m pytest test/testCompactReport.py`
  (examples 1-5 plus Checkstyle, HTML, clang/MSVS, `-Eduplicate` and analysis
  results) and `nix develop -c make extensive` (2933 passed).
