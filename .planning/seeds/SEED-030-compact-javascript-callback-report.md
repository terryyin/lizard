---
id: SEED-030
status: active
planted: 2026-11-04
planted_during: Product backlog intake requested by the developer
scope: Compact JavaScript callback reporting
---

# SEED-030: Compact JavaScript callback reporting

## Why This Matters

JavaScript developers reviewing Lizard reports can get many `(anonymous)`
rows for callbacks with no decisions of their own. A compact report makes
the functions that need attention easier to find. See [GitHub issue #324](https://github.com/terryyin/lizard/issues/324).

## Selected Stories

<a id="compact-callback-report"></a>

### 1. Show a compact JavaScript complexity report

**Identity:** SEED-030#compact-callback-report
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/019-compact-callback-report/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"b187f17d95e5662b1934e1c592c91b4ac02d6ddf448bd3177079e00e8dff7172","plan":"1db694f57b3096a9cf4448c39fdea7791e4a8dd1c91ed58aa62f1719f6fa0240"}}
```

**Goal:** A JavaScript developer scanning Lizard's CLI report can opt into a
compact table that leaves out rows for trivial nested callbacks, so the
functions that need attention are easier to find, without changing any
function's measurements or the default report.

**Scope:**

- A long-only `--compact` option changes the default human-readable text report.
  In its function table, a row is left out only when that function is named
  `(anonymous)`, was defined inside another function (not at file/global scope),
  has CCN 1, and is not reported as a warning under the current thresholds,
  options and whitelist. The enclosing function's row is unchanged.
- Each retained row keeps its own metrics and source location. The file table,
  warning section, totals, function counts and averages still cover all analyzed
  functions, and the warning exit status is unchanged.
- Without `--compact`, every output is unchanged. With it, machine-readable and
  other formatter outputs (XML, CSV, Checkstyle, HTML, clang/MSVS warning
  styles and extension printers) are unchanged.
- Boundary assumption: delivery is verified for JavaScript. Other readers that
  name nested functions `(anonymous)` get the same table behavior naturally; no
  language filter is added and no other reader's naming is changed.
- Excluded: adding a callback's complexity, tokens or parameters to its
  enclosing function (the issue's alternative), a marker or count of omitted
  rows, compacting named or CCN > 1 callbacks, and compact behavior for other
  formatters.

**Key examples:**

- `const sum = array => array.reduce((a, b) => a + b);` with `--compact`: one
  function row, `sum` at CCN 1; file and total function counts are 2. Without
  `--compact`, and in analysis results, both functions appear at CCN 1.
- Changing the reducer to `(a, b) => a ? a + b : b` keeps its CCN 2 row with
  `--compact`; `sum` stays CCN 1.
- With `--compact`, `function f() { function g() {} }` keeps `g`, and a
  top-level `[1, 2].forEach(x => console.log(x));` keeps its `(anonymous)`
  row, because it has no enclosing function.
- With `--compact -a 1`, the two-parameter reducer stays visible at CCN 1 as a
  parameter warning, and the warning still makes the run exit non-zero.
