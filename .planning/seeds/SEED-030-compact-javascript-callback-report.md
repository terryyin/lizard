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
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

As a JavaScript developer, I can opt into a compact human-readable CLI report
so I can scan function metrics without a separate row for every simple callback.

The compact report collapses a callback row into its enclosing function's
display only when the callback is anonymous, is nested inside a function, has
CCN 1, and has no warning under the current warning settings. Each function
keeps its own metrics and source location. Summary counts and averages use all
analyzed functions. The default report, machine-readable output, and warning
exit status retain their existing behavior.

#### Evaluation

- For `const sum = array => array.reduce((a, b) => a + b)`, the compact report
  shows one row for `sum` at CCN 1. The default report and analysis results
  contain both functions at CCN 1.
- Adding a ternary to the reducer leaves its CCN 2 row visible and `sum`
  at CCN 1. A named nested function and a callback with no enclosing function
  also retain their rows.
- With the parameter limit set to one, the two-parameter reducer remains
  visible at CCN 1 and its parameter warning still affects the exit status.
