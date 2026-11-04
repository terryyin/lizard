---
id: SEED-032
status: active
planted: 2026-11-04
planted_during: Bug report found while fixing SEED-031 (OCaml leading blank lines)
scope: Erlang reader line numbers
---

# SEED-032: Erlang leading blank lines

## Stories

<a id="erlang-leading-blank-lines"></a>

### 1. Fix Erlang leading blank lines

**Identity:** SEED-032#erlang-leading-blank-lines
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** Resolve the reported discrepancy: Erlang functions report the line
they are actually on, even when the file starts with blank lines.

**Scope:** The Erlang reader's line numbering only.

**Expectations:**

- Expected: an `.erl` file starting with two blank lines and then
  `f(X) -> X.` reports `f` at line 3.
- Actual (observed): `f` is reported at line 1; the reader's Pygments lexer
  strips leading newlines.
- Evidence: observed through `analyze_source_code` while fixing the same
  defect in the OCaml reader; the F# and OCaml readers lex with
  `stripnl=False`.
