---
id: SEED-031
status: active
planted: 2026-11-04
planted_during: Bug report from the F# support story's developer handoff
scope: OCaml reader line numbers
---

# SEED-031: OCaml leading blank lines

## Stories

<a id="ocaml-leading-blank-lines"></a>

### 1. Fix OCaml leading blank lines

**Identity:** SEED-031#ocaml-leading-blank-lines
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** Resolve the reported discrepancy: OCaml functions report the line
they are actually on, even when the file starts with blank lines.

**Scope:** The OCaml reader's line numbering only.

**Expectations:**

- Expected: a `.ml` file starting with two blank lines and then
  `let f x = x` reports `f` at line 3.
- Actual (reported): `f` is reported at line 1; the reader's Pygments lexer
  strips leading newlines.
- Evidence: found while delivering F# support, whose reader lexes with
  `stripnl=False`.
- Remaining uncertainty: not yet reproduced by a test.
