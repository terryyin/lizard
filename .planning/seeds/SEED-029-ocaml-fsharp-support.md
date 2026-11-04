---
id: SEED-029
status: active
planted: 2026-11-04
planted_during: Product backlog intake requested by the developer
scope: OCaml and F# language support
---

# SEED-029: OCaml and F# support

## Why This Matters

OCaml and F# maintainers want to use Lizard to find complex functions in
their codebases. This request comes from [GitHub issue #303](https://github.com/terryyin/lizard/issues/303).
Each language can deliver that outcome independently.

## Selected Stories

<a id="ocaml-support"></a>

### 1. Support OCaml analysis

**Identity:** SEED-029#ocaml-support
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

As an OCaml maintainer, I can analyze OCaml source files with Lizard and see
function names and cyclomatic complexity so I can find functions to review.

**Evaluation:** Lizard discovers a representative `.ml` file and reports
separate named functions with the expected complexity for straight-line and
branching code.

**Refinement questions:** Settle initial syntax coverage, how nested
bindings are reported, and whether signature files are in scope.

<a id="fsharp-support"></a>

### 2. Support F# analysis

**Identity:** SEED-029#fsharp-support
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

As an F# maintainer, I can analyze F# source files with Lizard and see
function names and cyclomatic complexity so I can find functions to review.

**Evaluation:** Lizard discovers a representative `.fs` file and reports
separate named functions with the expected complexity for straight-line and
branching code.

**Refinement questions:** Settle initial coverage for indentation, members,
scripts, and signature files.
