---
id: SEED-021
status: active
planted: 2026-10-29
planted_during: Product backlog intake requested by the developer
scope: Dart language support
---

# SEED-021: Dart support

## Why This Matters

Dart and Flutter developers need to identify complex code in their codebases
using the same Lizard workflow they use for other languages.

## Selected Stories

<a id="dart-support"></a>

### 1. Support Dart analysis

**Identity:** SEED-021#dart-support
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

As a Dart developer or maintainer, I can analyze Dart source files through
Lizard's existing CLI and Python API and receive named functions and methods
with cyclomatic complexity, non-comment lines of code, token counts, and
parameter counts, so I can find code that needs review or simplification.

**Evaluation:** Representative Dart examples demonstrate `.dart` source-file
discovery, function and method boundaries, and expected metrics for
straight-line and branching code through the public analyzer and CLI.
Comments and strings do not introduce decisions. Existing language analysis
remains correct.

**Refinement questions:** Settle the initial Dart version and syntax coverage,
the treatment of constructors, getters, setters, closures, and expression-bodied
functions, and decision-counting examples before implementation planning.
