---
id: SEED-020
status: active
planted: 2026-10-29
planted_during: Product backlog intake requested by the developer
scope: COBOL language support
---

# SEED-020: COBOL support

## Why This Matters

COBOL developers and maintainers need to identify complex code in their
codebases using the same Lizard workflow they use for other languages.

## Selected Stories

<a id="cobol-support"></a>

### 1. Support COBOL analysis

**Identity:** SEED-020#cobol-support
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

As a COBOL developer or maintainer, I can analyze COBOL source files through
Lizard's existing CLI and Python API and receive named executable units with
cyclomatic complexity, non-comment lines of code, and token counts, so I can
find code that needs review or simplification.

**Evaluation:** Representative COBOL examples demonstrate source-file
discovery, executable-unit boundaries, and expected metrics for straight-line
and branching code through the public analyzer and CLI. Comments and strings
do not introduce decisions. Existing language analysis remains correct.

**Refinement questions:** Settle the initial dialect and fixed/free source
format coverage, recognized file extensions, the mapping of COBOL programs,
sections, and paragraphs to reported executable units, decision-counting
examples, and the treatment of copybooks before implementation planning.
