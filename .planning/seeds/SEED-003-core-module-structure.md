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
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

A maintainer or contributor changing how Lizard analyzes, discovers files, or
reports results today has to work inside one ~1,200-line `lizard.py`. They
should instead find each responsibility in a module whose boundaries follow
the architecture of the analyzer, so a change to one concern is made in one
place.

The decomposition must be driven by architectural reasoning, not by the line
count:

- Each resulting module is a natural, cohesive unit of the architecture
  vision (for example along the analysis pipeline, data model, file
  discovery, warning policy, output, and CLI boundaries), using the
  ubiquitous language in `docs/adrs/0001-ubiquitous-language-accepted.md`.
- The 250-line standard is the expected result of a reasonable structure,
  not a target to force. Do not split a cohesive unit only to get under the
  limit.
- If the most reasonable structure cannot be reached in one go, leave the
  remaining long file in place rather than forcing it; record what remains
  for a later story.

Observable behavior stays unchanged: CLI output and exit codes, and the
public import surface that extensions and external users rely on, such as
`lizard.main`, `lizard.analyze`, `lizard.analyze_file`, `FunctionInfo`,
`FileInfoBuilder`, and `get_warnings` (the `lizard` console entry point and
`py_modules=['lizard']` in `setup.py` included).

The architecture vision, the target module boundaries, how the public
surface is preserved, and whether this needs an ADR are not decided yet; they
belong to refinement.
