---
id: SEED-017
status: active
planted: 2026-10-27
planted_during: Maintainer report of shifted Vue line numbers
---

# SEED-017: Vue source line numbers

<a id="vue-source-line-numbers"></a>

### Preserve original Vue line numbers when template precedes script

**Identity:** SEED-017#vue-source-line-numbers
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/013-vue-source-line-numbers/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"a37a67d33d36c419a9f9cfe315fe88e76e3ed1d02fe3d8dcd8bb48c97beb6864","plan":"7db6bc9ea7ccabbecdcf82e2f73c27e5acd3c7174112e2cee21664a5a6215b7c"}}
```

**Beneficiary:** Vue users locating analyzed functions and warnings in their
original single-file components.

**Goal:** Report line numbers from the original `.vue` file when a `<template>`
block appears before `<script>`, so users can navigate directly to the reported
source locations.

**Reported behavior and evidence:** The maintainer reports that this existing
bug is unrelated to arrow handling. Preparation reproduced it on `b37648a`
through public analysis and the real CLI: named functions on source lines
8–11 and 13–15 report 2–5 and 7–9, and a warning for the first function points
to line 2. Adding four lines before `<script>` leaves those incorrect reports
unchanged. A component beginning with `<script>` reports its correct locations.

**Scope:** Preserve original source locations while filtering Vue blocks for
analysis. Public function `start_line` and `end_line`, default CLI locations,
and warning locations use the original component's one-based lines. Blank
lines and ignored template/style content contribute their source line breaks,
including between already-supported script blocks. JavaScript and TypeScript
script content retain their existing function detection and metrics; ignored
content does not become analyzed code.

Deferred promises: arrow parsing or parameter changes, broader Vue syntax
support, source maps, and new output formats or options. This delivery does not
combine the existing JavaScript/TypeScript arrow stories or require them as
suppliers.

**Key examples:**

- Given a template followed by two named script functions on lines 8–11 and
  13–15, public analysis and the default CLI report those original ranges;
  requesting CCN warnings at threshold 1 points to the first function at line 8.
- Given the same script with four additional blank/template lines before it,
  analyzing the component moves both ranges by four lines, to 12–15 and 17–19,
  without changing function names, NLOC, CCN, token counts, or parameter counts.
- Given equivalent JavaScript or `<script lang="ts">` functions after ignored
  multiline content, analyzing the component retains original source ranges;
  content between supported script blocks likewise retains the later script's
  locations. Template/style content produces no functions or code metrics.
- Given a component that starts with `<script>`, analyzing it retains its
  correct ranges and metrics. A component with no script still has no functions.
