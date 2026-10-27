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

**Beneficiary:** Vue users locating analyzed functions and warnings in their
original single-file components.

**Goal:** Report line numbers from the original `.vue` file when a `<template>`
block appears before `<script>`, so users can navigate directly to the reported
source locations.

**Reported behavior:** Every reported line number is shifted in this layout.
The maintainer reports that the bug already existed and is unrelated to arrow
handling; treat it as an independent Vue location bug.

**Key examples to verify:**

- A Vue component with a multiline `<template>` before `<script>` reports each
  function's start and end lines at its actual locations in the original file,
  through public analysis and CLI output.
- Adding blank lines or template content before `<script>` moves the reported
  locations by the same number of source lines.
- Ordinary named functions reproduce and verify the issue without arrows.
  Components with `<script>` first retain correct locations.

**Scope:** Preserve original source locations across Vue block extraction.
Do not combine this work with the existing JavaScript/TypeScript arrow stories.

**Preparation:** Queued for reproduction and story refinement; no implementation
or executable plan is included in this backlog addition.
