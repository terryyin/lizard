---
name: dough-page-objects
description: >-
  Checks per-entry page-object guidance when authoring or changing browser tests,
  including end-to-end tests and page/component objects. Applies the shared
  convention only to test sources selected by pageObjects: true in the
  project's intent.locations configuration; other entries keep their practice.
---

# Check the test entry before browser test work

Use the project established by the task. Identify each test source path, including
an intended path for a new test, before applying this convention.

Follow [shared test-entry selection](../dough-execute-plan/references/test-entry-rules.md)
to resolve each source before loading [the page-object convention](references/convention.md).
Only the source's selected `pageObjects` rule activates it. Recheck when the
test's path or the work's scope changes; report unresolved selection before
changes that depend on it. Other entries retain their practice.
