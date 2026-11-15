---
name: dough-page-objects
description: >-
  Applies the shared page-object convention when authoring or changing browser
  or end-to-end tests and their page objects, for test sources selected by
  pageObjects: true in intent.locations.
---

# Check the test entry before browser test work

Use the project established by the task. Identify each test source path, including
an intended path for a new test, before applying this convention.

Follow [shared test-entry selection](../dough-execute-plan/references/test-entry-rules.md)
to resolve each source before loading [the page-object convention](references/convention.md).
Only the source's selected `pageObjects` rule activates it. Recheck when the
test's path or the work's scope changes; report unresolved selection before
changes that depend on it. Other entries retain their practice.
