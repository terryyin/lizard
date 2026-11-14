---
name: dough-makeme
description: >-
  Selects and applies the shared MakeMe convention when writing or refactoring
  tests or their data builders. Only test sources in configured entries with
  makeMe enabled select the convention.
---

# Apply MakeMe to selected tests

Use the project established by the task. Identify the test source paths in scope,
including intended paths for new tests and the consuming tests for builder changes.

Follow [shared test-entry selection](../dough-execute-plan/references/test-entry-rules.md)
to resolve each source before loading [the shared convention](references/convention.md).
Only the source's selected `makeMe` rule activates it, independently of
`pageObjects` and whether the test uses a browser. Other sources keep their
project practice. Discovering or reading this entrypoint alone does not enable it.
Recheck when paths or scope change; report unresolved selection before changes
that depend on it.
