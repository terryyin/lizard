---
name: dough-makeme
description: >-
  Checks whether test sources select the shared MakeMe convention and applies
  it when authoring, extending or refactoring tests. Use for ordinary test work
  as well as Dough execution; only configured entries with makeMe enabled
  receive the convention.
---

# Apply MakeMe to selected tests

Use the project established by the task. Before arranging test data, identify
each test source path, including an intended path for a new test.

Follow [shared test-entry selection](../dough-execute-plan/references/test-entry-rules.md)
to resolve each source before loading [the shared convention](references/convention.md).
Only the source's selected `makeMe` rule activates it, independently of
`pageObjects` and whether the test uses a browser. Other sources keep their
project practice. Discovering or reading this entrypoint alone does not enable it.
Recheck when paths or scope change; report unresolved selection before changes
that depend on it.
