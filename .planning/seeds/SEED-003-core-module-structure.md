---
id: SEED-003
status: active
planted: 2026-10-23
planted_during: Backlog request to restructure the core analyzer module
trigger_when: A maintainer changes analysis, CLI, file discovery, or output code in lizard.py
scope: large
---

# SEED-003: Core module structure

<a id="self-contained-output-tests"></a>

### Make the output tests independent of order and working directory

**Identity:** SEED-003#self-contained-output-tests
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/003-self-contained-output-tests/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"506959cb0f4690cc698e6622d679a2692eeda904a38b95788dbfbedc97a57d82","plan":"5a8fd244368d83b1c702892537999b360607fb288005264f6eea93ea64357884"}}
```

**Goal:** A maintainer running the core analyzer's focused tests, as
`.agents/agent-map.md` names them, gets a result that does not depend on
which other tests ran first or on a `whitelizard.txt` in the working
directory. This is a bounded correction from the execution retrospective of
SEED-003#core-module-architecture (`.planning/seeds/SEED-003-core-module-structure.md`
at commit `0d2eade`); it adds no product
promise.

**Scope:**

- `test/testOutput.py` `TestFileInformationOutput::test_print_and_save_detail_information_with_ext`
  passes when run alone or with only `test/test_analyzer.py`, without relying
  on an earlier test having patched `FileInformation` for extensions.
- `test/testOutput.py` `TestWarningOutput` tests pass from a working
  directory that holds a `whitelizard.txt`, by patching the whitelist read at
  `lizard_core.warning_filter.auto_read`.
- Test-only. Product code, CLI output, and the full suite's results do not
  change.

Plan: [003-self-contained-output-tests](../slice-plans/003-self-contained-output-tests/PLAN.md)
