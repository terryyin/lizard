# Plan: Make the output tests independent of order and working directory

**Identity:** SEED-003#self-contained-output-tests

**Source:** [Make the output tests independent of order and working directory](../../seeds/SEED-003-core-module-structure.md#self-contained-output-tests)

## Provenance

Execution retrospective of SEED-003#core-module-architecture
(plan `.planning/slice-plans/002-core-module-architecture/PLAN.md` at commit
`0d2eade`), reviewed commits
`269bd9e`, `bfa74f1`, `1750e67`, `3dec892`, `ae400bd`, `18dc561` on
`claude/change-the-core-analyzer-through-modules-shaped`. Both weaknesses
predate that story (observed at its starting commit `f626c3f` and at every
slice start); the story's plan recorded them in **Current decisions** and
**Learnings**. Slice 6 relabelled the agent map's "Core analyzer" focused
command, which is the command that fails.

## Current findings

- `nix develop -c python -m pytest test/test_analyzer.py test/testOutput.py`
  (the agent map's "Core analyzer" command) reports
  `1 failed, 45 passed`:
  `TestFileInformationOutput::test_print_and_save_detail_information_with_ext`
  builds `OutputScheme([Ext()])` and calls `print_and_save_modules` without
  `scheme.patch_for_extensions()`, so `FileInformation` lacks
  `average_max_nesting_depth` unless an earlier test in the run patched it.
  The full suite passes only because of test order.
- Run from a directory holding `whitelizard.txt` with `foo`,
  `test/testOutput.py` also fails
  `TestWarningOutput::test_should_use_clang_format_for_warning` and
  `test_warning_when_max_nesting_depth_missing`: these reach
  `whitelist_filter` through `get_warnings` with the default whitelist and
  read the real file. `test_sort_warning` reaches the same read.

## Preserved promises and constraints

- No product code change; CLI output and `import lizard` behavior unchanged.
- Tests stay at the public entry points they use today; patch only the
  filesystem boundary (`lizard_core.warning_filter.auto_read`), never parsing
  logic (`AGENTS.md` principles 3 and 4).
- Every existing assertion keeps its meaning; no test is weakened or removed.

## Outside-in proof

| Promise | Owning slice | Observable proof |
| --- | --- | --- |
| The agent map's core analyzer command passes on its own | 1 | `nix develop -c python -m pytest test/test_analyzer.py test/testOutput.py` → no failures; `nix develop -c python -m pytest test/testOutput.py::TestFileInformationOutput` → no failures |
| Output tests ignore a `whitelizard.txt` in the working directory | 1 | From a temp dir holding `whitelizard.txt` with `foo`: `PYTHONPATH=<checkout> <checkout>/.venv/bin/python -m pytest -q -p no:cacheprovider --rootdir=<checkout> <checkout>/test/testOutput.py` → no failures |
| Nothing else changes | 1 | `nix develop -c python -m pytest -q` → all pass with the same count; `nix develop -c make` green; `git diff --stat` touches only `test/testOutput.py` |

## Decisive premises

| Premise | Consuming operation | Observation | Result |
| --- | --- | --- | --- |
| The focused command fails today | Slice 1 proof | `.venv/bin/python -m pytest -q -p no:cacheprovider test/test_analyzer.py test/testOutput.py` at `18dc561` | `1 failed, 45 passed` (`test_print_and_save_detail_information_with_ext`); settled |
| A cwd `whitelizard.txt` breaks output tests today | Slice 1 proof | From a temp dir with `whitelizard.txt` = `foo`, pytest on `test/testOutput.py` at `18dc561` | `3 failed, 21 passed` (the two `TestWarningOutput` tests above plus the order-dependent one); settled |

## Slices

### 1. Output tests set up their own extension schema and whitelist
Type: Behavior
Status: done
Proof: the three proofs in **Outside-in proof**.

Behavior: `test_print_and_save_detail_information_with_ext` depends on test
order and `TestWarningOutput` reads the working directory's
`whitelizard.txt` → the extension test patches `FileInformation` through the
scheme it builds (as `main` does), and `TestWarningOutput` patches
`lizard_core.warning_filter.auto_read` to return an empty whitelist for each
test → the focused command and the temp-directory run pass, and the full
suite passes with the same count.

## Current decisions

- One slice: both fixes are test-only edits to one file with one proof loop.
- Calling `scheme.patch_for_extensions()` in the test mirrors how `main`
  prepares a scheme for extensions; it patches the shared `FileInformation`
  class the same way the order-dependent run already does.

## Accepted proof

Slice 1 (`test/testOutput.py` only, +5 lines; base `a95ace8`):

- Source-only review of the correction story returned ten commitments
  (order independence, cwd `whitelizard.txt` independence, ext test alone and
  with only `test/test_analyzer.py`, no reliance on an earlier patch,
  `TestWarningOutput` from a whitelist cwd via `auto_read`, test-only, CLI
  output and full-suite results unchanged); each maps to an observation below.
- `nix develop -c python -m pytest test/test_analyzer.py test/testOutput.py`
  → 46 passed (was 1 failed, 45 passed).
- `nix develop -c python -m pytest test/testOutput.py::TestFileInformationOutput`
  → 3 passed; the ext test alone → 1 passed (`Avg.ND` header and row
  assertions).
- From a temp dir holding `whitelizard.txt` = `foo`:
  `PYTHONPATH=<checkout> <checkout>/.venv/bin/python -m pytest -q -p no:cacheprovider --rootdir=<checkout> <checkout>/test/testOutput.py`
  → 24 passed (was 3 failed, 21 passed); `TestWarningOutput.setUp` patches
  `lizard_core.warning_filter.auto_read` to `''`.
- `nix develop -c python -m pytest -q` → 1652 passed, 7 skipped (no tests
  added or removed); `nix develop -c make` green.
- Consumer trace: no production or test consumers; guidance readers
  (`.agents/agent-map.md`, the seed, this plan) only name the path or command.
- Independent refactor pass: no edits.

## Learnings

None.

## Execution complete

Product advice: no change. The correction stays within the near-future
direction (maintenance); the sibling whitelist tests (`test/test_analyzer.py`,
`test/testExtension.py`, `testCppOperatorDeclarations.py`) already pass from a
working directory holding `whitelizard.txt` (31 passed), so no follow-up is
needed. Process review skipped by project default.
