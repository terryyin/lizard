# Process PR #530 — Python inline function discovery

**Identity:** SEED-040#process-pr-530
**Source:** [canonical story](../../seeds/SEED-040-process-pr-530.md#process-pr-530)

## Goal and scope

Python users get inline function suites reported with sound boundaries and
line counts, and maintainers get one complete disposition of
[PR #530](https://github.com/terryyin/lizard/pull/530): merge the corrected
revision when the gates below hold, or leave it unmerged with the unmet gate.

**In scope:** the open PR head
`2948b38ed0bf70c5a4c39163b45dfa50c3020314` and the same patch applied to
current trunk. Change the existing Python reader:
`PythonReader.preprocess`, `PythonIndents.set_nesting`, and
`PythonStates._state_first_line`. Keep the contributor's valid inline-suite
tests, re-enable `test_one_line_functions`, and add the shared-line docstring
NLOC case.

**Excluded:** the multiline f-string `end_line` limit (observed, unchanged by
the patch); closed PRs #526 and #529; other languages; a second Python parser.
No 15-minute deferral: this story corrects the NLOC defect in the same
disposition. Do not force-push.

**PFE:** Function discovery and suite boundaries already belong to the Python
reader and the shared nesting stack. The patch changes those owners. Reuse
them. No new parser, counter, or PR-specific switch.

**Performance:** paired benchmark, candidate versus the current target, on
`api`, `api-parameters`, and `cli`. Freeze the workload before timing. Each
paired 95% upper slowdown bound must be strictly below 3%. No cohort
cumulative baseline.

## Outside-in proof

Public `analyze_file.analyze_source_code` on the story's examples. Focused
Python reader tests, including the contributor file and the NLOC regression,
must fail before the behavior and pass after it. Then
`nix develop -c python -m pytest`, `nix develop -c make`, the applicable
GitHub `build` matrix, and the paired benchmark before merge. `web` runs only
when the workflow's change filter says this diff needs it.

## Settled decisive premises

Observed 2026-11-06 in this preparation workspace at
`df542e4298e6972200e8bf01d792c74f774d067d` with
`nix develop -c python` and `analyze_file.analyze_source_code`, unless a row
names another checkout. The applied patch was `git apply` of the captured
diff, then `git apply -R`; the tree was clean afterward. Packet:
`/private/tmp/lizard-pr-530-evidence/terryyin--lizard--pr530/adb05ad67af6c485e079054ac8c58e4dac6386f730908eb5db265275b9405d41`.

| Premise | Observation | Result |
| --- | --- | --- |
| PR still open at the intake head | `gh` packet, head `2948b38`, base `01afe9c`, state open, 1 unresolved thread | True |
| Patch applies to this trunk | `git apply --check`, then apply, on `df542e4` | Applies |
| Inline `Stream.read` / `Stream.close` missing on trunk | public analyzer, no patch | `[]` |
| Same example after the patch | public analyzer, patch applied | `read` and `close`, lines 2 and 3, NLOC 1, CCN 1 |
| Nested inline drops the outer function on trunk | `def outer` / `def inner(): return 1` | only `outer.inner` |
| Nested inline after the patch | contributor example `outer` / `inner(flag)` | `outer.inner` and `outer`, CCN 2 and 2, end lines 2 and 3 |
| Block `kept(x)` with `if` stays | public analyzer, no patch | `kept`, lines 1–4, NLOC 4, CCN 2 |
| Same block after the patch | public analyzer, patch applied | unchanged |
| Shared-line docstring plus `return` | patch applied: `def value(): """A value."""; return 1` | discovered with NLOC 0 |
| Block docstring baseline | no patch: docstring on the next line, and docstring plus `return` | NLOC 1 and NLOC 2; the `def` line remains |
| Docstring-only inline | patch applied: `def documented(): """only docs"""` | NLOC 0, unlike the block form's NLOC 1 |
| f-string decision stays inside its function | patch applied, contributor one-line f-string example | `value` CCN 2 end line 1; `next_value` CCN 1 end line 2 |
| Deferred multiline f-string end line | same source with and without the patch | `show` end line 3, NLOC 2, CCN 1 either way |
| Existing Python tests still pass with the patch | `pytest` `testPythonInlineSuites.py`, `testPythonStructure.py`, `testPythonFstrings.py`, `testPython.py` | 57 passed |
| Fast-forward publication is available | `git push --dry-run` of descendant `7bca5a3` to `ahuimanu/lizard` `fix/python-inline-suites`; `git ls-remote` afterward | dry-run accepted the update; remote remained `2948b38`. `maintainer_can_modify` is false. Do not force-push |

## Ordered slices

### 1. Report Python inline suites with the definition line counted
Type: Behavior
Status: planned
Proof: story examples through `analyze_file.analyze_source_code`; red then
green on the new NLOC case and the re-enabled one-line test; contributor
inline-suite tests; focused Python reader files; full pytest and `make`;
paired `api`, `api-parameters`, and `cli` against the current target; required
CI on the final head.
Behavior: Given the story's inline, nested, continuation, f-string, block, and
shared-line docstring examples on the current target → finish PR #530's
disposition → public analysis reports those functions with the boundaries and
NLOC in the story, the performance bound holds, and the corrected head is
merged with the contributor's commit retained, or the unmet gate is recorded
and the PR stays unmerged.

## Current decisions

- One Behavior slice owns review, the NLOC correction, refactor, proof,
  performance, CI, publication, and merge.
- Publish the correction as a fast-forward on
  `ahuimanu:fix/python-inline-suites`. The dry-run showed that path accepts a
  descendant. Refuse a force-push.
- Correct `_state_first_line` so a docstring that shares the `def` line does
  not remove that line's NLOC. Additional docstring-only lines may still be
  excluded, matching the block baseline.
- Thank the contributor after a confirmed merge, including the NLOC
  correction. The invocation of Lizard Process PR authorizes that comment.

## Concern evidence

- Slice-plan refinement: not needed — one complete-cycle Behavior, one proof
  loop, and no second outcome or hidden Structure slice.
- Remaining slice-specific concerns: none identified.
