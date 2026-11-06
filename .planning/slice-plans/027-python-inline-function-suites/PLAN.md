# Judge Python inline function suites

**Identity:** SEED-042#python-inline-function-suites
**Source:** [canonical story](../../seeds/SEED-042-python-inline-function-suites.md#python-inline-function-suites)

## Goal and scope

Python users and maintainers get a recorded decision about functions whose
suite is on the `def` line. The existing Python reader changes only when the
story examples are shown to be missing on trunk, a candidate reports them
(with shared-line docstring NLOC 1), and every required paired 95% upper
slowdown bound is strictly below 3%. Until both the behavior and the bound
hold, trunk stays as it is and
[PR #530](https://github.com/terryyin/lizard/pull/530) stays unmerged.

**In scope:** confirm the story examples on current trunk; evaluate the open
PR head `2948b38ed0bf70c5a4c39163b45dfa50c3020314` plus the shared-line
docstring NLOC correction in `lizard_languages/python.py` and
`lizard_languages/python_structure.py`; keep or adapt contributor
inline-suite tests and re-enable `test_one_line_functions`; paired
`api`, `api-parameters`, and `cli` against current trunk; land with
contributor history when both gates hold, or record the judgment on the PR
and leave it unmerged.

**Excluded:** the multiline f-string `end_line` limit; closed PRs #526 and
#529; other languages; a second Python parser; merging #530 only to close
review; treating the dropped SEED-040 experiment as accepted code. No cohort
cumulative baseline for this story.

**PFE:** Inline-suite discovery and indentation boundaries already belong to
`PythonReader` / `PythonIndents` / `PythonStates` and the Python reader tests
under `test/test_languages/`. Reuse those owners. The PR is a candidate patch
on them, not a new subsystem.

**Performance:** installed
`.agents/skills/lizard-process-pr/scripts/paired_benchmark.py`. Workloads
`api`, `api-parameters`, and `cli` against current trunk. Freeze a Python
source corpus before timing. Each paired 95% upper slowdown must be strictly
below 3%. The 2026-11-06 attempt of a behavior-correct candidate failed that
bound (~9.7% median on 281 files; paired API CI +2.2% to +13.6%; one
bracket-depth lookup alone ~+3.1%).

## Outside-in proof

Public `analyze_file.analyze_source_code` on the story examples. Discriminating
tests (contributor inline-suite file and shared-line docstring NLOC, plus
re-enabled `test_one_line_functions`) fail without the behavior and pass with
it. Focused Python reader tests the change reaches, then
`nix develop -c python -m pytest`, `nix develop -c make`, required hosted CI
on the exact landable head, and every required paired workload before merge.
When no candidate clears both gates, the recorded PR judgment and unchanged
trunk are the proof of the reject disposition.

## Settled decisive premises

Observed 2026-11-06 in this preparation workspace at
`996046c9a62fb42a554e98d68e313bb45d5517ba` with `nix develop -c python` and
`analyze_file.analyze_source_code`, unless a row names another source. The
applied patch was `git apply` of `/tmp/pr530-obs/pr.diff` (from
`gh pr diff 530`), then `git checkout --` / remove of touched paths; the tree
held only the seed edit afterward. Intake packet also retained under
`/private/tmp/lizard-pr-530-evidence/terryyin--lizard--pr530/adb05ad67af6c485e079054ac8c58e4dac6386f730908eb5db265275b9405d41`.

| Premise | Observation | Result |
| --- | --- | --- |
| PR #530 still open at disposition head | `gh pr view 530`, head `2948b38ed0bf70c5a4c39163b45dfa50c3020314`, state OPEN | True |
| Patch applies to this trunk | `git apply --check` then apply of retained `pr.diff` on `996046c9` | Applies |
| `Stream.read` / `Stream.close` missing on trunk | public analyzer, no patch; class also had block `kept` | only `[('kept', 2, 4)]` |
| Same `Stream` methods after the patch | public analyzer, patch applied | `read`, `close`, `kept` with CCN 1, 1, 2 |
| Nested `outer` / `inner(): return 1` drops outer on trunk | public analyzer, no patch | only `outer.inner` |
| Same nested example after the patch | public analyzer, patch applied | `outer.inner` and `outer` |
| Shared-line docstring on trunk | `def value(): """A value."""; return 1` | no function |
| Same docstring after the PR head patch | public analyzer, patch applied | `value` with NLOC 0 |
| Block `kept(x)` with `if` on trunk | public analyzer, no patch | `kept`, CCN 2, NLOC 4 |
| One-line regression is disabled on trunk | `testPythonStructure.py` | `xtest_one_line_functions` |
| Paired harness present | `.agents/skills/lizard-process-pr/scripts/paired_benchmark.py --help` | workloads `api`, `api-parameters`, `cli`; default limit 3% |
| Prior correct candidate failed the &lt;3% bound | seed / maintainer PR comment 2026-11-06 | ~9.7% / API CI +2.2%–+13.6%; bracket-depth alone ~+3.1% |
| Contributor branch is not maintainer-writable | retained SEED-040 packet / prior dry-run notes | `maintainer_can_modify` false; do not force-push |

## Ordered slices

### 1. Decide inline-suite reporting under the 3% gate
Type: Behavior
Status: planned
Proof: story examples through `analyze_file.analyze_source_code`; red then
green on discriminating inline-suite and shared-line docstring NLOC tests;
focused Python reader coverage; full pytest and `make`; paired `api`,
`api-parameters`, and `cli` against current trunk with frozen corpus and
paired 95% upper slowdown strictly below 3%; required hosted CI on any
landable head; otherwise a recorded judgment on PR #530 with trunk unchanged.
Behavior: Given the confirmed trunk defects and PR #530 as candidate → evaluate
at most the PR head plus shared-line docstring NLOC fix, and one cheaper
alternative in the same Python reader owners if that candidate misses the
bound → when both behavior and the &lt;3% bound hold, land with contributor
history preserved and thank the contributor; otherwise record the unmet gate
on the PR, leave it unmerged, and leave the reader on trunk unchanged.

## Current decisions

- Keep the story's strict &lt;3% paired upper-bound contract on `api`,
  `api-parameters`, and `cli` versus current trunk. No cumulative cohort
  baseline.
- Change only the existing Python reader owners; never add a second parser.
- Bound search: try the PR head + NLOC correction first; if it fails the
  bound, try one cheaper alternative in those owners that avoids per-token
  bracket-depth work when possible. If neither clears both gates, the reject
  disposition completes the story.
- Freeze baseline/candidate checkouts and a Python corpus before timing;
  retain every valid measurement, including failures.
- Do not force-push `ahuimanu:fix/python-inline-suites`. Prefer a
  fast-forward when the remote accepts a descendant; otherwise an equivalent
  exact-head vehicle that keeps contributor history.
- Thank the contributor only after a confirmed merge.

## Concern evidence

- Slice-plan refinement: not needed — one Behavior owns the complete
  disposition, one proof loop, and the cheaper-alternative bound is a current
  decision rather than a second outcome.
- Remaining slice-specific concerns: none identified.
