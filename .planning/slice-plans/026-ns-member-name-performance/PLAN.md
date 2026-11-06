# Land nested-structure member-name skip under cumulative NS performance

**Identity:** SEED-035#ns-member-name-performance
**Source:** [canonical story](../../seeds/SEED-035-ns-member-name-performance.md#ns-member-name-performance)
**Source review:** [source-review.md](./source-review.md)

## Goal and scope

JavaScript/TypeScript `-ENS` users get accurate nesting when a structure word
is a member name, and maintainers merge
[PR #554](https://github.com/terryyin/lizard/pull/554) (or an equivalent
exact-head revision) only after every required paired performance gate under
`--extension ns` has a paired 95% upper slowdown strictly below 3% — including
cumulative `api-parameters` against the frozen accepted-set baseline — then
acknowledge the contributor.

**In scope:** the as-authored `_member_name` after `.` / `?.` in
`lizard_ext/lizardns.py` plus
`test_member_named_like_a_structure_is_not_a_structure`; product changes in
existing NS owners needed so the gates pass; quiet-load remeasurement of all
six incremental and cumulative workloads; merge and acknowledgement.

**Excluded:** other language readers; other open FabioMalpezzi PRs; changing
how NS counts real control structures; a parallel nesting counter; relaxing
the strict &lt;3% / frozen-baseline contract for this merge. The SEED-034
≤15-minute product budget does not apply — this story is that follow-up.

**PFE:** Nested-structure detection already belongs to
`lizard_ext/lizardns.py` and its discriminating tests under
`test/testJavaScriptNestedStructures.py`. Reuse those owners. No second
counter or PR-specific switch.

**Performance:** installed
`.agents/skills/lizard-process-pr/scripts/paired_benchmark.py` with
`--extension ns`. Workloads `api`, `api-parameters`, and `cli`. Incremental vs
current target; cumulative vs frozen baseline
`4a05e4056cebb2edd75cb02b7a43e911369f4ce5`. Corpora reused from
`/Users/terryyin/.cache/lizard-pr-reviews-SEED-034/pr552/proof/corpus/`
(`affected-expanded` for api/cli, `consumers` for api-parameters). Retain
SEED-034 attempt5 evidence; do not time the same failing candidate seeking a
pass without a product or contract-compatible measurement change.

## Outside-in proof

Public `FileAnalyzer` with the NS extension (same entry as
`test/testJavaScriptNestedStructures.py`) on the story examples.
Discriminating test
`test_member_named_like_a_structure_is_not_a_structure` fails without the
behavior and passes with it. Focused NS/JS/TS coverage the change reaches,
then `nix develop -c python -m pytest`, `nix develop -c make`, required hosted
CI on the exact merge head, and every required paired workload under
`--extension ns` before merge.

## Settled decisive premises

Observed 2026-11-06 in this preparation workspace at
`636a9a9d42cbcea822faaa00685daeeb4a6b36d3` with `nix develop -c python` and
`FileAnalyzer(get_extensions([NestedStructure()]))`, unless a row names
another source. The applied patch was `git apply` of
`docs/reviews/fabio-malpezzi-SEED-034/pr554/pr.diff`, then `git apply -R` or
`git checkout --` of touched paths; the tree held only the seed edit
afterward.

| Premise | Observation | Result |
| --- | --- | --- |
| PR #554 still open at disposition head | `gh pr view 554`, head `2368413b2de05b05ff44ce863dcded8061342492`, state OPEN | True |
| Patch applies to this trunk | `git apply --check` then apply of retained `pr.diff` on `636a9a9` | Applies |
| Member `.catch` / `?.catch` inflate NS on trunk | public NS analyzer, no patch → `[(anonymous),0], [(anonymous),1], [load,0], [(anonymous),0], [retry,2]` | Defect present |
| Same example after the patch | public NS analyzer, patch applied → `[(anonymous),0], [(anonymous),0], [load,0], [(anonymous),1], [retry,1]` | Matches story / test |
| Discriminating test fails without product change | patch applied, then `lizardns.py` restored; pytest that test | Failed vs expected list |
| Discriminating test passes with product change | full patch applied; same pytest | Passed |
| Existing JS NS file still green without the new test | trunk `pytest test/testJavaScriptNestedStructures.py` | 3 passed |
| Frozen cumulative baseline checkout present | `git -C …/pr554/baseline-cum rev-parse HEAD` | `4a05e4056cebb2edd75cb02b7a43e911369f4ce5` |
| Frozen corpora present | `ls …/pr552/proof/corpus/` | `affected-expanded`, `consumers` present |
| Quiet-load cumulative `api-parameters` failed for #554 candidate | retained attempt5 JSON / README; median ~3.638%, CI [+2.770%, +4.513%], `--extension ns` | Bound not met |
| Incremental api / api-parameters / cli and cumulative api passed under quiet load | retained attempt5 measurement-decision / README | Pass |
| Prior-set-alone NS cumulative `api-parameters` owner | Quiet-load SEED-035 slice1 prior-set vs frozen baseline `4a05e405…`: median ~0.643%, CI [+0.303%, +0.984%], `--extension ns`, passes; full-candidate (master `996046c9…` + `pr.diff`) CI [+1.127%, +1.411%], also passes. Evidence: `docs/reviews/fabio-malpezzi-SEED-034/pr554/seed035-slice1/` | Settled — prior-set alone does **not** exceed &lt;3%; incremental cost of #554’s `_member_name` / `_state_global` still under bound; no trunk NS owner must change solely for this gate |

## Ordered slices

### 1. Confirm who owns the cumulative NS api-parameters overage
Type: Behavior
Status: done
Early probe: yes — settles the open prior-set cost-owner premise; a result
that cannot guide a gate-passing merge stops dependent slice 2 and revises
the plan.
Proof: quiet-load paired `--extension ns` `api-parameters` of (a) current
target / accepted set without #554 vs frozen baseline, and (b) full candidate
including #554 semantics vs the same baseline; retain JSON under the story
evidence root; complete or replace attempt5’s incomplete cumulative CLI only
when the host stays quiet enough for a decisive result.
Behavior: Given the retained attempt5 fail and unsettled isolation → run the
quiet-load prior-set and full-candidate comparisons → the plan records whether
prior-set alone already exceeds the &lt;3% upper bound and which existing NS
owner must change for slice 2.

### 2. Land member-name skip under every required NS performance gate
Type: Behavior
Status: done
Proof: discriminating test red then green; focused NS/JS/TS coverage reached
by the change; full pytest and `make`; all six paired workloads under
`--extension ns` with paired 95% upper slowdown strictly below 3%; required
hosted CI on the exact head; merge of #554 (or equivalent exact-head
revision) with contributor acknowledgement.
Behavior: Given #554’s member-name semantics and the cost owner from slice 1
→ change existing NS owners only as needed so every required gate passes →
member `.catch` / `?.catch` are not structures, real `if` / `try`/`catch`
still raise NS, the exact head merges, and the contributor is thanked; if a
required gate cannot pass without relaxing the story’s contract, leave #554
unmerged with evidence and stop for human review of that contract.

## Current decisions

- Keep the Fabio cohort strict &lt;3% upper bound and the frozen cumulative
  baseline `4a05e4056cebb2edd75cb02b7a43e911369f4ce5` for this merge.
- Prefer the as-authored `_member_name` semantics; expand product changes
  only to clear the measured gate, still inside `lizardns` / its tests.
- Refresh baseline/candidate checkouts and corpus hashes before timing;
  reuse attempt5 only while production bytes, corpus, harness, and options
  remain compatible.
- Do not force-push the contributor branch; publish corrections without
  lost history when the PR head must move.
- Thank the contributor only after a confirmed merge.

### Slice 1 learnings (measurement; Status remains planned until coordinator delivery)

- Prior-set alone (current `origin/master` `996046c9…` without #554) vs
  frozen baseline: cumulative NS `api-parameters` **passes** (CI upper
  ~0.984% &lt; 3). Already-accepted trunk work does **not** own a failing
  overage on this gate; `lizardns.py` unchanged vs baseline-cum.
- Full candidate (same master + retained `pr.diff`): also **passes** (CI
  upper ~1.411% &lt; 3). Incremental cost is owned by #554’s
  `NestedStructure._state_global` (`.` / `?.` branch) and
  `_member_name` in `lizard_ext/lizardns.py`, still under the bound.
- Slice 2 product owner for the member-name landing remains those
  functions (plus the discriminating test); no separate trunk NS
  performance remediation is required for cumulative api-parameters on
  this refreshed observation. No early-probe stop — result can guide a
  gate-passing merge for this workload; remaining paired gates
  (including quiet-load cumulative CLI; attempt5 cum-cli left incomplete)
  still required.
- Expensive-proof: quiet-gate wait retained; sequential prior-set then
  full-candidate; cum-cli skipped after full-candidate `load_after` ≫ 2.5.

### Slice 2 learnings (measurement + product; Status remains planned until coordinator delivery)

- Landed as-authored `_member_name` / `_state_global` `.`/`?.` plus
  discriminating test on the execution tree (uncommitted). Timing candidate
  = current `origin/master` `7e4ef442…` + retained `pr.diff` (product bytes
  match execution). Slice1 cum-api-parameters JSON not reused (master advanced).
- Discriminating red-without / green-with proved; focused NS file 4 passed;
  full pytest 3038 passed / 7 skipped; `make` exit 0.
- Quiet-load remasured all six paired `--extension ns` workloads
  (`--seconds 30 --pairs 20 --limit-pct 3`): every paired 95% upper strictly
  &lt; 3%. Cumulative api-parameters vs frozen `4a05e405…`: CI
  [+1.860%, +2.177%]. Cumulative CLI: CI [+0.449%, +0.611%] — decisive quiet
  pass; G1 interim can become proved (coordinator records disposition).
- Evidence: `docs/reviews/fabio-malpezzi-SEED-034/pr554/seed035-slice2/`.
  No contract relaxation; no extra NS owner changes beyond as-authored patch.

## Concern evidence

- Slice-plan refinement: not needed — one early probe for the unsettled
  cost-owner premise, one merge Behavior, one NS owner, one proof loop per
  slice.
- Remaining slice-specific concerns: none identified beyond the early probe
  bounded in slice 1.

## Story obligations

### G1. Cumulative CLI still lacks a quiet-load decisive pass
Reported: slice 1 — "Cumulative CLI not replaced (post-run load too high); attempt5 incomplete cum-cli retained."
Story clause: "Incomplete or contaminated runs (including attempt5’s unfinished cumulative CLI) do not count as a pass; quiet-load decisive results are required."
Disposition: proved by slice 2: docs/reviews/fabio-malpezzi-SEED-034/pr554/seed035-slice2/cumulative-cli.json (quiet-load pass, CI upper ~0.611%)
