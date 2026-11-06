# SEED-035 slice 2 — land member-name skip under NS performance gates

Host: Territory.local. Date: 2026-11-06 → 2026-11-07.
Execution checkout leaves uncommitted product (`lizard_ext/lizardns.py` +
discriminating test) matching retained `pr.diff`. Timing candidate =
`origin/master` `7e4ef442…` + same patch (product bytes match execution
`lizardns.py` / test). Slice status remains **planned** (coordinator owns
delivery).

## Expensive-proof decisions

- Wait for three consecutive 1-minute load samples with load1 &lt; 2.5 before
  each launch; sequential six workloads; `--seconds 30 --pairs 20 --limit-pct 3
  --extension ns`.
- Did **not** reuse slice1 full-candidate cumulative api-parameters JSON:
  `origin/master` advanced `996046c9…` → `7e4ef442…`; remasured all six on the
  refreshed candidate.
- Quiet gates met before each launch (see `expensive-proof-decisions.log` /
  `load-gate-timings.log`). Mid-run `load_after` often &gt; 2.5; next launch
  waited for a fresh quiet streak. No incomplete/contaminated runs.

## Checkouts / SHAs

| Role | Path | SHA / state |
| --- | --- | --- |
| Execution (uncommitted product) | this worktree @ `43a07aafa…` + dirty | `_member_name` present |
| baseline-inc | `…/pr554/baseline-inc` | `7e4ef442c1df7a2db1f181a86f6439f7ed933910` (no patch) |
| candidate / full-candidate | `…/pr554/candidate` | same master + `pr.diff` |
| baseline-cum (frozen) | `…/pr554/baseline-cum` | `4a05e4056cebb2edd75cb02b7a43e911369f4ce5` |
| Corpora | `…/pr552/proof/corpus/` | `affected-expanded` (api/cli), `consumers` (api-parameters) |

## Semantics / suites

| Proof | Result |
| --- | --- |
| Discriminating test red without product | FAILED as expected (`discriminating-red.log`) |
| Discriminating test green with product | 1 passed (`discriminating-green.log`) |
| Focused `testJavaScriptNestedStructures.py` | 4 passed |
| Full `pytest` | 3038 passed, 7 skipped |
| `make` | exit 0 |

## Paired `--extension ns` results (all decisive, quiet-load)

| Workload | Mode | Median Δ | 95% CI | Upper &lt; 3% | Verdict |
| --- | --- | --- | --- | --- | --- |
| api | incremental | −0.093% | [−0.417%, +0.233%] | yes | **passes** |
| api-parameters | incremental | −0.079% | [−0.466%, +0.310%] | yes | **passes** |
| cli | incremental | +0.260% | [−0.862%, +1.395%] | yes | **passes** |
| api | cumulative | +0.589% | [+0.359%, +0.819%] | yes | **passes** |
| api-parameters | cumulative | +2.018% | [+1.860%, +2.177%] | yes | **passes** |
| cli | cumulative | +0.530% | [+0.449%, +0.611%] | yes | **passes** |

JSON under this directory: `incremental-*.json`, `cumulative-*.json`.

**Every required gate’s paired 95% upper is strictly &lt; 3%** on candidate
`7e4ef442…` + as-authored `_member_name`.

## G1 (cumulative CLI)

Quiet-load decisive **pass** (CI upper ~0.611%). Replaces attempt5’s incomplete
cumulative CLI for this candidate. Coordinator may record G1 disposition as
proved; slice Status left planned.

## Product owner

As-authored only: `NestedStructure._state_global` (`.` / `?.`) +
`_member_name` in `lizard_ext/lizardns.py`, plus
`test_member_named_like_a_structure_is_not_a_structure`. No extra NS remediation
required for the six gates on this observation.
