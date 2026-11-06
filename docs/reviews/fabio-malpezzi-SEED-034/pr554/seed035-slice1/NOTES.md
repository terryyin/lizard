# SEED-035 slice 1 — cumulative NS api-parameters cost-owner probe

Host: Territory.local. Date: 2026-11-06. No product code changed in the
execution checkout; measurement and plan evidence only.

## Expensive-proof decision

Retained: wait for three consecutive 1-minute load samples with 1-min load
&lt; 2.5 before each timing launch; run prior-set then full-candidate
sequentially; skip cumulative CLI unless the host stays quiet after both
api-parameters runs.

- First quiet gate (before prior-set): samples 17–19 in `load-gate.log`
  (`2.409`, `2.249`, `2.295`) → `QUIET_GATE_MET` 20:16:49+0700.
- Second quiet gate (before full-candidate): samples 15–17 in
  `load-gate-full.log` (`2.378`, `2.033`, `2.316`) → `QUIET_GATE_MET`
  20:56:52+0700.
- After full-candidate: `load_after` ≈ `[4.40, 3.51, 3.02]` — above the
  quiet gate. Cumulative CLI not re-run; attempt5’s incomplete cum-cli
  left as-is (`attempt5/cumulative-cli-timings.log`).

## Checkouts / SHAs

| Role | Path | SHA / state |
| --- | --- | --- |
| Frozen baseline-cum | `~/.cache/lizard-pr-reviews-SEED-034/pr554/baseline-cum` | `4a05e4056cebb2edd75cb02b7a43e911369f4ce5` |
| Prior-set (current origin/master, no #554) | `…/pr554/prior-set` | `996046c9a62fb42a554e98d68e313bb45d5517ba`; `_member_name` absent |
| Full candidate | `…/pr554/full-candidate` | same master SHA + `docs/reviews/…/pr554/pr.diff` applied; `_member_name` / discriminating test present |
| Corpus | `…/pr552/proof/corpus/consumers` | frozen consumers corpus |

`origin/master` at probe time matched the slice start SHA
`996046c9a62fb42a554e98d68e313bb45d5517ba` (advanced past attempt5’s
then-master).

## Results (paired `--extension ns` `api-parameters`, `--seconds 30 --pairs 20 --limit-pct 3`)

### (a) Prior-set vs frozen baseline

- JSON: `prior-set-cumulative-api-parameters.json`
- `passes`: true · `verdict`: `passes`
- `paired_change_pct`: **0.643%**
- `paired_95pct_ci`: **[+0.303%, +0.984%]** (upper strictly &lt; 3)
- `load_before`: `[2.351, 2.909, 3.076]` · `load_after`: `[3.111, 3.357, 3.283]`

**Prior-set alone does not exceed the &lt;3% upper bound.** Already-accepted
trunk work (relative to the frozen cumulative baseline) does not own a
failing cumulative NS api-parameters overage on this refreshed observation.
`lizard_ext/lizardns.py` itself is unchanged between baseline-cum and
current master; accepted-set product deltas in this window are language
readers (JS/TS/Swift/C#), not NS.

### (b) Full candidate (#554 semantics) vs same baseline

- JSON: `full-candidate-cumulative-api-parameters.json`
- `passes`: true · `verdict`: `passes`
- `paired_change_pct`: **1.269%**
- `paired_95pct_ci`: **[+1.127%, +1.411%]** (upper strictly &lt; 3)
- `load_before`: `[2.193, 2.479, 2.781]` · `load_after`: `[4.398, 3.507, 3.025]`

**Only full-candidate (vs prior-set) adds measurable cost, and it still
clears the bound.** Incremental overage relative to prior-set is owned by
PR #554’s addition: `NestedStructure._state_global` (the `'.'` / `'?.'`
branch) and `NestedStructure._member_name` in `lizard_ext/lizardns.py`.

## Slice-2 owner guidance

- Prior-set alone: **does not** exceed &lt;3% → no already-accepted NS
  owner on trunk must change solely to clear this cumulative api-parameters
  gate.
- Full-candidate: **passes** on current master + retained `pr.diff` →
  #554’s `_member_name` owns the incremental cost but not a failing
  overage on this workload.
- Slice 2 should land the as-authored member-name skip in
  `lizard_ext/lizardns.py` (`_state_global` / `_member_name`) plus
  `test/testJavaScriptNestedStructures.py`, and still clear the remaining
  required paired workloads (including incomplete cum-cli under quiet
  load). No early-probe stop for a contract-blocked merge on the basis of
  this cumulative api-parameters probe alone.

## Relation to attempt5 fail

Attempt5 quiet-load full-candidate cumulative api-parameters failed
(median ~3.638%, CI upper ~4.513%) against then-master. This slice’s
required refreshed comparisons use current `origin/master`
(`996046c9…`) + the same retained patch vs the same frozen baseline and
now pass. Do not treat attempt5 as a retime-for-pass of an identical
candidate; the target SHA advanced. Remaining gates (esp. cumulative CLI)
are still required before merge.

## Can this guide a gate-passing merge?

Yes for **this** workload: both prior-set and full-candidate clear
cumulative NS `api-parameters` under quiet load on the refreshed target.
It does **not** by itself authorize merge — slice 2 must still satisfy
every required paired gate and CI. No early-probe stop signal from this
slice’s settled premise.
