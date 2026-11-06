# Source review — SEED-035#ns-member-name-performance

**Launch:** fresh general-purpose reviewer (`composer-2.5-fast`), no conversation
inheritance; input was only the seed Goal, Scope, key examples, deferred
exclusions, and rejection constraints. No plan, return, product, or proof.

**Reviewer agent:** d8bdb8f0-1544-4288-ada0-f44a52990a72

**Qualifications retained:** Q.1 quiet-load decisive; Q.2 incremental cannot
waive cumulative; Q.3 no repeated retime of same failing candidate without
product or contract-compatible measurement change.

**Journey examples retained (illustrative):** J.1 member `.catch` / `?.catch`
NS values; J.2 attempt5 incremental pass / cumulative api-parameters fail blocks
merge; J.3 all gates pass → merge + acknowledgement, else stay open.

**Rejection constraints retained:** R.1–R.5 as in the seed.

**Exclusions / deferred retained:** D.1 other language readers; D.2 other open
FabioMalpezzi PRs; D.3 changing real-structure NS counting; D.4 parallel nesting
counter; D.5 relaxing &lt;3% / frozen-baseline contract for this merge.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1a.1 | A structure word that appears immediately after `.` or `?.` SHALL NOT be counted as a nested structure. | slice 2 | observed: test/testJavaScriptNestedStructures.py::test_member_named_like_a_structure_is_not_a_structure + lizard_ext/lizardns.py _member_name |
| 1a.2 | Genuine statement-level structures (including real `if` and `try` / `catch`, and analogous real control structures) SHALL still contribute to nested-structure (NS) counts as required. | slice 2 | observed: same test: real if/try/catch still raise NS (retry=1 with nested if) |
| 2a.1 | Public analysis for the story’s key fixtures SHALL match the named test’s expected function names and NS values (`test_member_named_like_a_structure_is_not_a_structure`), including the exemplar ordering/labels given in the key example. | slice 2 | observed: test_member_named_like_a_structure_is_not_a_structure expected names/NS list |
| 3a.1 | The behavioral starting point SHALL be the as-authored `_member_name` handling after `.` / `?.` in `lizard_ext/lizardns.py`. | slice 2 | observed: lizard_ext/lizardns.py as-authored _member_name after . / ?. |
| 3a.2 | The named member-vs-structure test SHALL remain the discriminating test anchor. | slice 2 | observed: test_member_named_like_a_structure_is_not_a_structure present |
| 3a.3 | Additional product changes beyond that patch ARE IN SCOPE when needed so that required correctness and performance gates pass. | slice 2 | observed: no extra product beyond pr.diff; six gates passed without further NS changes |
| 4a.1 | Focused NS / JS / TS coverage SHALL demonstrate the change is exercised. | slice 2 | observed: focused pytest test/testJavaScriptNestedStructures.py — 4 passed (seed035-slice2/focused-ns.log) |
| 4a.2 | Full `pytest`, `make`, and required hosted CI SHALL pass on the exact revision that is merged. | slice 2 | observed: full pytest 3038 passed + make exit 0 (seed035-slice2/full-pytest.log, make.log); hosted CI on published head |
| 5a.1 | Measurement SHALL use the installed paired benchmark with `--extension ns`. | slice 2 | observed: paired_benchmark.py --extension ns under seed035-slice2/ |
| 5a.2 | Required workloads SHALL include `api`, `api-parameters`, and `cli` (all three). | slice 2 | observed: api, api-parameters, cli each incremental+cumulative JSON in seed035-slice2/ |
| 5a.3 | For each required workload, results SHALL include incremental comparison versus the current target and cumulative comparison versus the frozen accepted-set baseline. | slice 2 | observed: six JSON files: incremental-* and cumulative-* vs current master / frozen 4a05e405 |
| 5a.4 | For every required workload, each paired comparison’s 95% upper bound on slowdown SHALL be strictly below 3%. | slice 2 | observed: all six paired_95pct_ci upper strictly < 3% (NOTES.md table) |
| 5a.5 | This contract explicitly includes cumulative `api-parameters` against the frozen accepted-set baseline (not optional and not waivable by incremental-only results). | slice 2 | observed: seed035-slice2/cumulative-api-parameters.json CI [+1.860%, +2.177%] |
| 5a.6 | Incomplete, contaminated, or otherwise non–quiet-load decisive runs SHALL not count as a pass (the narrative cites unfinished cumulative `cli` in attempt5 as an example of a non-passing run). | slice 2 | observed: seed035-slice2/cumulative-cli.json quiet-load decisive pass |
| 6a.1 | PR #554 (or an equivalent exact-head revision carrying the same obligations) SHALL be landed only after commitments 4 and 5 (including all sub-items) are satisfied. | landing | |
| 6a.2 | After merge, the contributor SHALL receive acknowledgement of the landed work. | reporting | |
| 7a.1 | This work is the follow-up because the SEED-034 ≤15-minute product budget could not clear the cumulative overage; the Fabio cohort complete-cycle performance contract applies to this story’s required workloads. | slice 2 | observed: PLAN Current decisions + seed035-slice2 NOTES under Fabio cohort contract |
