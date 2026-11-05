# Source review — SEED-035#publish-lizard-ci-proof

**Launch:** fresh general-purpose reviewer; sole input was the selected story
section Goal, Scope, key examples, exclusions, outcome, and acceptance from
`.planning/seeds/SEED-035-publish-lizard-ci-proof.md#publish-lizard-ci-proof`.
No plan, return, product, proof, or current-slice scope was supplied. Reviewer
read no other project files.

**Input:** story text as in that seed section at acceptance of slice 1
(identity SEED-035#publish-lizard-ci-proof).

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | A Lizard developer looking at a Python application run in Intent's CI status tab sees, beside the run's verdict, how many analyzer tests (`test`) that run passed, failed, or did something else. | slice 1 | observed: Intent CI status tab (story branch cursor/publish-lizard-ci-proof-for-intent): test: 3021 passed and 7 other checks; 1 of 1 part present. Run 42016250359 at c7643ce. |
| 2 | That same view shows how many website scenarios (`website/frontend/features`) that run passed, failed, or did something else. | slice 2 |  |
| 3 | That same view shows plainly when the website part was not run. | slice 2 |  |
| 4 | This replaces reading job logs to learn what a run proved, and gives the dashboard's CI proof its first live Lizard use. | slice 2 |  |
| 5 | `.planning/open-dough.json` declares `test` as a `python` Intent location, ahead of or beside the existing `website/frontend/features` location. | slice 1 | observed: .planning/open-dough.json intent.locations[0] kind=python path=test before gherkin |
| 6 | That `test` location selects pytest's own files (`setup.cfg` sets `python_files = test*`). | slice 1 | observed: .planning/open-dough.json include **/test*.py |
| 7 | The existing Gherkin location, its proof directory, and its proof generation command stay unchanged. | slice 1 | observed: .planning/open-dough.json website/frontend/features proof+proofGeneration unchanged |
| 8 | One named matrix leg of the `build` job is the representative leg. | slice 1 | observed: .github/workflows/python-app.yml representative-leg comment + matrix condition |
| 8.1 | Only that leg writes the `test` fragment. | slice 1 | observed: python-app.yml fragment steps gated to ubuntu-24.04/3.13 only |
| 8.2 | The fragment is written from a per-test pytest report. | slice 1 | observed: pytest --junitxml + junit-outcomes.py → fragment --outcomes |
| 8.3 | The fragment is published as part 1 of 1. | slice 1 | observed: fragment --part 1 --of 1 |
| 9 | Recommended representative leg: `ubuntu-24.04` with Python `3.13`; the workflow names that leg where the fragment step's condition is written. | slice 1 | observed: python-app.yml if matrix.os==ubuntu-24.04 && python-version==3.13 |
| 10 | The `web` job writes the `website/frontend/features` fragment. | slice 2 |  |
| 10.1 | From a per-scenario Cucumber report of its existing browser test step. | slice 2 |  |
| 10.2 | As part 1 of 1. | slice 2 |  |
| 10.3 | Cucumber's own scenario results are the unit counted. | slice 2 |  |
| 10.4 | The separate `test:proof` Node tests are not an Intent location and are not counted. | slice 2 |  |
| 11 | One collector job in `python-app.yml` runs after `build` and `web` however they ended, including when `web` was skipped. | slice 2 |  |
| 12 | That collector expects both entries with one part each. | slice 2 |  |
| 13 | That collector publishes `ci-proof-summary-attempt-<n>`. | slice 1 | observed: gh artifact ci-proof-summary-attempt-1 on run 42016250359; Intent Proof heading shows Attempt 1 of that run |
| 14 | The CI scripts the recipe names (the four collector modules and the report conversions) live in this repository, and the workflow runs those copies. | slice 2 |  |
| 15 | Fragments are written after failing tests too, so failures appear in the counts as well as the verdict. | slice 1 | observed: fragment/keep if !cancelled() && steps.pytest.outcome != skipped; local failed→failed in junit-outcomes.py |
| 16 | Only the representative leg reports `test`. | slice 1 | observed: only representative leg writes fragment artifact |
| 17 | Use the installed CI proof recipe with the judged workflow. | slice 1 | observed: scripts/ci-proof copies + python-app.yml fragment/summarize per recipe |
| 18 | Keep the coverage of the existing matrix and the condition for the website job. | slice 1 | observed: matrix and web needs/if unchanged; test_web_changes+testPythonVersionClaims pass |
| 19 | Use the configured `website/frontend/features` location when the conditional browser job runs. | slice 2 |  |
| E1 | Website covered, all passing: push changing `python-app.yml` runs build/changes/web → summary has suite-sized `test` and scenario-sized website counts; tab shows both. | slice 2 |  |
| E2 | Website skipped: push changing only `lizard_languages/` → collector runs; website part missing; tab shows absent not zero. | slice 2 |  |
| E3 | Failing analyzer test on representative leg → leg and run fail; `test` failed count includes it. | slice 1 | observed: junit-outcomes.py maps failure/error to failed; fragment after !cancelled() |
| E4 | Failure only on another leg → run verdict fails; `test` counts from representative leg may be all passed or missing if cancelled. |  | X1-excl |
| E5 | Rerun → collector reads only new attempt's fragments; publishes `ci-proof-summary-attempt-2`. | slice 1 | observed: artifact names include github.run_attempt; summarize --attempt |
| X1 | Counts from non-representative legs are not undertaken. |  | X1-excl |
| X2 | Committed pytest proof for `test` is not undertaken. |  | X2-excl |
| X3 | Changing the matrix, fail-fast, website change detection, or web job condition is not undertaken. |  | X3-excl |
| X4 | Retained Cucumber proof under `website/frontend/proof` stays as it is. |  | X4-excl |
| X5 | Adapter-based CI proof path does not apply. |  | X5-excl |
