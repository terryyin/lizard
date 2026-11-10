# Source review: SEED-052#publish-explicit-ci-test-counts

**Launch:** Fresh general-purpose reviewer with no conversation inheritance;
supplied only the selected story Goal, Scope, key examples and exclusions from
`seeds/SEED-052-publish-explicit-ci-test-counts.md#publish-explicit-ci-test-counts`.
No plan, product, proof, or current-slice scope was provided. Instruction:
use only those inputs; read no other project files; change nothing; return
numbered commitments with compound items split.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | Lizard maintainers can see how many analyzer test cases and website scenarios the selected CI attempt recorded in Intent. | slice 2 |  |
| 2 | Maintainers can compare case counts between builds that both declare the case unit. | slice 2 |  |
| 3 | Adopt explicit test case units in the existing CI proof publisher for the representative ubuntu-24.04 / Python 3.13 analyzer pytest leg and the website Cucumber job. | slice 2 | observed: workflow --unit case on both producers + expect :case; pipeline summary schema 2 |
| 3a | Explicit case units for the representative ubuntu-24.04 / Python 3.13 analyzer pytest leg. | slice 2 | observed: analyzer fragment --unit case; test_workflow_wires + pipeline proof |
| 3b | Explicit case units for the website Cucumber job. | slice 2 | observed: website fragment --unit case; test_workflow_wires + pipeline proof |
| 4 | Each pytest result is one case, including failed and other outcomes. | slice 2 | observed: pipeline pytest cases including failed/other as cases |
| 5 | Each Cucumber scenario result is one case, including failed and other outcomes. | slice 2 | observed: pipeline cucumber scenarios including failed/other as cases |
| 6 | Preserve the configured locations `test` and `website/frontend/features`, outcomes, file breakdowns, available execution times, and attempt/revision identity. | slice 2 | observed: pipeline preserves locations outcomes files times attempt/revision identity |
| 6.1 | Preserve location `test`. | slice 2 | observed: pipeline entry path test |
| 6.2 | Preserve location `website/frontend/features`. | slice 2 | observed: pipeline entry path website/frontend/features |
| 6.3 | Preserve outcomes. | slice 2 | observed: pipeline outcomes 2/1/1 and 1/1/1 |
| 6.4 | Preserve file breakdowns. | slice 2 | observed: pipeline by-file breakdown two analyzer files |
| 6.5 | Preserve available execution times. | slice 2 | observed: pipeline durationMs 375 and 1000 with untimed retained |
| 6.6 | Preserve attempt/revision identity. | slice 2 | observed: pipeline repository/runId/runAttempt/revision match IDENTITY |
| 7 | Use Open Dough’s released CI proof copy recipe. | slice 2 | observed: seven modules copied from installed dough-test-optimization recipe |
| 8 | Use its seven collector modules. | slice 2 | observed: ci-proof-{summary,options,contract,evidence,figures,fragments,collect}.mjs present |
| 9 | Without a Lizard-specific format. | slice 2 | observed: cmp identical to recipe; no Lizard-specific collector format |
| 10 | The version prerequisite is 0.3.93 or later. | slice 2 | observed: dough-update VERSION 0.3.94 >= 0.3.93 |
| 11 | This preparation already has 0.3.94 installed. | slice 2 | observed: dough-update VERSION 0.3.94 installed |
| 12 | An older installation is updated with `dough-update` before copying the collector. | slice 2 | observed: no dough-update needed; already 0.3.94 before copy |
| 13 | Both producers declare `case`. | slice 2 | observed: both fragment commands include --unit case |
| 14 | The collector expects one case part for each location. | slice 2 | observed: summarize --expect test=1:case and website/frontend/features=1:case |
| 15 | A job that wrote no report stays a missing part rather than claiming a successful empty run. | slice 2 | observed: test_missing_website_fragment + test_converter_failure |
| 16 | Including a conditionally skipped website job among jobs that wrote no report. | slice 2 | observed: workflow website skip preserved; missing part stays missing |
| 17 | Until both compared builds declare matching units, Intent reads unit-less results as legacy test results and shows no count or test execution time difference. | slice 1 | observed: live Intent CI status legacy labels + deployed overallCountsWords/comparisonEntriesOf legacy-vs-case (PLAN Slice 1 Accepted proof) |
| 17.1 | Unit-less results are read as legacy test results when compared builds do not both declare matching units. | slice 1 | observed: legacy summary read as unit legacy label tests via checkedRetainedSummary/ciProofSchema; live Proof shows legacy test results |
| 17.2 | No count difference is shown across incompatible units. | slice 1 | observed: legacy-vs-case entry cue units not comparable; overall cue no overall count — no invented count delta |
| 17.3 | No test execution time difference is shown across incompatible units. | slice 1 | observed: legacy-vs-case duration cue none/partial; no comparable test execution time difference |
| 18 | Interpreting those unit-less results as cases would create a misleading comparison. | slice 1 | observed: comparison suppresses incompatible units (whyNotCompared / legacy unit unknown) rather than treating legacy as cases |
| 19 | Existing upstream Intent behavior supplies this compatibility. | slice 1 | observed: deployed Open Dough 0.3.94 Intent reader/comparison supplies compatibility without Lizard UI changes |
| 20 | This story changes Lizard’s publisher. | slice 2 | observed: Lizard publisher/workflow/scripts only; Intent UI unchanged |
| 21 | Scope limits: no additional matrix legs, file-unit publication, changes to website job selection, analyzer behavior, or new Intent UI behavior. | slice 2 | observed: workflow selection/retention/artifacts preserved; no matrix/file-unit/analyzer/Intent UI |
| 21.1 | No additional matrix legs. | slice 2 | observed: no additional matrix legs in workflow diff |
| 21.2 | No file-unit publication. | slice 2 | observed: unit case only; no file-unit publication |
| 21.3 | No changes to website job selection. | slice 2 | observed: website job selection conditions unchanged |
| 21.4 | No analyzer behavior changes. | slice 2 | observed: no analyzer product code changes |
| 21.5 | No new Intent UI behavior. | slice 2 | observed: no Intent UI changes in this repo |
| 22 | Given four pytest results in two files (two passed, one failed, one skipped), publishing the representative leg reports four cases at `test`, with outcomes 2 passed / 1 failed / 1 other, retaining the two-file breakdown and available timings. | slice 2 | observed: test_pipeline_publishes_explicit_case_evidence_for_both_producers analyzer side |
| 22.1 | Four cases at `test` for four pytest results (2 passed / 1 failed / 1 skipped → other). | slice 2 | observed: analyzer evidence 2 passed 1 failed 1 other |
| 22.2 | Retains the two-file breakdown and available timings. | slice 2 | observed: two-file breakdown and timings retained |
| 22.3 | Does not report two test files as the case count. | slice 2 | observed: case counts not file counts |
| 22.4 | Does not multiply the count across matrix legs. | slice 2 | observed: representative leg only; no matrix multiply |
| 23 | Given a Cucumber report with a background and three scenarios (passed, failed, skipped), publication reports three cases at `website/frontend/features`, with outcomes 1 / 1 / 1. | slice 2 | observed: pipeline cucumber three scenarios 1/1/1 |
| 23.1 | Three cases at `website/frontend/features` for three scenarios; outcomes 1 passed / 1 failed / 1 other. | slice 2 | observed: website evidence 1 passed 1 failed 1 other |
| 23.2 | The background and individual steps are not additional cases. | slice 2 | observed: background excluded from case count |
| 23.3 | Available hook and step times remain part of each scenario’s execution time. | slice 2 | observed: hook/step durations in scenario execution time (1000 ms) |
| 24 | Given both reports for one attempt, its summary and Intent agree with the runner case totals and outcomes for both locations. | slice 2 |  |
| 25 | A missing report leaves that location’s declared case part missing and uncounted. | slice 2 | observed: missing website fragment keeps declared case part missing |
| 26 | Given two migrated builds whose case totals change from 4 to 5 and from 3 to 4, their Intent comparison shows +1 case at each location. | slice 2 |  |
| 27 | Given an older unit-less build and a migrated build, the older results remain legacy results and neither a count nor a test execution time difference is shown across the incompatible units. | slice 1 | observed: legacy schema-1 vs case branch: overall and entry rows keep legacy labelling and suppress count/time deltas (PLAN Slice 1 Accepted proof) |
