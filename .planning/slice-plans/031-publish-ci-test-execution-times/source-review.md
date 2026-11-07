# Canonical story review

Fresh reviewer `/root/source_review` ran without inherited conversation.
Its launch received only the canonical story's beneficiary, goal, scope,
key examples and exclusions. The reviewer had no plan, product or proof
input and made no edits. The commitments below retain its numbered return.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| G01 | The beneficiary is a Lizard maintainer comparing Python and website test runtimes. | slice 2 | observed: Intent CI status for run 42382227121 at b10a6fd showed analyzer 13.3 s and website 25.5 s, matching the exact artifact milliseconds after display rounding |
| G02 | The maintainer views a judged `python-app.yml` attempt in Intent’s **CI status** tab. | slice 1 | observed: Intent CI status on the selected story branch showed passed run 42381491501 at 0ef4807, 3,093 passed and 7 other checks, and 12.2 s |
| G03 | Each reporting proof entry shows cumulative summed test execution time. | slice 2 | observed: Intent CI status for run 42382227121 at b10a6fd showed analyzer 13.3 s and website 25.5 s, matching the exact artifact milliseconds after display rounding |
| G04 | Each reporting proof entry shows per-file counts. | slice 2 | observed: Intent branch comparison file rows on b10a6fd showed test_ci_proof.py 4 checks and 189 ms plus access.feature 3 checks and 803 ms, matching both runner reports and the summary |
| G05 | Each reporting proof entry shows per-file times. | slice 2 | observed: Intent branch comparison file rows on b10a6fd showed test_ci_proof.py 4 checks and 189 ms plus access.feature 3 checks and 803 ms, matching both runner reports and the summary |
| G06 | The maintainer can compare `test` with `website/frontend/features` without opening job logs. | slice 2 | observed: Intent CI status for run 42382227121 at b10a6fd showed analyzer 13.3 s and website 25.5 s, matching the exact artifact milliseconds after display rounding |
| G07 | If `web` skips, website proof remains explicitly missing. | slice 2 | |
| R01 | Follow the installed CI proof recipe. | slice 2 | observed: Both producer steps in python-app.yml supply --outcomes --times --by-file to the existing installed-recipe copies and the dual-converter replay exercises these flags |
| R02 | Re-copy all five collector modules unchanged into `scripts/ci-proof/`. | slice 1 | observed: Byte comparison of each scripts/ci-proof/ci-proof-{summary,contract,figures,fragments,collect}.mjs with Open Dough v0.3.89 installed scripts, plus the diff against the old copies |
| R03 | Include `ci-proof-figures.mjs` in that copy. | slice 1 | observed: Byte comparison of each scripts/ci-proof/ci-proof-{summary,contract,figures,fragments,collect}.mjs with Open Dough v0.3.89 installed scripts, plus the diff against the old copies |
| R04 | Replace the older count-only copies from SEED-035. | slice 1 | observed: Byte comparison of each scripts/ci-proof/ci-proof-{summary,contract,figures,fragments,collect}.mjs with Open Dough v0.3.89 installed scripts, plus the diff against the old copies |
| R05 | Keep schema versions at `1`. | slice 1 | observed: Mixed-duration replay asserts schemaVersion 1 in the collector output and existing contract versions in ci-proof-contract.mjs |
| R06 | Use the recipe’s optional `durationMs` field for timings. | slice 1 | observed: Mixed-duration replay asserts 375 ms from two available durations including the failed test, on entry, part and test/test_example.py |
| R07 | Keep the existing judged workflow, `python-app.yml`. | slice 1 | observed: Verified workflow selector python-app.yml and both existing intent.locations in .planning/open-dough.json |
| R08 | Keep the existing Intent locations. | slice 1 | observed: Verified workflow selector python-app.yml and both existing intent.locations in .planning/open-dough.json |
| R09 | Keep `ubuntu-24.04` / `3.13` as the representative Python matrix leg. | slice 1 | observed: Both Python fragment steps in .github/workflows/python-app.yml require ubuntu-24.04 and 3.13 and the matrix contract test passes |
| R10 | Keep the conditional `web` job. | slice 2 | observed: The workflow diff preserves the existing web job gate needs.changes.outputs.web == 'true'; test/test_web_changes.py passes |
| R11 | Keep the collector job. | slice 1 | observed: The ci-proof job in .github/workflows/python-app.yml collects both expected entries and uploads ci-proof-summary-attempt-<n> |
| R12 | Keep the artifact naming pattern `ci-proof-summary-attempt-<n>`. | slice 1 | observed: The ci-proof job in .github/workflows/python-app.yml collects both expected entries and uploads ci-proof-summary-attempt-<n> |
| R13 | Extend the pytest JUnit converter to emit a `--times` file. | slice 1 | observed: test/test_ci_proof.py verifies JUnit time strings in seconds are emitted unchanged and the count-only invocation still works |
| R14 | Extend the Cucumber JSON converter to emit a `--times` file. | slice 2 | observed: Both converter CLI tests and the dual-converter replay assert seconds TSV using the exact repository-relative outcome path and no line for omitted times |
| R15 | The specified timing record is one `<seconds>	<repository-relative check path>` line per check. The partial-report example separately qualifies how checks without a duration are counted. | slice 2 | observed: Both converter CLI tests and the dual-converter replay assert seconds TSV using the exact repository-relative outcome path and no line for omitted times |
| R16 | Timing records use the same check path labels as the outcomes file. | slice 2 | observed: Both converter CLI tests and the dual-converter replay assert seconds TSV using the exact repository-relative outcome path and no line for omitted times |
| R17 | Pytest’s JUnit `time` values are already in seconds. | slice 1 | observed: test/test_ci_proof.py verifies JUnit time strings in seconds are emitted unchanged and the count-only invocation still works |
| R18 | Convert Cucumber JSON `result.duration` from nanoseconds to seconds. | slice 2 | observed: test_cucumber_outcomes_keep_failures_and_untimed_scenarios asserts the sum of before, step, and after values as 0.875 seconds and preserves zero |
| R19 | Cucumber scenario time sums available before-hook durations. | slice 2 | observed: test_cucumber_outcomes_keep_failures_and_untimed_scenarios asserts the sum of before, step, and after values as 0.875 seconds and preserves zero |
| R20 | The same scenario sum includes available step durations. | slice 2 | observed: test_cucumber_outcomes_keep_failures_and_untimed_scenarios asserts the sum of before, step, and after values as 0.875 seconds and preserves zero |
| R21 | The same scenario sum includes available after-hook durations. | slice 2 | observed: test_cucumber_outcomes_keep_failures_and_untimed_scenarios asserts the sum of before, step, and after values as 0.875 seconds and preserves zero |
| R22 | Both producer fragment steps pass `--times`. | slice 2 | observed: Both producer steps in python-app.yml supply --outcomes --times --by-file to the existing installed-recipe copies and the dual-converter replay exercises these flags |
| R23 | Both producer fragment steps pass `--by-file`. | slice 2 | observed: Both producer steps in python-app.yml supply --outcomes --times --by-file to the existing installed-recipe copies and the dual-converter replay exercises these flags |
| R24 | Each present part carries entry-level `durationMs` under the recipe’s optional timing contract. | slice 2 | observed: Intent CI status for run 42382227121 at b10a6fd showed analyzer 13.3 s and website 25.5 s, matching the exact artifact milliseconds after display rounding |
| R25 | Each present part carries a file breakdown Intent can display. | slice 2 | observed: Intent branch comparison file rows on b10a6fd showed test_ci_proof.py 4 checks and 189 ms plus access.feature 3 checks and 803 ms, matching both runner reports and the summary |
| R26 | A run that skips `web` still publishes the summary. | slice 2 | |
| R27 | In that summary, the website part is explicitly missing. | slice 2 | |
| R28 | In that case, the Python part may still carry times. | slice 2 | |
| R29 | In that case, the Python part may still carry a file breakdown. | slice 2 | |
| R30 | Only the representative Python leg reports `test` timings. Reporting from every leg would inflate totals. | slice 1 | observed: Both Python fragment steps in .github/workflows/python-app.yml require ubuntu-24.04 and 3.13 and the matrix contract test passes |
| R31 | Keep the existing summary schema. | slice 1 | observed: Mixed-duration replay asserts schemaVersion 1 in the collector output and existing contract versions in ci-proof-contract.mjs |
| R32 | Keep Intent unchanged. | slice 2 | observed: Aggregate implementation diff from 8a0ae5775 touches only workflow, CI scripts, converter tests and planning records; Intent renderer is exercised directly |
| R33 | Use the existing recipe contract for `fragment --by-file`. | slice 2 | observed: Both producer steps in python-app.yml supply --outcomes --times --by-file to the existing installed-recipe copies and the dual-converter replay exercises these flags |
| E01 | A push runs both `build` and `web`. | slice 2 | observed: GitHub Actions run 42382227121 completed build, web and collector; gh run download retrieved ci-proof-summary-attempt-1 and both exact source reports |
| E02 | The maintainer downloads the `ci-proof-summary-attempt-<n>` artifact. | slice 2 | observed: GitHub Actions run 42382227121 completed build, web and collector; gh run download retrieved ci-proof-summary-attempt-1 and both exact source reports |
| E03 | The maintainer views the judged attempt in Intent. | slice 2 | observed: Intent CI status for run 42382227121 at b10a6fd showed analyzer 13.3 s and website 25.5 s, matching the exact artifact milliseconds after display rounding |
| E04 | For both `test` and `website/frontend/features`, the downloaded summary and Intent show passed counts matching the runner reports. | slice 2 | observed: Run 42382227121 Intent CI status and downloaded artifact show 3,095 passed and 7 other for test and 16 passed for website, zero failures, matching independent report aggregation |
| E05 | The same two entries show matching failed counts. | slice 2 | observed: Run 42382227121 Intent CI status and downloaded artifact show 3,095 passed and 7 other for test and 16 passed for website, zero failures, matching independent report aggregation |
| E06 | The same two entries show matching other counts. | slice 2 | observed: Run 42382227121 Intent CI status and downloaded artifact show 3,095 passed and 7 other for test and 16 passed for website, zero failures, matching independent report aggregation |
| E07 | The same two entries show matching `durationMs`. | slice 2 | observed: Intent CI status for run 42382227121 at b10a6fd showed analyzer 13.3 s and website 25.5 s, matching the exact artifact milliseconds after display rounding |
| E08 | At least one file from each entry shows matching counts. | slice 2 | observed: Intent branch comparison file rows on b10a6fd showed test_ci_proof.py 4 checks and 189 ms plus access.feature 3 checks and 803 ms, matching both runner reports and the summary |
| E09 | At least one file from each entry shows matching time. | slice 2 | observed: Intent branch comparison file rows on b10a6fd showed test_ci_proof.py 4 checks and 189 ms plus access.feature 3 checks and 803 ms, matching both runner reports and the summary |
| E10 | A push skips `web`. | slice 2 | |
| E11 | The summary and Intent show Python times in the example. | slice 2 | |
| E12 | The summary and Intent show the Python file breakdown in the example. | slice 2 | |
| E13 | The summary and Intent show `website/frontend/features` as missing proof. Zero duration would be a different state. | slice 2 | |
| E14 | A report for a present part omits duration on some checks. | slice 1 | observed: Mixed-duration replay uses a JUnit report with passed, failed and skipped checks including one omitted time and observes a present part with the original counts |
| E15 | The file breakdown counts those checks as untimed. |  | G1 |
| E16 | The part still publishes. | slice 1 | observed: Mixed-duration replay uses a JUnit report with passed, failed and skipped checks including one omitted time and observes a present part with the original counts |
| E17 | The part’s `durationMs` reflects only timed checks. | slice 1 | observed: Mixed-duration replay asserts 375 ms from two available durations including the failed test, on entry, part and test/test_example.py |
| E18 | A check fails on a reporting job. | slice 1 | observed: Mixed-duration replay uses a JUnit report with passed, failed and skipped checks including one omitted time and observes a present part with the original counts |
| E19 | The fragment still uploads under `!cancelled()`. | slice 2 | observed: Each existing fragment artifact upload still runs under !cancelled() with a non-skipped test step, including failure; the local failed-check fixtures show that such a report produces a fragment with the failed outcome and duration |
| E20 | The failure appears in the counts. | slice 1 | observed: Mixed-duration replay uses a JUnit report with passed, failed and skipped checks including one omitted time and observes a present part with the original counts |
| E21 | Available durations still contribute to entry-level `durationMs`. | slice 1 | observed: Mixed-duration replay asserts 375 ms from two available durations including the failed test, on entry, part and test/test_example.py |
| E22 | Available durations still contribute to the file row. | slice 1 | observed: Mixed-duration replay asserts 375 ms from two available durations including the failed test, on entry, part and test/test_example.py |
| A01 | Intent’s CI status tab already renders optional `durationMs` supplied in the summary. | slice 1 | observed: Intent CI status on the selected story branch showed passed run 42381491501 at 0ef4807, 3,093 passed and 7 other checks, and 12.2 s |
| A02 | Intent already renders supplied per-file breakdowns. The story calls for confirmation of the existing rendering on a real run during execution. | slice 1 | observed: Intent test scope CI proof table for branch 0ef4807 showed test_ci_proof.py with 2 checks and 40 ms |
| A03 | The developer’s `gh` can read the repository’s Actions artifacts. | slice 1 | observed: gh run download 42381491501 retrieved ci-proof-summary-attempt-1 and ci-proof-source-pytest-attempt-1, which the independent verifier read |
| X01 | Wall-clock job duration is outside this increment. `durationMs` is summed test execution time from runner reports. | slice 2 | observed: JUnit testcase and Cucumber hook and step duration values are used; the dual-converter replay asserts their summed execution times |
| X02 | Wall-clock step duration is outside this increment. | slice 2 | observed: JUnit testcase and Cucumber hook and step duration values are used; the dual-converter replay asserts their summed execution times |
| X03 | Timing publication from non-representative Python matrix legs is outside this increment. | slice 1 | observed: Both Python fragment steps in .github/workflows/python-app.yml require ubuntu-24.04 and 3.13 and the matrix contract test passes |
| X04 | Publishing through a `ciAdapter` is outside this increment. The project has none. | slice 2 | observed: .planning/open-dough.json selects python-app.yml without a ciAdapter and the upload is a GitHub Actions artifact |
| X05 | Matrix changes are outside this increment. | slice 2 | observed: The diff preserves the matrix and scripts/web_changes.py; both workflow contract test files passed |
| X06 | Website change-detection changes are outside this increment. | slice 2 | observed: The diff preserves the matrix and scripts/web_changes.py; both workflow contract test files passed |
| X07 | Changes to the `web` condition are outside this increment. | slice 2 | observed: The workflow diff preserves the existing web job gate needs.changes.outputs.web == 'true'; test/test_web_changes.py passes |
| X08 | Intent UI work beyond its existing timing and file rendering is outside this increment. | slice 2 | observed: Aggregate implementation diff from 8a0ae5775 touches only workflow, CI scripts, converter tests and planning records; Intent renderer is exercised directly |
| D01 | Cross-leg views of these timings are deferred. | slice 2 | observed: The aggregate implementation publishes only the existing representative-leg and website attempt summaries and their per-file figures |
| D02 | Historical trend views are deferred. | slice 2 | observed: The aggregate implementation publishes only the existing representative-leg and website attempt summaries and their per-file figures |
| D03 | Displaying separate setup/install time beside test execution time is deferred. | slice 2 | observed: The aggregate implementation publishes only the existing representative-leg and website attempt summaries and their per-file figures |
