# CI proof summary

Intent's **CI status** tab can show, below the verdict of the shown CI
result, which of this project's Intent locations that result proved: passed,
failed and other counts per location, and any part of it that did not report.
It reads one small summary that this project's own CI writes: a GitHub Actions
workflow publishes it as an artifact of the shown run, and a project whose CI
is read through a `ciAdapter` serves it from that adapter for the shown
attempt. Present parts and their entries may carry optional `durationMs`
(summed test execution time in milliseconds); a part without one carries
none, and schema versions stay at 1. With `fragment --by-file`, each entry is
also broken down by test file, so selecting an entry or folder in Intent
shows that scope's figures and its immediate folders' and files'; without it,
Intent shows entry totals and says the breakdown is unavailable. Use this reference when the request is
to make this project's CI publish that summary, or when the tab says the
shown run or attempt has no CI proof summary.

Every change is to this project's own workflow, CI scripts or CI adapter.

## Check the project first

Resolve these from this project before editing its CI:

- **Proof entries.** `.planning/open-dough.json` `intent.locations[].path`, in
  that order. Each proved path is one proof entry (written entry below),
  spelled exactly as configured. Without configured Intent locations, stop:
  the tab shows proof only for them, so name the missing setting instead of
  inventing entries.
- **Judged CI.** When `.planning/open-dough.json` sets a nonempty
  `ciAdapter`, the tab judges the attempts that adapter discovers: follow
  steps 1 and 2, then [publish through a CI adapter](#publish-through-a-ci-adapter)
  in place of steps 3 to 6. Otherwise the tab judges `ciWorkflow` (default
  `ci.yml`) under `.github/workflows/`, and the summary must come from a run
  of that workflow. When neither applies, because the CI is not GitHub
  Actions and no `ciAdapter` reads it, stop and say the tab cannot show proof
  for it until the project reads its CI through a
  [CI command adapter](../../dough-execute-plan/manuals/custom-ci.md).
- **Per-test results.** For each entry, the CI job and test command that run
  its tests, and a report file that command can write with one result per test
  or check. An exit status alone proves nothing per test: name that entry as
  not provable by its current command and leave it out, or stop for the
  developer when no entry is provable.
- **Node.js 20.12 or later** wherever the collector runs (on GitHub
  Actions, `actions/setup-node` on the producer and collector runners). The
  collector uses only Node built-ins.

## How the summary is made

On GitHub Actions:

1. Each job part that runs an entry's tests converts its report into one
   **fragment** per entry and keeps them as an artifact named for its job,
   part and run attempt.
2. One **collector** job, after those jobs end however they ended, compares
   this attempt's fragments with the declared entries and part counts and keeps
   the summary as `ci-proof-summary-attempt-<n>`.
3. The tab downloads that artifact with the developer's `gh`. It uses the
   summary only when its repository, run id, attempt and revision equal the
   shown run's. A declared part without a fragment is shown as missing, a part
   reported twice as ambiguous, and neither is counted.

Through a CI adapter, the attempt's own CI job writes the fragments and the
summary, named by the adapter's `attemptId`, and the adapter answers the
tab's `proof` request with that summary. The tab uses it only when its
repository, `attemptId` and revision equal the shown attempt's, and shows
missing and ambiguous parts the same way.

## 1. Copy the collector into this project

Copy these five modules, unchanged and side by side, from this skill's
installed `scripts/` directory into the directory this project keeps CI
scripts in (written `<ci-scripts>` below), and commit them:

- [ci-proof-summary.mjs](../scripts/ci-proof-summary.mjs), the command line;
- [ci-proof-contract.mjs](../scripts/ci-proof-contract.mjs);
- [ci-proof-figures.mjs](../scripts/ci-proof-figures.mjs);
- [ci-proof-fragments.mjs](../scripts/ci-proof-fragments.mjs);
- [ci-proof-collect.mjs](../scripts/ci-proof-collect.mjs).

The CI job runs these copies, never a path inside the installed skill. When
a later guidance update changes them, copy them again.

## 2. Map test commands to entries

For each entry, record the job that runs its tests and its **parts**: 1 for a
single job, the matrix length for a matrix of shards. Number every part of an
entry once, `1` to `n`: two jobs reporting the same part make it ambiguous.

`fragment` reads one of two report formats:

- `--outcomes FILE`: one `<status><TAB><check path>` line per test or check,
  status `passed`, `failed` or anything else (counted as other). The unit is
  checks. Each line counts toward the `--entry` whose path contains it (the
  longest one), so one job may report several entries. A line in no given
  entry counts toward none. Use it for any runner that can name each test's
  file. Optional `--times FILE` uses the same check labels with one
  `<seconds><TAB><check path>` line per check; each matching entry's fragment
  then carries `durationMs` as the sum of its checks' seconds in milliseconds.
  Without `--times`, or for an entry with no times line, the fragment carries
  no duration.
- `--playwright-record FILE`: JSON
  `{"schemaVersion":1,"phase":"completed","tests":[{"executions":[{"outcome":"expected"}],"attempts":[{"durationMs":42}]}]}`
  for one or more `--entry` paths. With several entries, each test's absolute
  `location.file`, relative to the record's `run.cwd`, counts toward the
  longest containing entry; a test with no usable owning path counts toward
  none. With one entry, the whole record counts toward that entry, including
  tests with no usable file path. The unit is tests. A test passes when every
  outcome is `expected`, fails when any is `unexpected` or `flaky`, and is
  otherwise other. Any other `phase` fails the step and writes no fragment.
  When finished attempts record `durationMs`, each fragment's `durationMs` is
  their sum across its tests (retries included). An entry with no selected
  tests still receives its zero-count fragment. `--times` applies only with
  `--outcomes`.

Add `--by-file` to break each fragment down by test file. Each `--outcomes`
line's check label is then its repository path (as `tests/a.sh`); a label
that is not a relative path inside its entry is counted as unassigned, and a
check with no `--times` line as untimed. A `--playwright-record` test's
file is its absolute `location.file` relative to the record's `run.cwd`, the
directory the run started in (the repository root for `npm run`); a test
whose file is not inside its sole entry under that directory, or that has no
location, is counted as unassigned rather than given a path; several entries
require a usable owning path to assign a test. A test none
of whose attempts recorded `durationMs` is untimed. No machine path is
published. Every count stays accounted for: files plus unassigned equal the
fragment's counts, and `summarize` keeps each part's breakdown, or its
absence, so Intent shows a part without one as a gap.

Write the conversion from the runner's own report into `<ci-scripts>` beside
the collector — for example a JUnit XML `testcase` walk that prints
`<status>\t<file>`, or a Playwright JSON walk into the record shape above.
A run that ended with a global error, or wrote no report, gives no fragment,
so its part shows as missing rather than as a count.

## 3. Write fragments in each producer job, after failures

Set workflow `env.CI_PROOF_REVISION` to
`${{ github.event.pull_request.head.sha || github.sha }}` so a pull request
names its head. Give the test step an `id`, and let a sharded matrix finish
every part with `strategy.fail-fast: false`. After the test step, when
`!cancelled()` and the test step was not `skipped`, convert the report if
needed and run:

```sh
node <ci-scripts>/ci-proof-summary.mjs fragment \
  --outcomes <report> [--times <times>] [--by-file] \
  --entry <entry> [--entry <entry>…] \
  --part <part> --of <parts> --attempt "${GITHUB_RUN_ATTEMPT}" \
  --revision "${CI_PROOF_REVISION}" --out "${RUNNER_TEMP}/ci-proof"
```

`<part>` is `1` and `<parts>` `1` for a single job, or the matrix value and
`${{ strategy.job-total }}` for a matrix. Use `--playwright-record <record>`
instead of `--outcomes` for a Playwright record; name every entry the shared
run proves. Upload
`${{ runner.temp }}/ci-proof/` as artifact
`ci-proof-fragments-${{ github.job }}-<part>-attempt-${{ github.run_attempt }}`
with `if-no-files-found: ignore` and one-day retention (`!cancelled()` so
failing tests still keep fragments). Keep the project's artifact-action pins;
pattern downloads need v4 or later.

## 4. Declare the expected parts and collect

Add a collector job that `needs` every producer, runs when `!cancelled()`,
checks out, sets up Node 20.12+, downloads
`ci-proof-fragments-*-attempt-${{ github.run_attempt }}` into a fragments
directory, then:

```sh
node <ci-scripts>/ci-proof-summary.mjs summarize \
  --fragments <fragments dir> \
  --expect <entry>=<parts> [--expect <entry>=<parts>…] \
  --repository "${GITHUB_REPOSITORY}" --run-id "${GITHUB_RUN_ID}" \
  --attempt "${GITHUB_RUN_ATTEMPT}" --revision "${CI_PROOF_REVISION}" \
  --out "${RUNNER_TEMP}/ci-proof-summary.json"
```

Upload that file as `ci-proof-summary-attempt-${{ github.run_attempt }}`
(`if-no-files-found: error`, retention from step 5). `--expect` lists proved
entries in `intent.locations` order with part counts; fragments present never
add an entry or part. The tab reads only that artifact name holding
`ci-proof-summary.json`, at most 1 MiB; a file breakdown of about 1,400 test
files measured about 170 KB. A larger summary is still written whole, never
truncated, and the collector names its size on stderr. The collector log
prints per-entry counts and gaps plus exclusion notes. A failing collector fails the run but not producer
results. No extra permission or secret is required.

## 5. Choose retention

Fragments need only a day (until the collector runs). Keep the summary as
long as developers should see proof for a shown run — for example 30 days,
within the repository's or organization's maximum artifact retention.

## 6. Grant the dashboard's read access

The tab reads with the developer's `gh` (`gh auth status`); the account needs
Actions read (fine-grained **Actions: Read**, or classic `repo` on a private
repository). Confirm a judged-workflow run lists
`ci-proof-summary-attempt-<n>` and that its zip holds only
`ci-proof-summary.json`. Wrong revision → check `CI_PROOF_REVISION`; missing
part → that part's fragment step, artifact, and collector exclusion notes.

## Publish through a CI adapter

Follow this in place of steps 3 to 6 when `ciAdapter` reads this project's CI.
Inputs: the attempt's `attemptId` and full `sha` as `discover` returns them,
and `OWNER/REPO` as the adapter's `check.repo`. Stop if the job cannot learn
`attemptId` or the full revision.

In the attempt's own job, after tests (pass or fail):

```sh
node <ci-scripts>/ci-proof-summary.mjs fragment \
  --outcomes <report> [--times <times>] [--by-file] \
  --entry <entry> [--entry <entry>…] \
  --part <part> --of <parts> --attempt-id "<attempt id>" \
  --revision "<full revision>" --out <fragments dir>
node <ci-scripts>/ci-proof-summary.mjs summarize \
  --fragments <fragments dir> \
  --expect <entry>=<parts> [--expect <entry>=<parts>…] \
  --repository "<OWNER/REPO>" --attempt-id "<attempt id>" \
  --revision "<full revision>" --out <summary file>
```

Name the attempt only by `--attempt-id` (text form of discovered `attemptId`).
An adapter's `proof` answer is read up to 1 MiB (the same summary read limit
Intent already uses for GitHub Actions summaries), so add `--by-file` to
`fragment` freely while the summary stays within that limit; `summarize`
keeps whatever breakdown the fragments carry. A successful `proof` answer
carries the complete summary as the collector wrote it — never trimmed or
replaced by `unavailable` for size. Without `--by-file`, Intent still shows
entry totals.
`--expect` matches step 4. Keep `<summary file>` where the adapter can read it
per attempt for as long as developers should see that proof. Implement
`proof` per
[CI proof reads](../../dough-execute-plan/manuals/custom-ci-proof.md):
`summaryProblems` empty and `attemptId`/`revision` match, else `unavailable`.
Check with a `proof` stdin request for a discovered attempt; the tab then
shows Proof named by `attemptId` and short revision.
