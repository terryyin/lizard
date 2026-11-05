# CI proof summary

Intent's **CI status** tab can show, below the verdict of the shown GitHub
Actions run, which of this project's Intent locations that run proved: passed,
failed and other counts per location, and any part of it that did not report.
It reads one small summary artifact that this project's own workflow
publishes. Use this reference when the request is to make this project's CI
publish that summary, or when the tab says the shown run published no CI
proof summary.

This works with GitHub Actions only. It adds observations of ordinary CI runs
beside any deliberately retained proof (such as
[Cucumber proof](cucumber-proof.md)), which it leaves untouched. Every change
is to this project's own workflow and CI scripts.

## Check the project first

Resolve these from this project before editing its workflow:

- **Proof entries.** `.planning/open-dough.json` `intent.locations[].path`, in
  that order. Each proved path is one proof entry (written entry below),
  spelled exactly as configured. Without configured Intent locations, stop:
  the tab shows proof only for them, so name the missing setting instead of
  inventing entries.
- **Judged workflow.** The tab judges `.planning/open-dough.json`
  `ciWorkflow` (default `ci.yml`) under `.github/workflows/`. The summary must
  come from a run of that workflow. When the project sets a nonempty
  `ciAdapter`, or its CI is not GitHub Actions, stop and say the tab cannot show
  proof for it.
- **Per-test results.** For each entry, the CI job and test command that run
  its tests, and a report file that command can write with one result per test
  or check. An exit status alone proves nothing per test: name that entry as
  not provable by its current command and leave it out, or stop for the
  developer when no entry is provable.
- **Node.js 20.12 or later** on the producer and collector runners
  (`actions/setup-node`). The collector uses only Node built-ins.

## How the summary is made

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

## 1. Copy the collector into this project

Copy these four modules, unchanged and side by side, from this skill's
installed `scripts/` directory into the directory this project keeps CI
scripts in (written `<ci-scripts>` below), and commit them:

- [ci-proof-summary.mjs](../scripts/ci-proof-summary.mjs), the command line;
- [ci-proof-contract.mjs](../scripts/ci-proof-contract.mjs);
- [ci-proof-fragments.mjs](../scripts/ci-proof-fragments.mjs);
- [ci-proof-collect.mjs](../scripts/ci-proof-collect.mjs).

The workflow runs these copies, never a path inside the installed skill. When
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
  file.
- `--playwright-record FILE`: JSON
  `{"schemaVersion":1,"phase":"completed","tests":[{"executions":[{"outcome":"expected"}]}]}`
  for exactly one `--entry`. The unit is tests. A test passes when every
  outcome is `expected`, fails when any is `unexpected` or `flaky`, and is
  otherwise other. Any other `phase` fails the step and writes no fragment.

Write the conversion from the runner's own report into `<ci-scripts>` beside
the collector. For a JUnit XML report, such as pytest's
`-o junit_family=xunit1 --junitxml=<file>` (whose `file` attribute names each
test's file relative to the repository root), `<ci-scripts>/junit-outcomes.py`:

```python
import sys
import xml.etree.ElementTree as ET

for case in ET.parse(sys.argv[1]).iter("testcase"):
    if case.get("file") is None:
        sys.exit(f"testcase {case.get('name')} names no file")
    failed = case.find("failure") is not None or case.find("error") is not None
    status = "failed" if failed else "other" if case.find("skipped") is not None else "passed"
    print(f"{status}\t{case.get('file')}")
```

For Playwright's JSON reporter (`--reporter=<existing>,json` with
`PLAYWRIGHT_JSON_OUTPUT_NAME` set to an absolute path),
`<ci-scripts>/playwright-record.mjs`:

```js
import { readFileSync, writeFileSync } from "node:fs";
const [reportFile, recordFile] = process.argv.slice(2);
const report = JSON.parse(readFileSync(reportFile, "utf8"));
const tests = [];
const visit = (suite) => {
  for (const spec of suite.specs ?? [])
    for (const test of spec.tests ?? [])
      tests.push({ executions: [{ outcome: test.status }] });
  (suite.suites ?? []).forEach(visit);
};
(report.suites ?? []).forEach(visit);
const phase = (report.errors ?? []).length === 0 ? "completed" : "errored";
writeFileSync(recordFile, JSON.stringify({ schemaVersion: 1, phase, tests }));
```

A run that ended with a global error, or wrote no report, gives no fragment,
so its part shows as missing rather than as a count.

## 3. Write fragments in each producer job, after failures

Name the revision once at the workflow level, so a pull request run names its
head rather than its merge commit:

```yaml
env:
  CI_PROOF_REVISION: ${{ github.event.pull_request.head.sha || github.sha }}
```

Give the test step an `id`, and let a sharded matrix finish every part with
`strategy.fail-fast: false` (a cancelled part shows as missing). After the test
step, in each producer job, `<part>` being `1` and `<parts>` `1` for a single
job, or the matrix value and `${{ strategy.job-total }}` for a matrix:

```yaml
      - name: Write the CI proof fragments
        if: ${{ !cancelled() && steps.<test-step-id>.outcome != 'skipped' }}
        run: |
          <conversion from the runner's report into <report>, if any>
          node <ci-scripts>/ci-proof-summary.mjs fragment \
            --outcomes <report> --entry <entry> [--entry <entry>…] \
            --part <part> --of <parts> --attempt "${GITHUB_RUN_ATTEMPT}" \
            --revision "${CI_PROOF_REVISION}" --out "${RUNNER_TEMP}/ci-proof"
      - name: Keep the CI proof fragments
        if: ${{ !cancelled() }}
        uses: actions/upload-artifact@v4
        with:
          name: ci-proof-fragments-${{ github.job }}-<part>-attempt-${{ github.run_attempt }}
          path: ${{ runner.temp }}/ci-proof/
          if-no-files-found: ignore
          retention-days: 1
```

Use `--playwright-record <record>` and a single `--entry` in place of
`--outcomes` for a Playwright record. The `!cancelled()` condition writes and
keeps fragments after failing tests too; the `skipped` check leaves out a job
whose setup failed before its tests ran, which then shows as missing. The
artifact name stays unique per job part and per attempt, so a rerun never
reads an earlier attempt's fragments. Keep the project's existing pins of the
artifact actions; pattern downloads need v4 or later.

## 4. Declare the expected parts and collect

Add one collector job to the judged workflow. `needs` lists every producer
job, and `if: ${{ !cancelled() }}` runs it after their failures:

```yaml
  ci-proof:
    needs: [<producer jobs>]
    if: ${{ !cancelled() }}
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: <20.12 or later, as the project pins it>
      - uses: actions/download-artifact@v4
        with:
          pattern: ci-proof-fragments-*-attempt-${{ github.run_attempt }}
          path: ${{ runner.temp }}/ci-proof-fragments
      - name: Summarize CI proof
        run: |
          mkdir -p -- "${RUNNER_TEMP}/ci-proof-fragments"
          node <ci-scripts>/ci-proof-summary.mjs summarize \
            --fragments "${RUNNER_TEMP}/ci-proof-fragments" \
            --expect <entry>=<parts> [--expect <entry>=<parts>…] \
            --repository "${GITHUB_REPOSITORY}" --run-id "${GITHUB_RUN_ID}" \
            --attempt "${GITHUB_RUN_ATTEMPT}" --revision "${CI_PROOF_REVISION}" \
            --out "${RUNNER_TEMP}/ci-proof-summary.json"
      - uses: actions/upload-artifact@v4
        with:
          name: ci-proof-summary-attempt-${{ github.run_attempt }}
          path: ${{ runner.temp }}/ci-proof-summary.json
          if-no-files-found: error
          retention-days: <summary retention>
```

The `--expect` declarations are the expectation: list the proved entries in
`intent.locations` order, each with its part count. Fragments present never
add an entry or a part. Keep each count equal to its job's matrix length, for
example with a comment beside the matrix or a project test that reads the
workflow. The tab reads only the artifact named exactly
`ci-proof-summary-attempt-<n>` holding the one `ci-proof-summary.json`
(at most 1 MiB, far above a real summary). The collector's log prints one
line per entry with its counts and gaps, and a note for each fragment it
excluded. A failing collector job fails the run but never changes a producer
job's result. The workflow needs no extra permission or secret.

## 5. Choose retention

Fragments are needed only until the collector job runs, so a day is enough.
Keep the summary as long as developers should see proof for a shown run: an
idle trunk or story branch may keep showing an older run, and an expired
summary is shown as expired. For example, keep it 30 days, within the
repository's or organization's maximum artifact retention and its Actions
storage.

## 6. Grant the dashboard's read access

The tab reads with the developer's own `gh`. It must be signed in
(`gh auth status`) as an account that can read this repository's Actions. A
fine-grained token needs the repository's **Actions: Read** permission; for a
private repository, a classic token needs the `repo` scope. Check it against a
run of the judged workflow:

```sh
gh api "repos/<owner>/<repo>/actions/runs/<run-id>/artifacts?per_page=100" \
  --jq '.artifacts[] | "\(.id) \(.name) expired=\(.expired)"'
gh api repos/<owner>/<repo>/actions/artifacts/<artifact-id>/zip > <scratch-dir>/summary.zip
```

The listing names `ci-proof-summary-attempt-<n>`, and the archive holds only
`ci-proof-summary.json`. Then Intent's **CI status** tab shows Proof for that
run. When it names another revision, check `CI_PROOF_REVISION`; when a part is
missing, check that part's fragment step and artifact, and the collector's
exclusion notes.
