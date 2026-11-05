# Vitest proof

Generate or refresh Vitest proof only when the caller explicitly enables
recording. Normal test runs leave existing proof byte-for-byte untouched,
whether they pass or fail, and create no proof files, directories or locks.
An explicit recording run uses the same tests, selection, projects, browser
instances, retries, sharding, reporters, diagnostics and exit meaning as a
normal run; focused runs are allowed, with no whole-suite requirement. It
maintains one compact snapshot of the test cases it actually ran.

Retain only passing test cases as committed proof, with their time, attempts,
commit and test-file version. During an explicit refresh, an executed final
unsuccessful result clears that case's earlier proof; cases the run did not
execute keep theirs. A pass records proof even if another case fails the run.
Open Dough's Intent reads the snapshot from the project's published revision.
Published proof is the last deliberately retained verification, not the
result of subsequent normal runs or a statement of current product health.

Implement the option and the recorder as a Vitest reporter in this project's
own code; Open Dough ships no recorder. When this project already maintains
another proof snapshot, such as Cucumber proof, the code that maintains it
may maintain this snapshot too.

Before generating proof, resolve the covered `typescript` or `javascript`
Intent location, its proof directory and the actual recording command or
skill from this project's settings. When they are missing, configure them
from the existing Vitest invocation or name the missing project input before
recording; do not silently enable recording in normal commands.

## Configure once

- Choose an explicit project-owned option, for example the environment
  variable `RECORD_VITEST_PROOF=1`, and a command or skill that sets it. Keep
  recording off by default in every profile, including CI: never set the
  option in Vitest configuration, package scripts the project's ordinary
  commands use, CI workflows or the project's default test wrapper. The
  recorder checks the option before any proof read, write, cleanup or lock.
  Merely running tests, optimizing them or using a CI profile never opts in.
- Add the recorder to the configured `reporters` in this project's Vitest
  configuration, beside the reporters already there, so the project's
  ordinary commands, its test wrapper and CI all load it with recording off.
  When the configuration names no reporters, list Vitest's `"default"`
  reporter beside the recorder so existing output stays. Where reporters
  differ by profile (for example a CI-only reporter), add the recorder to
  each profile's list. The recording command then differs from a normal run
  only by the option.
- Run the recording command with the configured reporters: give it no
  command-line `--reporter` option. A command-line `--reporter` replaces the
  configured `reporters` rather than adding to them (observed in Vitest
  5.0.3), so a command that passes one loads no recorder and records
  nothing, even with the option set.
- Keep existing output, warnings, diagnostics, collection, test order,
  projects, browser instances, retries, sharding and the command's exit
  meaning unchanged with the recorder loaded, recording on or off. The
  recorder prints nothing and never fails the run: contain its own errors so
  none surfaces as a Vitest error, an unhandled rejection or a changed exit
  status. When it cannot record, for example outside a Git checkout, it
  records nothing.
- Declare the runner, the proof directory and its generation instruction in
  `.planning/open-dough.json` on every Intent location it covers:

  ```json
  {
    "intent": {
      "locations": [
        {
          "path": "web/tests",
          "kind": "typescript",
          "include": ["**/*.spec.ts"],
          "runner": "vitest",
          "proof": "web/tests/proof",
          "proofGeneration": {
            "command": "RECORD_VITEST_PROOF=1 npm --prefix web test"
          }
        }
      ]
    }
  }
  ```

  A `typescript` or `javascript` location that declares `proof` also
  declares `runner`; `"vitest"` selects this reference's proof format.
  Without a `runner` Intent reads, it explains that setting on the location
  and reads none of its proof.
  `proof` is a directory path from the repository root. Commit its files; do
  not ignore it. `proofGeneration` contains exactly one nonempty string:
  `command`, run from the repository root, or `skill`, the invocation of a
  skill maintained by this project. Use this project's actual command or
  skill, including the explicit recording option and any wrapper,
  environment or selection it needs, as a single run (`vitest run`, not watch
  mode). Intent displays the instruction as text; it does not execute it.
  Missing or invalid generation configuration does not hide readable
  historical proof.

- Resolve the repository root with `git rev-parse --show-toplevel` and read
  the covered locations, those with `runner: "vitest"` and `proof`, from
  `.planning/open-dough.json` there. Record only test cases of files under
  those locations, each into the snapshot of its location's proof directory.
  Cases of other files run in the same session, such as fixture projects in
  temporary directories, are not recorded.
- Test the recorder by running Vitest on fixture projects, each in its own
  temporary Git checkout with its own `.planning/open-dough.json`, setting
  the option only for the inner runs, so the outer test run never records.
  When a test compares a run's output with and without the recorder, mask
  what Vitest prints from timing: durations, start times and its
  worker-startup hint (a line beginning `Isolate` with a following
  `at least` line), which it prints only when worker startup was slow, so
  the hint appears or not with machine load (observed in Vitest 5.0.5).

## Record from the reporter's results

- The recorder runs once per run in the Node process and receives every
  project's, worker's and browser instance's results. Record from the
  reporter's per-case result (in Vitest's reporter API, the test case
  delivered to `onTestCaseResult` with its `result()` and `diagnostic()`).
  Never record from test files, setup files or code running in a worker or
  browser.
- A test case is finished when its final result arrives, retries included.
  Vitest delivers one result per case, after its last attempt (observed in
  Vitest 5.0.3 with `retry: 1`), so intermediate failed attempts never reach
  the reporter and never clear proof; the final result decides.
- A final `passed` result records proof. A final `failed` result is an
  executed unsuccessful result: it clears the case's proof and records
  nothing. Any other result ran nothing and keeps the case's proof: a case
  skipped by `skip`, `skipIf`, `todo`, `only` elsewhere, a runtime
  `context.skip()` or a name or file filter. Cases outside the run's shard,
  cases a bail or interrupted run never reached, and cases of files that
  failed to collect also keep their proof.
- `attempts` is the result's retry count plus one; `1` without retries.
- `durationMs` is the case's duration as the reporter's per-case result
  gives it (`diagnostic().duration`), in milliseconds and often fractional.
  It already covers all of the case's attempts: a case passing on its retry
  reports the time of both attempts (observed in Vitest 5.0.3), so record it
  as given and add nothing.
- An identity executed more than once in one run (for example an `each`
  table whose rows share one name, or a file run twice in one project)
  records one observation that passes only when every execution passed,
  with `attempts` the greatest of them and `durationMs` their sum. Keep the
  run's executions of each identity in memory: the first pass in the run
  replaces any earlier proof, a later pass folds into it, and once one
  execution failed the identity stays cleared for the rest of the run.

## Identity

Identify a case by Vitest's own names, never by its task id, which encodes
collection order:

- `file`: the test module's path relative to the repository root, not
  Vitest's `root` or the working directory, with `/` separators.
- `test`: a list of names from the outermost `describe` or `suite` to the
  test, each as Vitest formatted it, including names `.each` or `.for`
  filled in: `["Cart", "totals", "adds 2 items"]`. Take each name
  from the case and its parent suites; do not split a joined full name,
  since a name may contain its separator.
- `project`: the Vitest project name, only when the run names one, for
  example a browser instance `chromium-touch`. Omit the field when the
  project name is empty.

Give `each` and `for` names placeholders (`%s`, `%i`, `$name` and the like)
where each row should be its own case. A table whose name has no placeholder
runs every row under one identical name, and its rows fold into one
observation.

## Refresh the snapshot

- Keep one maintained snapshot per proof directory, `latest.json`. Do not
  accumulate a file per run. Intent reads a proof file of up to 32 MiB, so
  one snapshot of the whole suite stays readable.
- After every finished executed case, refresh the snapshot atomically: write
  a non-JSON temporary file in the same directory, then rename it over
  `latest.json`. Re-read the directory's proof before each refresh and
  preserve unrelated observations; leave any file without the `.json`
  extension unchanged. A store this project shares with another runner's
  recorder may also fold other `.json` proof files in the directory into
  `latest.json` and remove them, as Intent reads them as one directory. A
  pass replaces or folds into that identity's observation; an executed
  unsuccessful result removes it and persists no failed observation or
  failure marker. Order observations by `file`, then `test`, then `project`
  (a case without one first) to limit diff noise; compare `test` lists name
  by name, a shorter list that is a prefix first. Delete the snapshot when
  it becomes empty rather than writing an empty replacement. An interrupted
  recording run keeps completed updates without claiming the remaining
  cases ran. A shard records only the cases it ran. A full-suite refresh
  rewrites the snapshot once per case; expect that cost in the Node process.
- Serialize proof mutations by concurrent recording runs sharing this
  checkout, so one run cannot lose another's update or restore a removed
  success. Keep any lock file without the `.json` extension and remove it
  when the run ends. Separate worktrees keep separate snapshots; reconcile
  them by identity, never keeping a removed success from another worktree.
- Every `.json` file in the proof directory, or in any directory below it, is
  read as proof. Keep nothing else there with that extension. One file that is
  not valid Vitest proof leaves the whole directory's proof unreadable until
  it is fixed or removed.

## File format

The snapshot is one JSON object:

```json
{
  "version": 1,
  "observations": [
    {
      "file": "web/tests/cart.spec.ts",
      "test": ["Cart", "totals", "adds 2 items"],
      "project": "chromium",
      "outcome": "passed",
      "attempts": 1,
      "durationMs": 42,
      "finishedAt": "2026-11-04T09:00:00.000Z",
      "commit": "a1b2c3d4a1b2c3d4a1b2c3d4a1b2c3d4a1b2c3d4",
      "fileBlob": "3b18e512dba79e4c8300dd08aeb37f8e728b8dad"
    }
  ]
}
```

Each observation has `file`, `test` and, when the run names a project,
`project` as above, and:

- `outcome`: always `passed`.
- `attempts`: retry count plus one, the greatest over executions folded into
  this observation.
- `durationMs`: milliseconds across the case's attempts, possibly
  fractional, summed over executions folded into this observation.
- `finishedAt`: ISO 8601 UTC time the case's final result arrived, the last
  one for folded executions.
- `commit`: full SHA of the commit the run started from (`git rev-parse HEAD`
  at run start).
- `fileBlob`: Git blob hash of the test file's bytes as tested, as
  `git hash-object <file>` prints it. Compute it once per file per run.

A file holds at most one observation per identity. Keep the snapshot
compact: no task ids, captured output, errors, stack traces, annotations,
browser details or other result details.

## Reading proof

Intent lists a file's test cases from its published TypeScript or
JavaScript source in definition order: every `it` or `test` call, with or
without a modifier chain such as `.each`, `.for`, `.skip`, `.only`, `.todo`,
`.concurrent` or `.skipIf(…)`, whose name is a string literal or a template
literal without substitutions, named by its enclosing `describe` or `suite`
names under the same rule. An `.each` or `.for` name is a template: observed names
that fit its placeholders show one row each, in Unicode code-point order of
the observed name, whatever order the snapshot holds them in; an observed
name that another test of the file names, or that an earlier template
already fits, is not that template's. A template without placeholders names
one case. A test observed under several
projects shows one row per observed project, in code-point order of the
project name, whatever order the snapshot holds them in. Any other test
shows one row, its proof or **No successful proof**. Observations whose
identity the published file no longer defines, such as a renamed test, are
not shown, and neither are tests whose own or enclosing suites' names are
not literals.
Missing or unreadable proof remains a distinct explanation. A matching
`fileBlob` shows only that the test file is unchanged since the case passed,
not that the product, its components or its fixtures are.

## Commit proof

Commit the deliberately refreshed snapshot with the work it observed, or in
one intentional proof checkpoint. A recording run does not commit
automatically. Normal test-only work creates no proof change or proof-only
commit. Only committed proof is published Proof; uncommitted files, including
observations from an explicitly recording CI run, do not count.
