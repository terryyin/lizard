# Cucumber proof

Record the latest observed result of every Cucumber scenario as committed
proof, so a reader of this project's feature files sees, beside each
scenario, when it was last observed, its outcome, and the commit and file
version it was observed on. Open Dough's Intent reads these files from the
project's published revision. Every ordinary run records the scenarios it
ran; nobody runs the whole suite only to collect proof.

These rules are for this project's Cucumber.js setup. Implement them in this
project's own code; Open Dough ships no recorder or consolidation tool.

## Configure once

- Write a proof recorder in this project as a Cucumber.js plugin and add it
  to the Cucumber configuration (`cucumber.js`, `cucumber.mjs`,
  `cucumber.json`, or the equivalent) beside the formatters already there,
  for example `format: ['progress'], plugin: ['./<path>/proof-plugin.js']`.
  Add it to every profile that runs scenarios, so focused, tagged, parallel,
  retried and full runs all record proof.
- Do not add the recorder as another `format` entry without its own output
  target: Cucumber.js writes standard output through only one formatter, the
  last such entry, so the recorder would replace the project's existing
  output. A Cucumber.js without plugin support can use a formatter given its
  own output file outside the proof directory instead.
- Record in the recorder, which runs once per run in Cucumber's main process
  and receives every worker's messages under `--parallel`; not in support-code
  hooks, which run in each worker.
- Keep existing output, diagnostics and the command's exit meaning
  unchanged. The recorder prints nothing and never fails the run. When it
  cannot record, for example outside a Git checkout, it records nothing.
- Declare the proof directory in `.planning/open-dough.json` on the Gherkin
  Intent location it covers:

  ```json
  {
    "intent": {
      "locations": [
        {
          "path": "e2e_test/features",
          "kind": "gherkin",
          "proof": "e2e_test/proof"
        }
      ]
    }
  }
  ```

  `proof` is a directory path from the repository root. Commit its files; do
  not ignore it.

## One file per run

- Each run writes its own file in the proof directory, named uniquely, for
  example `<UTC start time>-<random suffix>.json`
  (`20261101T095900Z-3f9a1c2e.json`). Never reuse a name, and never write to
  another run's file. Concurrent worktrees and repeated runs then merge
  without conflict.
- After every finished scenario, rewrite the whole file atomically: write it
  under a temporary name in the same directory that does not end in `.json`,
  then rename it over the run's file. An interrupted run keeps what finished.
  A run that finishes no scenario writes no file.
- A scenario has finished when its last attempt has finished, that is, when
  no retry follows. Record it once, then.
- Record only attempts that ran. Under `--dry-run`, and for scenarios that
  `--fail-fast` stops after an earlier failure, Cucumber reports every step,
  hooks included, as skipped without running it; record nothing for such an
  attempt.
- Every `.json` file in the proof directory, or in any directory below it, is
  read as proof. Keep nothing else there with that extension. One file that is
  not valid proof leaves the whole directory's proof unreadable until it is
  fixed or removed.

## File format

A proof file is one JSON object:

```json
{
  "version": 1,
  "observations": [
    {
      "feature": "e2e_test/features/jobs.feature",
      "scenario": "Retry a job after <failure>",
      "example": ["timeout", "3"],
      "outcome": "passed",
      "attempts": 2,
      "durationMs": 4100,
      "finishedAt": "2026-11-01T10:01:00.000Z",
      "commit": "e5f6a7b8e5f6a7b8e5f6a7b8e5f6a7b8e5f6a7b8",
      "featureBlob": "3b18e512dba79e4c8300dd08aeb37f8e728b8dad"
    }
  ]
}
```

Each observation has these fields:

- `feature`: feature file path from the repository root, with `/`
  separators. Cucumber names it relative to its working directory; convert
  when that is not the repository root.
- `scenario`: scenario name as written in the feature file. For an outline,
  its name before placeholders are filled in (the Gherkin source's name, not
  the runner's pickle name).
- `example`: outline rows only. The example row's cell values in column
  order, as Gherkin parses them: trimmed, with escaped pipes, backslashes and
  newlines unescaped. Omit it for a plain scenario.
- `outcome`: status of the last attempt, lowercase: `passed`, `failed`,
  `skipped`, `pending`, `undefined`, `ambiguous` or `unknown`. An attempt's
  status is its worst step status, hooks included, in this order from worst:
  `failed`, `ambiguous`, `undefined`, `pending`, `skipped`, `passed`,
  `unknown`.
- `attempts`: attempts made, retries included; `1` when the scenario was not
  retried.
- `durationMs`: milliseconds spent across all attempts.
- `finishedAt`: ISO 8601 UTC time the last attempt finished.
- `commit`: full SHA of the commit the run started from (`git rev-parse HEAD`
  at run start).
- `featureBlob`: Git blob hash of the feature file's bytes as tested, as
  `git hash-object <file>` prints it.

Identify a scenario only by `feature`, `scenario` and `example`. Never record
runner-generated IDs: they change on every run. A file holds at most one
observation per identity; a run that executes one twice keeps the later one.

Keep files compact: no screenshots, traces, captured output, error messages,
step details or other attachments.

## Reading proof

The latest observation of a scenario, by `finishedAt`, wins across all files,
whatever its outcome. Scenarios a run did not execute keep their earlier
observations from other files. A matching `featureBlob` shows only that the
scenario's Gherkin is unchanged since it was observed, not that the product or
its steps are.

## Consolidating, optionally

When the directory holds many files, consolidate: read every proof file, keep
the latest observation of each identity, write them as a new uniquely named
file in the same format, then delete the files consumed, and commit the
change together. Readers see the same latest observations before and after;
nothing distinguishes a consolidated file from a run file. A setup that
already keeps a single latest snapshot need not consolidate.

## Commit proof

Commit proof files with the ordinary work they observed. Only committed proof
is proof; files written by runs that are never committed, such as in CI, do
not count.
