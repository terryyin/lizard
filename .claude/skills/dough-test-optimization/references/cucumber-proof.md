# Cucumber proof

Generate or refresh Cucumber proof only when the caller explicitly enables
recording. Normal test runs leave existing proof byte-for-byte untouched,
whether they pass or fail, and create no proof files, directories or locks.
An explicit recording run uses the same tests, selection, diagnostics and exit
meaning as a normal run; focused runs are allowed, with no whole-suite
requirement. It maintains a compact snapshot of the scenarios it actually ran.

Retain only passing scenarios as committed proof, with their time, attempts,
commit and feature-file version. During an explicit refresh, an executed final
unsuccessful attempt clears that scenario's earlier proof; unexecuted scenarios
keep theirs. A pass records proof even if another scenario fails the run.
Open Dough's Intent reads these snapshots from the project's published
revision. Published proof is the last deliberately retained verification, not
the result of subsequent normal runs or a statement of current product health.

Apply this recording and snapshot contract to this project's existing Cucumber
producer, including Cucumber.js or a Cypress Cucumber preprocessor. Keep the
producer's established scenario and attempt observations. Implement the option
and refresh in this project's own code; Open Dough ships no recorder,
consolidation tool or new runner adapter.

Before generating proof, resolve the covered Intent location, proof directory
and actual recording command or skill from this project's settings. When they
are missing, configure them from the existing runner or name the missing
project input before recording; do not silently enable it in normal commands.

## Configure once

- Choose an explicit project-owned option, for example
  `RECORD_CUCUMBER_PROOF=1`, and a command or skill that enables it. Keep
  recording off by default in every profile, including CI. A recorder must
  check the option before any proof reads, writes, cleanup or lock allocation.
  Merely running tests, optimizing them or using a CI profile never opts in.
- For Cucumber.js, write the recorder as a plugin and conditionally add it to
  the project's configuration (`cucumber.js`, `cucumber.mjs`, `cucumber.json`,
  or equivalent) beside the existing formatters. For an existing Cypress
  Cucumber producer, keep its preprocessor's Cucumber Messages boundary and
  gate the existing proof writer there; do not add a second collection path.
  The recorder must support the focused, tagged, parallel, retried and full
  runs the project's recording command offers.
- For Cucumber.js plugin loading, native `import()` runs before `requireModule` or
  `require` hooks such as `tsx` apply. In a TypeScript project, write the
  recorder as JavaScript, or as a module Node loads by type stripping without
  warnings (for example `.mts` with erasable syntax only), and check that
  loading it prints nothing, so existing output stays unchanged.
- In Cucumber.js, do not add the recorder as another `format` entry without its own output
  target: Cucumber.js writes standard output through only one formatter, the
  last such entry, so the recorder would replace the project's existing
  output. A Cucumber.js without plugin support can use a formatter given its
  own output file outside the proof directory instead.
- In Cucumber.js, record in the recorder, which runs once per run in the main
  process and receives every worker's messages under `--parallel`, rather than
  support-code hooks in each worker. Other existing producers keep their
  current Messages aggregation boundary for one consistent refresh.
- Keep existing output, diagnostics and the command's exit meaning
  unchanged. The recorder prints nothing and never fails the run. When it
  cannot record, for example outside a Git checkout, it records nothing.
- Declare the proof directory and its generation instruction in
  `.planning/open-dough.json` on every Gherkin Intent location it covers:

  ```json
  {
    "intent": {
      "locations": [
        {
          "path": "e2e_test/features",
          "kind": "gherkin",
          "proof": "e2e_test/proof",
          "proofGeneration": {
            "command": "RECORD_CUCUMBER_PROOF=1 npm run test:e2e"
          }
        }
      ]
    }
  }
  ```

  `proof` is a directory path from the repository root. Commit its files; do
  not ignore it. `proofGeneration` contains exactly one nonempty string:
  `command`, run from the repository root, or `skill`, the invocation of a
  skill maintained by this project, for example
  `"proofGeneration": { "skill": "generate-intent-proof" }`. Use this
  project's actual command or skill, including the explicit recording option
  and any required environment or selection. The skill must document how to
  generate or refresh this location's proof without changing normal runs.
  Intent displays the instruction as text; it does not execute it. Missing or
  invalid generation configuration does not hide readable historical proof.

- Record only scenarios of feature files under the Gherkin location or
  locations that declare this proof directory. Scenarios of other features
  run through the same configuration, such as throwaway features this
  project's own tests run, are not recorded.

## Refresh successful proof explicitly

- Keep one maintained snapshot, for example `latest.json`, or stable
  per-feature snapshots. Do not accumulate a new file for every run. When
  adopting existing run files, reduce them using the reading rules below and
  remove consumed files as part of the same deliberate refresh.
- After every finished executed scenario, refresh the affected snapshot
  atomically: write a non-JSON temporary file in the same directory, then
  rename it over the snapshot. Stable scenario-identity ordering limits diff
  noise. Preserve unrelated observations; delete files made empty rather than
  writing an empty replacement. An interrupted recording run keeps completed
  updates without claiming the remaining scenarios were run.
- A scenario has finished when its last attempt has finished, that is, when
  no retry follows. Record a final pass once, then, preserving all attempts
  and their total duration. Intermediate retry failures do not clear proof.
- For an executed scenario whose final result is unsuccessful, remove its
  earlier observations from every proof file in this checkout, including the
  affected snapshot. Match by `feature`, `scenario` and `example`; preserve
  unrelated identities and observations newer than the unsuccessful attempt.
  Rewrite changed files atomically through a non-JSON temporary file and
  delete files made empty. Persist no failed observation or failure marker.
  A later pass records successful proof again.
- Serialize proof mutations by concurrent runs sharing this checkout, so
  cleanup cannot lose another scenario's proof or an in-memory snapshot
  restore a removed success. Separate worktrees retain separate proof
  directories; ordinary Git reconciliation must preserve identity cleanup.
  Reconcile by scenario identity rather than keeping a removed success from
  another worktree.
- Record only attempts that ran. Under `--dry-run`, and for scenarios that
  `--fail-fast` stops after an earlier failure, Cucumber reports every step,
  hooks included, as skipped without running it; record nothing for such an
  attempt, and do not clear its earlier proof. An executed unsuccessful
  attempt, including hooks or executed pending/undefined steps, clears proof.
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
- `outcome`: always `passed`. In an explicit recording run, determine the
  final attempt's result from all steps and hooks; an unsuccessful executed
  result clears earlier proof instead of recording an observation.
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

The latest successful observation of a scenario, by `finishedAt`, is its
last successful proof. Scenarios a run did not execute keep their earlier
proof. When reading older version-1 files containing unsuccessful outcomes,
first select each identity's latest observation across every file, then keep
only passes: a later unsuccessful observation invalidates an older success.
An unsuccessful result also clears a success with the same `finishedAt`,
independently of file order.
Do not filter unsuccessful observations before selecting the latest result.
Normal runs do not participate in this reduction: their later results leave
the published snapshot unchanged. No retained success means **No successful
proof**. Missing or unreadable proof remains a distinct explanation. A matching
`featureBlob` shows only
that the scenario's Gherkin is unchanged since it passed, not that the product
or its steps are. Last successful proof does not assert current product health.

## Consolidating existing proof

During adoption or an explicit refresh, consolidate existing run files: read
every proof file, keep the latest observation of each identity before retaining
only passes, write
those successes as a maintained snapshot in the same format, then delete
the files consumed, and commit the change together. If no success remains,
delete the consumed files without creating an empty replacement. Use this
same latest-first cleanup when adopting the contract over older proof files.
Readers see the same successful proof before and after;
nothing distinguishes a consolidated file from a run file. A setup that
already keeps a single latest snapshot need not consolidate.

## Commit proof

Commit deliberately generated snapshots and cleanup deletions with the work
they observed, or in one intentional proof checkpoint. A recording run does
not commit automatically. Normal test-only work creates no proof change or
proof-only commit. Only committed proof is published Proof; uncommitted files,
including observations from an explicitly recording CI run, do not count.
