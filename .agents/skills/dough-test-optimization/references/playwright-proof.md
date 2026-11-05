# Playwright proof

Generate or refresh Playwright proof only when the caller explicitly enables
recording. Normal test runs leave existing proof byte-for-byte untouched,
whether they pass or fail, and create no proof files, directories or locks.
An explicit recording run uses the same tests, selection, projects, workers,
retries, sharding, reporters, diagnostics and exit meaning as a normal run;
focused runs are allowed, with no whole-suite requirement. Prefer
repository-relative paths that uniquely identify each file when selecting a
focused recording: Playwright treats a bare basename as a substring filter
(observed in Playwright 1.63.0), so `intent-proof-generation.spec.ts` also
selects `boundary/authenticated-read-intent-proof-generation.spec.ts`. It
maintains one compact snapshot of the cases it actually ran.

Retain only plain passing cases as committed proof, with their time,
attempts, commit and test-file version. During an explicit refresh, an
executed final unsuccessful result clears that case's earlier proof; cases
the run did not execute keep theirs. A pass records even if another case
fails the run. Open Dough's Intent reads the snapshot from the project's
published revision. Published proof is the last deliberately retained
verification, not subsequent normal runs or a statement of current health.

Implement the option and the recorder as a Playwright reporter in this
project's own code; Open Dough ships no recorder. When this project already
maintains another proof snapshot, such as Vitest or Cucumber proof, the code
that maintains it may maintain this snapshot too. A suite that already loads
a quiet or custom reporter may extend that reporter rather than adding a
second one for the same events.

Before generating proof, resolve the covered `typescript` or `javascript`
Intent location, its proof directory and the actual recording command or
skill from this project's settings. When they are missing, configure them
from the existing Playwright invocation or name the missing input before
recording; do not silently enable recording in normal commands.

## Configure once

- Choose an explicit project-owned option, for example the environment
  variable `RECORD_PLAYWRIGHT_PROOF=1`, and a command or skill that sets it.
  Keep recording off by default in every profile, including CI: never set the
  option in Playwright configuration, package scripts the project's ordinary
  commands use, CI workflows or the project's default test wrapper. The
  recorder checks the option before any proof read, write, cleanup or lock.
  Merely running tests, optimizing them or using a CI profile never opts in.
- Add the recorder to the configured `reporter` list in this project's
  Playwright configuration, beside the reporters already there, so ordinary
  commands, the test wrapper and CI load it with recording off. Where
  reporters differ by profile, add it to each list. The recording command
  then differs from a normal run only by the option.
- Run the recording command with the configured reporters: give it no
  command-line `--reporter` option. A command-line `--reporter` replaces the
  configured `reporter` list rather than adding to them (observed in
  Playwright 1.63.0), so a command that passes one loads no recorder and
  records nothing, even with the option set.
- Keep existing output, warnings, diagnostics, collection, test order,
  projects, workers, retries, sharding and the command's exit meaning
  unchanged with the recorder loaded, recording on or off. The recorder
  prints nothing and never fails the run: contain its own errors so none
  surfaces as a Playwright error, an unhandled rejection or a changed exit
  status. When it cannot record, for example outside a Git checkout, it
  records nothing.
- Declare the runner, the proof directory and its generation instruction in
  `.planning/open-dough.json` on every Intent location it covers:

  ```json
  {
    "intent": {
      "locations": [{
        "path": "dashboard/tests",
        "kind": "typescript",
        "include": ["**/*.spec.ts"],
        "runner": "playwright",
        "proof": "dashboard/tests/proof",
        "proofGeneration": {
          "command": "RECORD_PLAYWRIGHT_PROOF=1 npm run --silent test:dashboard -- <files>"
        }
      }]
    }
  }
  ```

  A `typescript` or `javascript` location that declares `proof` also
  declares `runner`; `"playwright"` selects this reference's format. Without
  a `runner` Intent reads, it explains that setting and reads no proof.
  `proof` is a repository-root directory path; commit its files and do not
  ignore it. `proofGeneration` contains exactly one nonempty string:
  `command` from the repository root, or `skill` for a project-maintained
  skill. Use this project's actual command or skill, including the
  recording option and any wrapper, environment or selection, as one run.
  Intent displays the instruction as text; it does not execute it. Missing
  or invalid generation configuration does not hide readable historical
  proof.

- Resolve the repository root with `git rev-parse --show-toplevel` and read
  the covered locations, those with `runner: "playwright"` and `proof`, from
  `.planning/open-dough.json` there. Record only test cases of files under
  those locations, each into that location's proof directory. Cases of other
  files in the same session, such as fixture projects in temporary
  directories, are not recorded.
- Test the recorder by running Playwright on fixture projects, each in its
  own temporary Git checkout with its own `.planning/open-dough.json`,
  setting the option only for the inner runs. When nested runs of this
  project's real configuration start from an outer recording run, strip
  `RECORD_PLAYWRIGHT_PROOF` from those nested environments so they do not
  write into this checkout's proof directory.

## Record from the reporter's results

- The recorder runs once per run in the Playwright runner process and
  receives every project's and worker's results. Record from the reporter's
  per-attempt events (`onTestEnd` with its `result` and `test`), never from
  fixtures, hooks, setup files or code running in a worker or browser.
  Observed in Playwright 1.63.0: each `onTestEnd` carries `result.status`
  (`passed`, `failed`, `timedOut`, `skipped`, and when the run stops early
  `interrupted`), `result.retry`, `result.duration` for that attempt alone,
  `result.stdout` and `result.stderr`, `test.retries`, `test.expectedStatus`,
  and for repetitions `test.repeatEachIndex`.
- A test case is finished when its final attempt arrives. Treat an attempt
  as final when its status is `passed`, or when its retry index equals the
  test's configured retries. Intermediate failed attempts never clear proof
  and never record; only the final attempt decides. Do not wait for a
  separate end-of-run summary of cases.
- Proof is a final attempt with status `passed` and expected status
  `passed` that wrote nothing to stdout or stderr: the suite's strict plain
  pass. Record that case. A final `failed`, `timedOut`, `test.fail`
  (status `failed` with expected status `failed`) or printed-output result
  (status `passed` with captured stdout or stderr) ran to a verdict: clear
  the case's proof and record nothing. Any other result ran to no verdict
  and keeps the case's proof: a `skipped` result from `test.skip`,
  `test.fixme` or a `skip()` call in the body (expected status `skipped`);
  an `interrupted` result; a test outside the run's selection, project,
  shard or split; and a test an interrupted run never reached.
- `attempts` is the final attempt's retry index plus one; `1` without
  retries. `durationMs` is the sum of every attempt's `result.duration` for
  the case, in milliseconds. Playwright reports each attempt's duration alone
  (observed in Playwright 1.63.0), so add the attempts; do not take only the
  final one.
- An identity executed more than once in one run (for example
  `--repeat-each`, or another execution that shares the same `file`, `test`
  and `project`) records one observation that passes only when every
  execution passed, with `attempts` the greatest of them and `durationMs`
  their sum. Keep the run's executions of each identity in memory: the
  first pass replaces any earlier proof, a later pass folds into it, and
  once one execution failed the identity stays cleared for the rest of the
  run.

## Identity

Identify a case by Playwright's own names, never by test ids, definition
lines or collection order:

- `file`: the file suite's path relative to the repository root, not
  Playwright's test directory, the working directory or
  `test.location.file`. A helper-defined test still belongs to the importing
  spec's file suite; `test.location.file` names the helper and must not be
  used. Use `/` separators. Record only files under covered locations.
  Resolve the repository root and the file suite path through the same
  realpath (or equivalent) before taking the relative path: on some hosts
  (observed on macOS) `git rev-parse --show-toplevel` and Playwright's
  suite path disagree on an intermediate symlink such as `/var` versus
  `/private/var`, and an uncanonical relative path then falls outside every
  covered location and records nothing.
- `test`: the title path after the project and file entries, as Playwright
  gives it (`titlePath()` without the leading empty string, project name and
  file): a list from the outermost named describe to the test title, for
  example `["touch reading", "keeps a long story readable"]`. An anonymous
  `test.describe(fn)` adds no title and therefore no segment. Take each
  title from Playwright's path; do not split a joined full name, since a
  title may contain its separator.
- `project`: the Playwright project name, only when more than one configured
  project selects the file. Omit it when projects select disjoint files.
  Under `--project`, decide from `config.projects`, not only the root
  suite's selected projects: the config still lists every configured
  project while the root suite may hold only the selected one (observed in
  Playwright 1.63.0).

## Refresh the snapshot

- Keep one maintained snapshot per proof directory, `latest.json`. Do not
  accumulate a file per run. Intent reads a proof file of up to 32 MiB.
- After every finished executed case, refresh atomically: write a non-JSON
  temporary file in the same directory, then rename it over `latest.json`.
  Re-read before each refresh and preserve unrelated observations; leave
  non-`.json` files unchanged. A store shared with another runner may fold
  other `.json` files in the directory into `latest.json` and remove them.
  A pass replaces or folds into that identity; an executed unsuccessful
  result removes it with no failed observation. Order by `file`, then
  `test`, then `project` (absent first); compare `test` lists name by name,
  shorter prefix first. Delete an empty snapshot rather than writing empty
  JSON. An interrupted run keeps completed updates. A shard records only
  cases it ran. A full-suite refresh rewrites once per case.
- Serialize mutations across concurrent recording runs in this checkout.
  Keep any lock without the `.json` extension and remove it when the run
  ends. Separate worktrees keep separate snapshots; reconcile by identity,
  never keeping a removed success from another worktree.
- Every `.json` file in the proof directory or below is read as proof; keep
  nothing else there with that extension. One invalid Playwright proof file
  leaves the directory's proof unreadable until fixed or removed.

## File format

The snapshot is the Vitest proof file format with `project` available:

```json
{
  "version": 1,
  "observations": [{
    "file": "dashboard/tests/accessible-board-touch.spec.ts",
    "test": ["touch reading", "keeps a long story readable"],
    "outcome": "passed",
    "attempts": 1,
    "durationMs": 42,
    "finishedAt": "2026-11-04T09:00:00.000Z",
    "commit": "a1b2c3d4a1b2c3d4a1b2c3d4a1b2c3d4a1b2c3d4",
    "fileBlob": "3b18e512dba79e4c8300dd08aeb37f8e728b8dad"
  }]
}
```

When more than one configured project selects the file, include `project`
(for example `"project": "chromium"`). Each observation has `file`, `test`
and optional `project` as under Identity, plus `outcome` (always `passed`),
`attempts` (final retry index plus one, greatest over folds), `durationMs`
(summed attempt durations, summed again over folds), `finishedAt` (ISO 8601
UTC of the final attempt), `commit` (`git rev-parse HEAD` at run start) and
`fileBlob` (`git hash-object` of the file suite path, once per file per
run). At most one observation per identity. Keep the snapshot compact: no
test ids, captured output, errors, stacks, annotations or browser details.

## Reading proof

Intent lists a file's test cases from its published TypeScript or
JavaScript source in definition order: every family call (`describe`,
`suite`, `it`, `test`) bare or as a member of `test` (`test.describe`,
`test.it`), with or without a modifier chain that admits Vitest-style
modifiers and Playwright's `.serial`, `.parallel`, `.fixme` and `.fail`
(for example `test.describe.serial`, `test.fixme`, `test.fail`), whose
name is a string literal or a template literal without substitutions,
named by its enclosing suite names under the same rule. The case line is
the line of the `test` (or `it`) token. An anonymous `test.describe(fn)`
is read through and adds no segment. `test.describe.configure`, `test.use`,
hooks and `test.step` name no case. Several observed projects show one row
each, in code-point order of the project name; any other test shows one
row, its proof or **No successful proof**. Observations whose identity the
published file no longer defines are not shown, nor are tests whose own or
enclosing suites' names are not literals. Loop-generated tests (template
literals with substitutions inside loops) and helper-defined tests (calls
in an imported module) have no rows; their passes may remain in the
snapshot without appearing beside source. Summaries count listed rows
only. Missing or unreadable proof remains a distinct explanation. A
matching `fileBlob` shows only that the test file is unchanged since the
case passed, not product, component or fixture health.

## Commit proof

Commit the deliberately refreshed snapshot with the work it observed, or in
one intentional proof checkpoint. A recording run does not commit
automatically. Normal test-only work creates no proof change or proof-only
commit. Only committed proof is published Proof; uncommitted files,
including from an explicitly recording CI run, do not count.
