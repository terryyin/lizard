# Node test proof

Generate or refresh `node:test` proof only when the caller explicitly enables
recording. Normal test runs leave existing proof byte-for-byte untouched,
whether they pass or fail, and create no proof files, directories or locks.
An explicit recording run uses the same tests, selection, reporters,
diagnostics and exit meaning as a normal run; focused runs are allowed, with
no whole-suite requirement. It maintains one compact snapshot of the test
cases it actually ran.

Retain only passing test cases as committed proof, with their time, attempts,
commit and test-file version. During an explicit refresh, an executed final
unsuccessful result clears that case's earlier proof; cases the run did not
execute keep theirs. A pass records proof even if another case fails the run.
Open Dough's Intent reads the snapshot from the project's published revision.
Published proof is the last deliberately retained verification, not subsequent
normal runs or a statement of current product health.

Implement the option and the recorder as a `node --test` reporter in this
project's own code; Open Dough ships no recorder. When this project already
maintains another proof snapshot, such as Cucumber or Vitest proof, the code
that maintains it may maintain this snapshot too.

Before generating proof, resolve the covered `typescript` or `javascript`
Intent location, its proof directory and the actual recording command or
skill from this project's settings. When they are missing, configure them
from the existing `node --test` invocation or name the missing project input
before recording; do not silently enable recording in normal commands.

## Configure once

- Choose an explicit project-owned option, for example the environment
  variable `RECORD_NODE_TEST_PROOF=1`, and a command or skill that sets it.
  Keep recording off by default in every profile, including CI: never set the
  option in package scripts the project's ordinary commands use, CI workflows
  or the project's default test wrapper. The recorder checks the option
  before any proof read, write, cleanup or lock. Merely running tests,
  optimizing them or using a CI profile never opts in. A loaded recorder with
  the option unset records nothing and leaves proof untouched.
- Load the recorder as a `--test-reporter` module. Node requires a
  destination for every reporter (`ERR_INVALID_ARG_VALUE` without one);
  destinations may share `stdout`. Two loading shapes are valid:
  - Add the recorder beside the reporter the ordinary command already uses,
    each with its own `--test-reporter-destination`, so ordinary commands,
    the project's test wrapper and CI all load it with recording off. The
    recording command then differs from a normal run only by the option.
  - Leave ordinary commands without the recorder and load it only from the
    recording command, on the command line or through `NODE_OPTIONS`
    (for example `NODE_OPTIONS='--test-reporter=… --test-reporter-destination=stdout'`).
    `NODE_OPTIONS` reaches child processes that invoke `node --test`; a plain
    `node -e` child is unaffected by a `--test-reporter` value. Choose the
    shape that matches how this project runs `node --test`.
- Keep existing output, warnings, diagnostics, test order, scheduling and the
  command's exit meaning unchanged with the recorder loaded, recording on or
  off. The recorder prints nothing and never fails the run: contain its own
  errors so none surfaces as a Node error, an unhandled rejection or a
  changed exit status. When it cannot record, for example outside a Git
  checkout, it records nothing.
- Declare the runner, the proof directory and its generation instruction in
  `.planning/open-dough.json` on every Intent location it covers:

  ```json
  {
    "intent": {
      "locations": [{
        "path": "src/skills",
        "kind": "javascript",
        "include": ["**/*.test.mjs"],
        "runner": "node:test",
        "proof": "tests/proof",
        "proofGeneration": {
          "command": "RECORD_NODE_TEST_PROOF=1 bash scripts/test.sh <files>"
        }
      }]
    }
  }
  ```

  A `typescript` or `javascript` location that declares `proof` also
  declares `runner`; `"node:test"` selects this reference's format. Without
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
  the covered locations, those with `runner: "node:test"` and `proof`, from
  `.planning/open-dough.json` there. Record only test cases of files under
  those locations, each into that location's proof directory. Cases of other
  files in the same session, such as fixture projects in temporary
  directories, are not recorded.
- Test the recorder by running `node --test` on substitute files, each in its
  own temporary Git checkout with its own `.planning/open-dough.json`,
  setting the option only for the inner runs, so the outer test run never
  records. When a test compares a run's output with and without the
  recorder, mask what Node prints from timing so machine load cannot flip
  the comparison.

## Record from the reporter's events

- The recorder is a `--test-reporter` module that runs once per `node --test`
  process, in the main process, and receives every file's events. Record from
  `test:pass` and `test:fail` events only. Never record from test files,
  setup files or hooks.
- A case is a finished result whose `details.type` is `"test"` and whose
  enclosing results are suites (`details.type: "suite"`) or the file. Build
  the suite path from those enclosing names as Node's own reporters and this
  project's failures reporter name a test. A `describe` or `suite` result is
  not a case.
- A test's subtests fold into it: the case's result is Node's result for the
  parent test, and `durationMs` is that test's `details.duration_ms`, which
  already includes its subtests. A subtest records nothing of its own. A
  parent whose failure type is `subtestsFailed` is an executed unsuccessful
  result: it clears the case's proof and records nothing. Node has no retry,
  so `attempts` is always `1`.
- A final plain pass (`test:pass` without `skip` or `todo`) records proof. A
  `test:fail` that is not a `todo` failure is an executed unsuccessful
  result: it clears the case's proof and records nothing. Timeouts
  (`failureType: "testTimeoutFailure"`) clear. Any other finished result ran
  to no verdict and keeps the case's proof: a result with `skip` or `todo`
  set (including a failing `todo`, which Node reports as `test:fail` with
  `todo: true` and exit 0), a test a name pattern or `--test-only` excluded
  (Node emits nothing for it), a test cancelled by its parent, and a test an
  interrupted run never reached. Cases of files that failed to load also keep
  their proof.
- `durationMs` is the case's `details.duration_ms` in milliseconds and often
  fractional. Record it as given.
- An identity executed more than once in one process records one observation
  that passes only when every execution passed, with `durationMs` their sum.
  Keep the process's executions of each identity in memory: the first pass
  replaces any earlier proof, a later pass folds into it, and once one
  execution failed the identity stays cleared for the rest of the process.

The event shape above was observed on Node 24.21.0 and confirmed on Doughnut's
Node 26.13.0: the same fields (`details.type`, `duration_ms`, `skip`, `todo`,
`subtestsFailed`, timeout failure type, and exclusion silence) hold; no rule
change was required.

## Identity

Identify a case by Node's own names, never by line, column or collection
order:

- `file`: the test file's path relative to the repository root
  (`git rev-parse --show-toplevel`), with `/` separators, not the working
  directory alone.
- `test`: a list of names from the outermost `describe` or `suite` to the
  test, each as Node reported it: `["guidance", "rejects a missing path"]`.
  Take each name from the case and its enclosing suites; do not split a
  joined full name, since a name may contain its separator.

Do not include a `project` field. This snapshot is the Vitest proof format
without `project`.

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
  `test`; compare `test` lists name by name, shorter prefix first. Delete
  an empty snapshot rather than writing empty JSON. An interrupted run
  keeps completed updates. A full-suite refresh rewrites once per case.
- Serialize mutations across concurrent recording processes sharing this
  checkout. A wrapper such as `scripts/test.sh` that runs several
  `node --test` jobs at once makes this rule load-bearing. Keep any lock
  without the `.json` extension and remove it when the process ends.
  Separate worktrees keep separate snapshots; reconcile by identity, never
  keeping a removed success from another worktree.
- Every `.json` file in the proof directory or below is read as proof; keep
  nothing else there with that extension. One invalid proof file leaves the
  directory's proof unreadable until fixed or removed.

## File format

The snapshot is one JSON object in the Vitest proof format without
`project`:

```json
{
  "version": 1,
  "observations": [{
    "file": "src/skills/dough-story-refinement/scripts/dough-land-rerun.test.mjs",
    "test": ["rerun", "replays the last command"],
    "outcome": "passed",
    "attempts": 1,
    "durationMs": 12.4,
    "finishedAt": "2026-11-04T09:00:00.000Z",
    "commit": "a1b2c3d4a1b2c3d4a1b2c3d4a1b2c3d4a1b2c3d4",
    "fileBlob": "3b18e512dba79e4c8300dd08aeb37f8e728b8dad"
  }]
}
```

Each observation has `file` and `test` as under Identity, plus `outcome`
(always `passed`), `attempts` (always `1`), `durationMs` (milliseconds,
possibly fractional, summed over folds), `finishedAt` (ISO 8601 UTC of the
final result), `commit` (`git rev-parse HEAD` at run start) and `fileBlob`
(`git hash-object <file>`, once per file per process). At most one
observation per identity. Keep the snapshot compact: no line or column
numbers, captured output, errors, stacks or other result details. Omit
`project`.

## Reading proof

Intent lists a file's test cases from its published JavaScript or TypeScript
source in definition order: every `test` or `it` call outside another test's
callback, with or without a modifier such as `.skip`, `.only` or `.todo`,
whose name is a string literal or a template literal without substitutions,
named by its enclosing `describe` or `suite` names under the same rule. A
default import (`import test from "node:test"`) is read; an alias is not.
Member calls such as `t.test(` are not listed: a test's subtests have no
rows of their own. Loop-generated names (template literals with
substitutions) have no rows. Prefer literal names so Intent can show them.
Each listed test shows one row with its proof or **No successful proof**.
Observations whose identity the published file no longer defines, such as a
renamed test, are not shown, and neither are tests whose own or enclosing
suites' names are not literals. Missing or unreadable proof remains a
distinct explanation. A matching `fileBlob` shows only that the test file is
unchanged since the case passed, not that the product, its components or its
fixtures are.

## Commit proof

Commit the deliberately refreshed snapshot with the work it observed, or in
one intentional proof checkpoint. A recording run does not commit
automatically. Normal test-only work creates no proof change or proof-only
commit. Only committed proof is published Proof; uncommitted files, including
observations from an explicitly recording CI run, do not count.
