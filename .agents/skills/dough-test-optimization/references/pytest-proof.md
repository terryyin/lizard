# pytest proof

Generate or refresh pytest proof only when the caller explicitly enables
recording. Normal test runs leave existing proof byte-for-byte untouched,
whether they pass or fail, and create no proof files, directories or locks.
An explicit recording run uses the same tests, selection, plugins,
diagnostics and exit meaning as a normal run; focused runs are allowed, with
no whole-suite requirement. It maintains one compact snapshot of the test
cases it actually ran.

Retain only passing test cases as committed proof, with their time, attempts,
commit and test-file version. During an explicit refresh, an executed final
unsuccessful result clears that case's earlier proof; cases the run did not
execute keep theirs. A pass records proof even if another case fails the run.
Open Dough's Intent reads the snapshot from the project's published revision.
Published proof is the last deliberately retained verification, not the
result of subsequent normal runs or a statement of current product health.

Implement the option and the recorder as a pytest plugin in this project's
own code; Open Dough ships no recorder.

Before generating proof, resolve the covered `python` Intent location, its
proof directory and the actual recording command or skill from this
project's settings. When they are missing, configure them from the existing
pytest invocation or name the missing project input before recording; do
not silently enable recording in normal commands.

## Configure once

- Choose an explicit project-owned option, for example the environment
  variable `RECORD_PYTEST_PROOF=1`, and a command or skill that sets it. Keep
  recording off by default in every profile, including CI: never set the
  option in pytest configuration (`addopts`, `pytest.ini`, `pyproject.toml`),
  CI workflows or the project's default test wrapper. The recorder checks the
  option before any proof read, write, cleanup or lock. Merely running tests,
  optimizing them or using a CI profile never opts in.
- Register the plugin so the project's ordinary pytest commands, its test
  wrapper and CI all load it with recording off, for example through a `-p`
  entry the project already passes or `pytest_plugins` in the root
  `conftest.py`. The recording command then differs from a normal run only by
  the option.
- Keep existing output, warnings, diagnostics, collection, test order,
  parallel distribution and the command's exit meaning unchanged with the
  plugin loaded, recording on or off. Loading it emits no warning, which
  matters where `filterwarnings = error` is configured. The recorder prints
  nothing and never fails the run: contain its own errors so none surfaces as
  a pytest internal error or changes the exit status. When it cannot record,
  for example outside a Git checkout, it records nothing.
- Declare the proof directory and its generation instruction in
  `.planning/open-dough.json` on every `python` Intent location it covers:

  ```json
  {
    "intent": {
      "locations": [
        {
          "path": "tests",
          "kind": "python",
          "include": ["**/test_*.py"],
          "proof": "tests/proof",
          "proofGeneration": {
            "command": "RECORD_PYTEST_PROOF=1 pytest"
          }
        }
      ]
    }
  }
  ```

  `proof` is a directory path from the repository root. Commit its files; do
  not ignore it. `proofGeneration` contains exactly one nonempty string:
  `command`, run from the repository root, or `skill`, the invocation of a
  skill maintained by this project. Use this project's actual command or
  skill, including the explicit recording option and any wrapper,
  environment or selection it needs. Intent displays the instruction as
  text; it does not execute it. Missing or invalid generation configuration
  does not hide readable historical proof.

- Record only test cases of files under the `python` location or locations
  that declare this proof directory. Cases of other files run in the same
  session, such as `pytester` runs in temporary directories, are not
  recorded. Resolve each case's file against the repository root
  (`git rev-parse --show-toplevel`), not pytest's `rootdir` or the working
  directory.
- Test the recorder through pytest's `pytester`, setting the option only for
  the inner runs, so the outer test session never records. Clear
  `PYTEST_XDIST_WORKER`, `PYTEST_XDIST_WORKER_COUNT` and
  `PYTEST_XDIST_TESTRUNUID` for inner runs, so an inner session started from
  an xdist worker is not taken for a worker.

## Record from pytest's reports

- Record from `pytest_runtest_logreport` in the controlling process only.
  Under pytest-xdist, worker processes (`hasattr(config, "workerinput")`)
  read, write and lock nothing; the controller receives every worker's
  reports through the same hook. Without xdist, the single process is the
  controller.
- A test case is one collected item, identified by its node id. Gather its
  reports until one for a phase other than `setup` or `call` arrives, which
  finishes the case. That is normally its `teardown` report; pytest-xdist's
  failed report for an item whose worker crashed also finishes the case and
  clears its proof.
- `durationMs` sums the `duration` of the case's own setup, call and
  teardown reports, as pytest reports them, across all its attempts, in
  milliseconds.
- The case passed when every phase of its final attempt passed and the call
  is a plain pass. Each of these is an executed unsuccessful result that
  clears the case's proof and records nothing:
  - a failure or error in setup, call or teardown, including a teardown
    failure after a passing call;
  - an expected failure (`xfail`: the call report is skipped with
    `wasxfail` set);
  - an unexpected pass (`xpass`: a call report that passed with `wasxfail`
    set; under strict xfail pytest reports it as a failure);
  - any other report carrying `wasxfail`, including a skipped setup report
    of an xfail-marked case whose setup failed or of `xfail(run=False)`.
- A skipped case (skip or skipif marks, or `pytest.skip()`, without
  `wasxfail`) ran nothing: record nothing and keep its proof. Deselected
  cases (for example by `-m`, `-k` or `--deselect`), uncollected cases, a
  `--collect-only` run, and cases an interrupted or `--maxfail` run never
  reached produce no reports and keep their proof.
- `attempts` counts executed attempts when this project's runner reports
  reruns (for example `rerun` reports from a rerun plugin), otherwise `1`.
  Intermediate rerun failures do not clear proof; the final attempt decides.

## Identity

Identify a case by the parts of pytest's own node id, for example
`tests/test_cors.py::TestCredentials::test_sends_credentials[up]`:

- `file`: the path before the first `::`, as a repository path with `/`
  separators.
- `test`: the `::`-joined names after the file, class names included and the
  bracketed parameter id excluded: `TestCredentials::test_sends_credentials`.
- `parameterId`: for a parametrized case only, the text inside the node id's
  outer brackets, `up`. The first `[` after the file starts it and the final
  `]` ends it; the id itself may contain `::`, `-` or brackets. Omit the field
  for a case without one.

Never record collection order, worker ids, item indexes or other runner
object ids. Record parameter ids as pytest gives them, including ids pytest
derives from positions such as `param0` or `direction1`; these change when
parameters are added or reordered, so prefer explicit `ids=` or
`pytest.param(…, id=…)` where proof should keep its identity.

## Refresh the snapshot

- Keep one maintained snapshot per proof directory, `latest.json`. Do not
  accumulate a file per run. Intent reads a proof file of up to 32 MiB, so
  one snapshot of the whole suite stays readable.
- After every finished executed case, refresh the snapshot atomically: write
  a non-JSON temporary file in the same directory, then rename it over
  `latest.json`. Re-read `latest.json` before each refresh and preserve
  unrelated observations; leave any other file in the directory unchanged.
  A pass replaces that identity's observation; an executed unsuccessful
  result removes it and persists no failed observation or failure marker.
  Order observations by `file`, `test` and `parameterId` to limit diff
  noise. Delete the snapshot when it becomes empty rather than writing an
  empty replacement. An interrupted recording run keeps completed updates
  without claiming the remaining cases ran. A full-suite refresh rewrites
  the snapshot once per case; expect that cost on the controlling process.
- Serialize proof mutations by concurrent recording runs sharing this
  checkout, so one run cannot lose another's update or restore a removed
  success. A lock on the proof directory itself (for example `flock` on its
  descriptor) leaves no file behind; keep any lock file without the `.json`
  extension. Separate worktrees keep separate snapshots; reconcile them by
  identity, never keeping a removed success from another worktree.
- Every `.json` file in the proof directory, or in any directory below it, is
  read as proof. Keep nothing else there with that extension. One file that is
  not valid pytest proof leaves the whole directory's proof unreadable until it
  is fixed or removed.

## File format

The snapshot is one JSON object:

```json
{
  "version": 1,
  "observations": [
    {
      "file": "tests/test_cors.py",
      "test": "test_rounding",
      "parameterId": "up",
      "outcome": "passed",
      "attempts": 1,
      "durationMs": 2400,
      "finishedAt": "2026-11-04T09:00:00.000Z",
      "commit": "a1b2c3d4a1b2c3d4a1b2c3d4a1b2c3d4a1b2c3d4",
      "fileBlob": "3b18e512dba79e4c8300dd08aeb37f8e728b8dad"
    }
  ]
}
```

Each observation has `file`, `test` and, when parametrized, `parameterId` as
above, and:

- `outcome`: always `passed`.
- `attempts`: executed attempts, reruns included; `1` without reruns.
- `durationMs`: milliseconds across the case's phases and attempts.
- `finishedAt`: ISO 8601 UTC time the case's teardown report arrived.
- `commit`: full SHA of the commit the run started from (`git rev-parse HEAD`
  at session start).
- `fileBlob`: Git blob hash of the test file's bytes as tested, as
  `git hash-object <file>` prints it. Compute it once per file per run, when
  the controller receives the file's first passing case; under pytest-xdist
  the controller does not collect.

A file holds at most one observation per identity; a run that executes a case
twice keeps the later result. Keep the snapshot compact: no captured output,
tracebacks, longrepr, warnings, keywords or other report details.

## Reading proof

Intent lists a file's test cases from its published Python source in
definition order: module-level `def test_*` and `async def test_*`
functions, and `test_*` methods of `Test*` classes. A function whose
parameter ids were observed shows one row per observed id, in parameter id
order whatever order the snapshot holds them in; any other function shows
one row, its proof or **No successful proof**. Observations
whose identity the published file no longer defines, such as a renamed test,
are not shown, and neither are cases pytest generates dynamically or
collects under other naming rules. Missing or unreadable proof remains a
distinct explanation. A matching `fileBlob` shows only that the test file is
unchanged since the case passed, not that the product or its fixtures are.

## Commit proof

Commit the deliberately refreshed snapshot with the work it observed, or in
one intentional proof checkpoint. A recording run does not commit
automatically. Normal test-only work creates no proof change or proof-only
commit. Only committed proof is published Proof; uncommitted files, including
observations from an explicitly recording CI run, do not count.
