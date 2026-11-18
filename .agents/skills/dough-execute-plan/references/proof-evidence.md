# Retain proof evidence

## Evidence location

From the execution checkout, resolve `git rev-parse --git-path open-dough/evidence`
and create that directory. Keep the execution's logs, exit files, run records
and worker reports there, uncommitted and bound to that checkout. This Git
location is outside any directory the test runner clears. Preserve the evidence
until the checkout is retired; retiring a worktree removes its Git directory.
Choose a fresh name for each run so later commands do not overwrite earlier
evidence.

## Read the run record

Place `--log` and `--exit-file` in the evidence location when using the
[slice clock's `run`](oversized-slice.md#track-active-time). It writes
`<log>.run.json` beside the log. From launch, the record holds `command`
(the literal command and arguments), `revision` (the checkout's HEAD),
`uncommittedChanges`, `startedAt`, `log` and `exitFile`. The last two are
resolved paths. After the command ends, it gains `endedAt` and `exitStatus`
before the exit file is written.

Take the command, revision and terminal result from the record. Retain its
path with the host's supported continuation handle. A record with no
`exitStatus` has no recorded terminal result yet; use it to identify and
continue the existing command under [waiting](wait-for-proof.md) and the
[overdue check](overdue-delegated-verification.md), rather than launching a
second run. The record does not replace the host's continuation facility.

## Preserve decisive artifacts

When a decision rests on a diagnostic artifact the runner may remove or
overwrite, direct it to the evidence location or copy it there before starting
another run. Before an expensive diagnostic run, first confirm on a small run
that the artifact survives runner cleanup in that location. A passing rerun
does not recover a lost failing trace.

## Read large output selectively

Read from the log only the failing assertion or summary needed for the
decision, under [targeted retrieval](disposable-research.md). Keep that
observation's location. Do not load the whole log or rerun the suite to recover
the command, revision or result already retained in the run record.
