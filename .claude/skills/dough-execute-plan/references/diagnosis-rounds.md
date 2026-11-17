# Diagnose in evidence rounds

Rounds bound how long a failure's diagnosis runs. They never change whether
the repair is owed: the [failure policy](failure-policy.md) still owes it.

## Start rounds

A failure diagnosed and fixed within its slice's
[15 active minutes](oversized-slice.md#track-active-time) needs no record and
no round; the slice continues.

Diagnosis runs in rounds when its cause is still unknown and either:

- a slice reaches its limit while diagnosing; or
- an owned CI failure goes to a
  [repair agent](ci-monitor.md#handle-a-notification).

At a slice's limit, park the slice's unfinished attempt-owned changes: keep
them in the checkout and list them in the record. They may be the suspect.

## Run a round

A round has 15 active minutes on the slice clock. Start each round with the
same diagnosis name:

```sh
node '<installed-execute-plan>/scripts/slice-clock.mjs' start --identity '<identity>' --diagnosis '<name>'
```

`<name>` names the failure, for example its failing check. Run proof commands
through the clock's `run`, as in a slice.

Before every repeat batch and every CI run, write in the record the
hypothesis and stopping count that
[Diagnose failed proof](execution-decisions.md#diagnose-failed-proof) requires,
then start it.

When the clock reports `overrun: true` for the round, stop experimenting,
update the record, and pause the clock. Each round ends by updating the one
record.

## Keep the record

Planned execution keeps the record beside its plan, as
`diagnosis-<name>.md` in the plan's folder. Quick execution keeps it under
this project's executable-plan root, resolved as in
[slice planning](../../dough-slice-planning/SKILL.md#resolve-execution-context),
in a folder named for the execution identity. The record survives agents,
handbacks, refinements and resumes.

The record holds:

- each failure signature, and whether each reproduction matched it;
- the reproduction rate and its conditions: load, workers, platform, and
  other work running;
- the base-versus-candidate attribution, and whether this story's changes
  touch the failing code;
- CI run, job, attempt and artifact references;
- each hypothesis with its experiment and result;
- environments ruled out, with counts;
- the reproduction command, any instrument, and evidence actually captured;
- temporary diagnostic edits to revert, and the parked changes in the tested
  tree;
- unrelated defects found;
- the leading hypothesis and the next discriminating experiment;
- the round count and cumulative active minutes, from the clock's `round` and
  `cumulativeMinutes`.

## Continue or stop

Start the next round in a fresh context, from the record alone, only when the
last round made progress or the next round switches experiment kind. A fresh
context continues the same diagnosis name, so it resumes the round count and
cumulative minutes.

Progress means one of:

- the round first reproduced the signature, naturally or forced, raised its
  rate, or captured missing evidence;
- an instrument or experiment narrowed the cause;
- it confirmed a hypothesis; or
- it refuted a hypothesis while a matching reproduction exists.

After a round whose reproduction attempts all failed, the next experiment must
be a different kind: instrumentation, injection, or diagnostics that let the
next CI failure answer it. More repetition at higher load is the same kind.

Without progress or a kind switch, stop for a human, even before the cap.
After the authorized rounds, 3 by default, stop for a human.

## Confirm the cause and fix it

A cause is confirmed only by a reproduction that fails before the fix. That
reproduction becomes the fix's proof.

The fix gets its own 15-minute slice clock, not a round:

- A cause in the parked changes resumes that slice with the fix. Restart
  its clock with `start --slice '<slice>' --split-from '<slice>'`, so it
  starts at zero.
- A cause in committed or trunk code is fixed and delivered before the parked
  work resumes, as [CI repair](ci-monitor.md#handle-a-notification) orders it.

## Stop for a human

A stop keeps the record, the parked work and the owed repair. Report the
round count, cumulative active minutes, leading hypothesis, next
discriminating experiment, and any design judgment the evidence needs, under
[stop for human judgment](execution-decisions.md#stop-for-human-judgment).

Only the human's answer decides what follows:

- more rounds: record the authorization, then start the next round with
  `--rounds-authorized <total>`;
- a different approach; or
- the design judgment the stop named.
