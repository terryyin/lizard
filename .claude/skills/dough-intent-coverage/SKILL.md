---
name: dough-intent-coverage
description: >-
  Runs one targeted scratch break check when a developer explicitly requests
  Dough Intent Coverage during review. Reports whether maintained proof catches
  one plausible fault in an important story promise, and names the next proof
  action the evidence supports.
---

# Check one story promise

Use this skill on explicit request. Return evidence the caller can use to
decide the next proof action for the selected promise. Leave test repairs,
broader assessment, automation, persistence, and follow-up routing to their
owners.

## Choose the check

Capture the invocation start from the host clock when available. Otherwise
take a clock reading at the start of your work, before looking up the story
or preparing the checkout.

Identify the selected story and revision, the promise and its expected
observable behavior, and one plausible fault. Treat any caller-supplied
promise, fault, focused command, fixture, or assertion as a suggestion:
validate it against the selected revision and story, reuse what holds, and
explain a consequential revision when you change target, command, or
assertion. Prefer an existing maintained assertion that could catch the
fault when one is already in view. When no such assertion is known, select
the nearest relevant passing checks for that promise and plan an
independent scratch witness for the omitted path. Do not require a
nonexistent selected assertion before investigating a missing journey.
Missing hints do not prevent a check: search only for the narrowest
relevant proof. When proceeding would require a broad exploratory run,
name the remaining gap and stop rather than sweeping. A separately
authorized broader investigation remains outside this skill. Use this
project's recorded intent, maintained focused test command, and scratch
checkout conventions. Ask for the story source if you cannot establish the
promise. If other required context is missing, name what you need and stop
before editing.

## Run and restore

1. Prepare an isolated scratch checkout of the selected revision, or verify
   that a supplied checkout is already isolated at that revision. Record its
   pre-probe state so you can distinguish your edits from existing changes.
   Run the focused or nearest-relevant checks there. Require a passing
   baseline that actually exercised those checks before applying any fault.
2. Arrange to undo your temporary edit even if the run fails or is
   interrupted. Introduce one fault, leaving fixtures and assertions intact.
   Rerun the same focused or nearest-relevant command. Separately observe
   whether the promised behavior went wrong on the intended path—for a
   missing journey, that independent witness may be a scratch exercise of
   the omitted transition. A fault executing somewhere in the test is
   insufficient evidence that the selected behavior went wrong. A maintained
   check that never exercises the wrong path does not establish an assertion
   verdict.
3. Restore the edit, compare the scratch state to its pre-probe state, and
   rerun the same command. Verify that the restored checks pass. Preserve
   changes that were already present. If you cannot verify restoration or
   the restored pass, report the remaining gap; incomplete cleanup is not a
   successful probe.

## Return the result

Select one verdict from the same evidence rule: whether maintained proof
exercises the wrong path and catches it.

- **Caught.** A maintained assertion failed on independently observed wrong
  behavior on the path it exercised. Name the assertion that actually
  caught it, including an earlier helper when that is the catch, and limit
  the claim to the observed path.
- **Assertion gap.** A maintained check exercised the independently observed
  wrong behavior, yet its assertion passed. The next proof action is to
  strengthen that observing assertion.
- **Confirmed missing test path.** An independent scratch observation
  establishes that the fault causes the promised behavior to go wrong,
  while the relevant maintained checks pass and omit that path. Name the
  checks examined and the omitted transition. Bound the finding to those
  examined checks; do not claim absence across the whole suite. Merely
  finding no matching test, without wrong-behavior observation, does not
  confirm a missing path. The next proof action is to add journey proof for
  the omitted transition.
- **Insufficient evidence.** Identify the missing setup, passing baseline,
  reach, wrong-behavior observation, or restoration check. An unrelated
  failure, a green run without evidence of wrong behavior, or inspection
  that finds no matching assertion without a faulted observation belongs
  here. The next proof action is to gather the named missing observation.

State the next proof action separately from the verdict. Separately note any
already-planned repair or prior disposition the evidence matches, and
distinguish that recognition from a new repair decision caused by the probe.
Leave test repair and follow-up routing with the caller.

Cite the story and revision, fault location and change, fixture when used,
maintained checks or assertion examined, and run observations, including how
you verified reach, wrong behavior, and restoration. Return the finding and
its cost together to the caller. The caller owns any saved record. Persist
the report as free text; this skill adds no report schema or verdict enum.

Include four separate cost entries, even for an inconclusive attempt. Give
each figure's unit and basis and label it measured or estimated. Mark each
unavailable entry explicitly and give the reason.

- **Test runtime.** Report the test runner's runtime when observed.
- **Human effort.** Report a separately supported measure when available.
- **Agent effort.** Report a separately supported measure when available.
- **Invocation-to-report elapsed.** Include lookup, setup, and runs. A reading
  taken during tests or restoration measures only a partial interval.

Prepare the finding, evidence, and first three cost entries. When Node is
available, use [the installed report finisher](./finish-report.mjs) as the
final tool operation. It writes the prepared body, samples the end clock,
and appends the elapsed entry. Set the directory to the skill you loaded.
Pass the observed invocation start in Unix epoch milliseconds from the same
clock, or omit that argument when it is unavailable or incomparable.

```sh
node "$intent_coverage_skill_dir/finish-report.mjs" "$intent_coverage_start_ms" <<'REPORT'
<prepared report body>
REPORT
```

Return the finisher's output directly. Its cutoff is report production before
later delivery. If Node is unavailable, use an observed complete host interval
with its actual cutoff, or mark complete elapsed unavailable and give the
endpoints of any measured portion.
