---
name: dough-intent-coverage
description: >-
  Runs one targeted scratch break check when a developer explicitly requests
  Dough Intent Coverage during review. Reports whether an existing test catches
  one plausible fault in an important story promise.
---

# Check one story promise

Use this skill on explicit request. Return evidence the caller can use to
decide whether to retain or strengthen the selected test protection. Leave
test repairs, broader assessment, automation, and persistence to their owners.

## Choose the check

Capture the invocation start from the host clock when available. Otherwise
take a clock reading at the start of your work, before looking up the story
or preparing the checkout.

Identify the selected story and revision, the promise and its expected
observable behavior, one plausible fault, and an existing assertion that
could catch it. Use this project's recorded intent, maintained focused test
command, and scratch checkout conventions. Ask for the story source if
you cannot establish the promise. If other required context is missing,
name what you need and stop before editing.

## Run and restore

1. Prepare an isolated scratch checkout of the selected revision, or verify
   that a supplied checkout is already isolated at that revision. Record its
   pre-probe state so you can distinguish your edits from existing changes.
   Run the focused test there. Require a passing baseline that actually
   exercised the selected test before applying any fault.
2. Arrange to undo your temporary edit even if the run fails or is
   interrupted. Introduce one fault, leaving the fixture and assertion
   intact. Rerun the same focused command. Check that the behavior changed
   on the intended path and whether the run reached the intended assertion.
   A fault executing somewhere in the test is insufficient evidence that
   the selected behavior went wrong.
3. Restore the edit, compare the scratch state to its pre-probe state, and
   rerun the same command. Verify that the restored check passes. Preserve
   changes that were already present. If you cannot verify restoration or
   the restored pass, report the remaining gap.

## Return the result

Report one outcome for the selected promise, fault, and observed path:

- **Caught.** The fault reached the intended assertion, which failed on the
  changed behavior.
- **Assertion gap.** The intended assertion passed and independent evidence
  shows that the fault produced wrong behavior on the path it exercised.
- **Insufficient evidence.** Identify the missing setup, passing baseline,
  reach, behavior observation, or restoration check. An unrelated failure
  or a green run without evidence of wrong behavior belongs here.

Cite the story and revision, fault location and change, fixture, maintained
assertion, and run observations, including how you verified reach and
restoration. Return the finding and its cost together to the caller. The
caller owns any saved record.

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
