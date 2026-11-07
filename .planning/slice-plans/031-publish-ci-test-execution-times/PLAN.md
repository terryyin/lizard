# Publish Lizard test execution times in Intent

**Identity:** SEED-051#publish-ci-test-execution-times
**Source:** [canonical story](../../seeds/SEED-051-publish-ci-test-execution-times.md#publish-ci-test-execution-times)

## Goal and scope

A Lizard maintainer comparing analyzer and website CI runtimes sees, in
Intent's **CI status** tab for a judged `python-app.yml` attempt, cumulative
summed test execution time and a per-file breakdown for each reporting proof
entry (`test`, and `website/frontend/features` when that job ran), and sees
the website part as missing when `web` did not run.

Follow the [installed CI proof recipe](../../../.agents/skills/dough-test-optimization/references/ci-proof-summary.md)
in [`python-app.yml`](../../../.github/workflows/python-app.yml). Reuse the
SEED-035 wiring (representative Python leg, conditional `web`, collector,
artifact names). Re-copy the five collector modules unchanged; extend the
project converters and fragment steps with `--times` and `--by-file`.

**Excluded:** wall-clock job/step duration; timings from non-representative
matrix legs; `ciAdapter` publishing; Intent UI changes; matrix / website-change
/ `web` condition changes.

**Assumptions:** GitHub Actions only (no `ciAdapter`); optional `durationMs`
and file breakdowns are what Intent already reads when present; the
developer's `gh` reads this repository's Actions artifacts.

## PFE and architecture

**Responsibility:** publish attempt-scoped test execution times and file
breakdowns into the existing CI proof summary.

**Existing solution:** SEED-035's CI proof path (`scripts/ci-proof/` converters
+ `python-app.yml` producers/collector) plus the installed recipe's optional
`durationMs` / `fragment --by-file` support in the skill scripts (including
`ci-proof-figures.mjs`). Change that path: refresh the vendored collector and
extend the converters and fragment invocations. Do not add a second proof
pipeline or fork the collector.

No Accepted ADR governs CI proof (ADR-0000, ADR-0001); no conflict. No North
Star topic warranted.

## Outside-in proof

| Promise (seed key example) | Owner | Observable signal |
| --- | --- | --- |
| 1 — both jobs: counts, `durationMs`, and a file from each match the reports | Slice 2 (`test` from slice 1) | Real run with `web`: downloaded summary has both entries present with `durationMs` and `files`; spot-check one file per entry against the runner reports / local conversion of those reports; Intent shows times and breakdown |
| 2 — website skipped → website missing; `test` still timed | Slice 2 | Real run with `web` skipped: summary shows `website/frontend/features` missing and `test` with `durationMs` + files; Intent shows the same |
| 3 — partial times → untimed checks, part still present | Slice 1 (local) | Local fragment with some outcomes lacking times lines: those paths `untimed`; part present with partial `durationMs` |
| 4 — failed tests still timed when duration exists | Slice 1 (local) + natural CI | Local failed outcome with a times line contributes to `durationMs`; CI keeps `!cancelled()` fragment upload |
| Existing counts and structure preserved | Slices 1–2 | `test/test_web_changes.py` and `test/testPythonVersionClaims.py` pass; full `nix develop -c make` |

Local gate: `AGENTS.md` — full suite and `nix develop -c make` before
completing a change. Real CI runs are the promised Intent boundary; local
replays settle converters and collector shape.

## Slices

### 1. Analyzer suite times and file breakdown reach Intent
Type: Behavior
Status: planned
Proof: a real `python-app.yml` run on the story branch publishes
`ci-proof-summary-attempt-<n>` whose `test` part is present with
`durationMs`, `breakdown: true`, and `files` whose counts and times match a
local conversion of that leg's JUnit report for at least one file; Intent's
CI status tab shows the times and breakdown for that run; full local suite
and `make` pass. Early probe: if the summary lacks timings/files or the tab
does not render them, stop and replan before slice 2.

Behavior: a push to the story branch → the representative `build` leg writes
a `test` fragment with `--times` and `--by-file` from pytest JUnit → the
collector publishes the summary → Intent shows cumulative time and per-file
figures for `test`.

Work:

- Re-copy the five collector modules unchanged from
  `.agents/skills/dough-test-optimization/scripts/` into `scripts/ci-proof/`
  (`ci-proof-summary.mjs`, `ci-proof-contract.mjs`, `ci-proof-figures.mjs`,
  `ci-proof-fragments.mjs`, `ci-proof-collect.mjs`).
- Extend `scripts/ci-proof/junit-outcomes.py` to emit a times TSV (same check
  paths as outcomes, seconds from `testcase/@time`) alongside outcomes—for
  example a second output path argument—so missing `time` skips the times
  line (untimed) without dropping the outcome.
- `python-app.yml` representative-leg fragment step: write outcomes and times,
  then `fragment --outcomes … --times … --by-file` (keep existing entry/part/
  attempt/revision/out).
- Local replay of example 3 (partial times) before relying on CI.
- Observe: push, download the summary with `gh`, spot-check one `test` file,
  and confirm Intent rendering (browser automation on the developer's
  dashboard if available, otherwise ask the developer to look).

Interim behavior (accepted, replaced by slice 2): website entry may still be
count-only or missing times until its fragment step gains `--times` /
`--by-file`; when `web` runs under the old website fragment, the summary may
show website counts without `durationMs`/breakdown.

### 2. Website scenario times, or their absence, reach Intent
Type: Behavior
Status: planned
Proof: one real run with `web` → summary `website/frontend/features` present
with `durationMs`, breakdown, and at least one file matching the Cucumber
report conversion; one real run with `web` skipped → that part missing while
`test` remains timed; Intent shows both states; full local suite and `make`
pass.

Behavior: a push that runs `web` → the website fragment carries times and
files → Intent shows them; a push that skips `web` → the collector still
runs and the website part shows missing.

Work:

- Extend `scripts/ci-proof/cucumber-outcomes.mjs` to emit times: sum each
  scenario's before/step/after `result.duration` values (nanoseconds →
  seconds); omit the times line when none report duration; keep the same
  `website/frontend/<uri>` path labels as outcomes.
- `python-app.yml` `web` fragment step: write outcomes and times, then
  `fragment --outcomes … --times … --by-file`.
- Observe both runs and Intent as in slice 1.

## Current decisions

- Representative leg and workflow structure stay as SEED-035 left them
  (`ubuntu-24.04` / `3.13`; collector `needs: [build, web]`; artifact names
  unchanged).
- Collector modules are refreshed from the skill, never hand-edited for this
  story; timing belongs in converters and fragment CLI flags.
- Execution mode: Story Branch Mode; every push runs `python-app.yml`, so
  story-branch pushes give the real runs.
- Preserve workflow shapes that `test/test_web_changes.py` and
  `test/testPythonVersionClaims.py` assert.

## Decisive premises

| Premise | Consuming operation | Observation (form / profile) | When / revision | Result |
| --- | --- | --- | --- | --- |
| Installed collector with `--times` and `--by-file` writes entry `durationMs` and `files`, and `summarize` keeps them while still marking a missing expected part | `node …/ci-proof-summary.mjs fragment` / `summarize` on fixture TSVs | Skill scripts under `.agents/skills/dough-test-optimization/scripts/` (the modules slice 1 copies) | 2026-11-07, workspace `publish-lizard-test-execution-times-in-intent` | Fragment: `durationMs: 35`, two `files` with times; summary: `test` present with files, `website/frontend/features` part 1 missing |
| Pytest xunit1 JUnit supplies `testcase/@time` in seconds with repository-relative `file` under `test/` | `pytest -o junit_family=xunit1 --junitxml=…` then planned converter | `nix develop -c python -m pytest` on a scratch file under `test/` (reverted) | 2026-11-07 | `file=test/…/….py`, `time=0.001` (seconds) |
| Cucumber-js JSON exposes optional step `result.duration` in nanoseconds; scenario seconds are their sum / 1e9 | Planned cucumber converter | cucumber-js v13.3.0 `json_formatter.ts` + `duration_helpers.ts` (upstream); synthetic scenario math in this workspace | 2026-11-07 | Duration field is ns; sample before+steps+after → `2.501` seconds on `website/frontend/features/….feature` |
| Project collector copies are older than the skill (no `durationMs` / `--by-file`; missing `ci-proof-figures.mjs`) | Re-copy in slice 1 | `diff` of project vs skill modules; `ls scripts/ci-proof` | 2026-11-07 | All four existing modules differ; `ci-proof-figures.mjs` absent from project |
| Intent renders optional `durationMs` and file breakdowns for a live Lizard summary | Dashboard CI status tab for a real run | Recipe contract; `gh` route known from SEED-035 | 2026-11-07 | Unverified on a timed Lizard summary — bounded by slice 1 as early probe |

## Concern evidence

Slice-plan refinement: not needed — two Behavior slices, each one entry's
timed end-to-end outcome with its own real-run proof; interim untimed website
during slice 1 is named and replaced by slice 2; no fragmented or combined
concerns.

Remaining slice-specific concerns: none identified in this review. The
Intent-rendering premise for timed summaries is bounded by slice 1 as the
early probe.
