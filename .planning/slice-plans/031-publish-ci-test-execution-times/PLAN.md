# Publish Lizard test execution times in Intent

**Identity:** SEED-051#publish-ci-test-execution-times
**Source:** [canonical story](../../seeds/SEED-051-publish-ci-test-execution-times.md#publish-ci-test-execution-times)
**Source review:** [Isolated canonical story review](source-review.md)

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
Status: done
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
Status: done
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
- Established checkout: `/Users/terryyin/git/lizard/.worktrees/publish-lizard-test-execution-times-in-intent`,
  existing owned workspace; execution branch `codex/publish-lizard-test-execution-times-in-intent`.
  Integration checkout: `/Users/terryyin/git/lizard`. Authorized trunk: `origin/master`.
  Publisher: `dashboard-territory.local-lizard`; agent: Koharu-chan.
  Starting revision: `1ed475f48512ac26d06a89ebf8a6fc798e572a3e`.
  Published claim (trunk and story branch) and starting candidate:
  `1e11c01c3ef0e4d3d76232a8f920d9ac6753027e`.
- Codex CI observer: GitHub Actions, `terryyin/lizard`, `python-app.yml`
  (Python application), story-branch target. Key
  `ci-watch-execution:terryyin/lizard:codex/publish-lizard-test-execution-times-in-intent:lizard-ci-times-koharu`;
  coordinator `lizard-ci-times-koharu`, directory `/tmp/dough-ci-501/watch-d7fbF6`,
  PID 21915, session 30703, cell `g60:6`. Trunk claim coverage is unobserved.
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


## Story obligations

### G1. The shared collector drops untimed checks in a partly timed file
Reported: slice 1 — "The slice 1 JUnit replay produced two timing lines and three outcomes for test/test_example.py. The summary preserved 375 ms and all three outcomes but reported zero untimed at the entry and part, and omitted untimed on the file row. Each should report one."
Story clause: "Those checks count as untimed in the file breakdown"
Disposition: proved by slice 1: /tmp/lizard-slice1-proof.PffZtQ/replay.sh now observes untimed 1 for entry, present part, and file with installed Open Dough 0.3.89
Commitment: E15

### G2. Website file breakdown and times are pending the website producer
Reported: slice 1 — "Run 42381491501 shows the website job's 16 counts and one present part, with times and per-file figures still absent in Intent."
Story clause: "each present part carries entry-level `durationMs` and a file breakdown Intent can show"
Disposition: proved by slice 2: run 42382227121 artifact and Intent branch comparison at b10a6fd show website duration and access.feature breakdown

## Slice 1 execution evidence

- The existing CI proof recipe's timing implementation is a blocking premise.
  In the installed Open Dough v0.3.88 collector, the first timing line for a
  file resets that file's whole `untimed` count. Its shared source on Open
  Dough `main` at `cf660eed18c221f31bc89b4163298e8b47f013d6` has the same
  blob as the installed copy. Both Lizard producer formats need a common
  correction. The story's Scope explicitly requires byte-identical copies of
  the installed modules and the Key examples require accurate untimed counts.
- Replay from this checkout with
  `nix develop -c sh /tmp/lizard-slice1-proof.PffZtQ/replay.sh`.
  The converter preserves the failed and untimed outcomes and emits timing
  lines for the 125 ms pass, the 250 ms failure, and the zero-duration pass
  in a second file. The installed collector reports `(0, 0, absent)` at
  entry, part, and file where the replay asserts `(1, 1, 1)`.
  The report and summary are in the replay directory.
- A disposable collector trial at
  `/tmp/lizard-ci-collector-trial.4anSG2` counts timing lines per file,
  subtracting that count from the file's outcomes while summing durations.
  It passes the same replay and all 31 shared CI proof tests. The trial is
  outside both repositories.
- Resume after a corrected shared collector is available for installation,
  or after the developer changes the byte-for-byte requirement in the story.
  Slice 1 is still planned. Its converter, matching collector copies,
  representative workflow wiring, local regression test, and temporary
  JUnit report upload for live proof remain uncommitted. No timed CI attempt
  was published or shown in Intent. Slice 2 has not started.
- Focused pytest passed all 16 cases. Full `nix develop -c python -m pytest`
  passed with 3,093 passed and 7 skipped. `nix develop -c make` passed.
- The observer at `/tmp/dough-ci-501/watch-d7fbF6` stopped with no unread
  events and confirmed local exit. The published claim remains the branch base.

### Resume with corrected Open Dough

Lizard trunk supplied Open Dough v0.3.89 in `8a0ae5775`. The story branch
fast-forwarded locally from the published claim and kept its existing edits.
All five collector modules match their installed sources. The same replay
now asserts 375 ms and one untimed check at the entry, part and file while
retaining the failure and measured zero. The earlier stop no longer applies.

The restarted Codex observer uses the same coordinator and target, directory
`/tmp/dough-ci-501/watch-ybtG4B`, PID 80988, session 43511, cell `g86:5`.
The published branch base remains the original claim until progress delivery.

Independent refactor review found no necessary edits. On the resumed candidate
`nix develop -c python -m pytest` and `nix develop -c make` passed with
3,093 passing tests and 7 skips. The one-day JUnit artifact on the story
branch enables comparison with the exact GitHub runner input.

Hosted proof passed on the story branch at
`0ef480784998a9e60c30768575973ce32fe53954`,
[run 42381491501, attempt 1](https://github.com/terryyin/lizard/actions/runs/42381491501).
Downloaded the exact JUnit XML and `ci-proof-summary-attempt-1`; independent
XML aggregation matched every count and duration on the 157 file rows,
the entry, and its present part. The summary reported 3,093 passed, 7 other,
12,179 ms total and 2 passed / 40 ms for `test/test_ci_proof.py`.
Intent's selected story branch CI status showed that run, counts and
12.2 s; its test-scope breakdown showed the same file with 2 checks / 40 ms.
Website counts are present without timing in this run, the planned interim
state addressed in slice 2.

## Slice 2 execution evidence

The converter retains count-only invocation and emits one optional timing
line per scenario with any duration. It sums before hooks, steps and after
hooks as nanoseconds before converting to seconds. The existing conditional
website fragment uses the same timing and breakdown flags as the analyzer.
Local Nix focused pytest passed 18 tests. The dual-converter replay
`nix develop -c sh /tmp/lizard-slice2-proof.1FafRK/replay.sh` verified a
present website part with 2 passed, 1 failed, 1 other, 1,000 ms and 1 untimed,
including same-file and measured-zero cases, plus the skipped website state.
Independent refactor review found no necessary edits.

Full Nix pytest and `make` passed with 3,095 passing tests and 7 skips.
Both temporary one-day runner reports are retained for comparison with this
progress publication. They will be removed after the hosted observations.

Website-enabled hosted proof passed at `b10a6fd1cb3aedaebfa9a71b787e7a7e9a70bd9d`,
[run 42382227121, attempt 1](https://github.com/terryyin/lizard/actions/runs/42382227121).
Downloaded the summary and both runner reports. Independent aggregation matched
all 157 analyzer files and 5 website files, entry counts, durations and part
flags. Analyzer reported 3,095 passed, 7 other and 13,324 ms.
Website reported 16 passed and 25,474.361311 ms. Intent showed 13.3 s and
25.5 s for that run. Its file rows showed `test/test_ci_proof.py` with
4 checks / 189 ms and `website/frontend/features/access.feature` with
3 checks / 803 ms, matching the raw reports at display precision.
The next proof publication changes only planning records so the workflow
observes the website-skip case on the same product code.

Skipped-website hosted proof passed at `f2c6dc7bf687d252e442934f02205ec33a3a1fa4`,
[run 42382720864, attempt 1](https://github.com/terryyin/lizard/actions/runs/42382720864).
GitHub skipped `web` and passed the collector. The artifact had website
part 1 missing and no duration, plus a present analyzer part at 14,029 ms
with 157 file rows. Intent showed 14.0 s and website part 1 of 1 missing;
the analyzer file row showed 4 checks / 253 ms.

The two temporary runner-report uploads were removed after evidence capture.
That removal stays inside the workflow concepts traced in the independent
refactor return. The production fragments and summary continue to upload.
Full Nix pytest and `make` passed again with 3,095 passing tests and 7 skips.
