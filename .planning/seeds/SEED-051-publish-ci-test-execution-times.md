---
id: SEED-051
status: active
planted: 2026-11-07
scope: story
---

# Publish Lizard test execution times in Intent

<a id="publish-ci-test-execution-times"></a>

### Publish Lizard test execution times in Intent

**Identity:** SEED-051#publish-ci-test-execution-times
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/031-publish-ci-test-execution-times/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"0e104bedbcd03927f8fbc5a79ba3505dc9bb605bfb3a95e394c2860c0407c719","plan":"ac88b5ea4d2e0c9e31432c5e2014dfe25d564a09ab0ad05829df335622435e42"}}
```

**Beneficiary:** A Lizard maintainer comparing the runtime of Python and website tests in CI.

#### Goal

A Lizard maintainer looking at a judged `python-app.yml` attempt in Intent's
**CI status** tab sees, for each reporting proof entry, the cumulative summed
test execution time and a per-file breakdown of counts and times, so they can
compare the analyzer suite (`test`) with the website scenarios
(`website/frontend/features`) without opening job logs. When the website job
does not run, its proof stays explicitly missing.

#### Scope

Required behavior:

- Follow the [installed CI proof recipe](../../.agents/skills/dough-test-optimization/references/ci-proof-summary.md).
  Re-copy its five collector modules unchanged into `scripts/ci-proof/`
  (including `ci-proof-figures.mjs`), replacing the older count-only copies
  from SEED-035. Schema versions stay at 1; timings use optional `durationMs`.
- Keep the existing judged workflow, Intent locations, representative Python
  matrix leg (`ubuntu-24.04` / `3.13`), conditional `web` job, collector job,
  and `ci-proof-summary-attempt-<n>` artifact naming.
- Extend the pytest JUnit and Cucumber JSON converters so each also emits a
  `--times` file: one `<seconds>\t<repository-relative check path>` line per
  check, using the same path labels as the outcomes file. Pytest's JUnit
  `time` is already seconds; Cucumber JSON step `result.duration` is
  nanoseconds and must be converted to seconds (scenario time is the sum of
  its before/step/after durations that report one).
- Both producer fragment steps pass `--times` and `--by-file` so each present
  part carries entry-level `durationMs` and a file breakdown Intent can show.
- A run that skips `web` still publishes the summary with the website part
  missing; the Python part may still carry times and files.

Required restrictions:

- Only the representative Python leg reports `test` timings (same reason as
  SEED-035: every matrix leg would inflate totals).
- Do not invent a new summary schema or change Intent: optional `durationMs`
  and `fragment --by-file` are the recipe's existing contract.

Scope limits (not undertaken, not prevented):

- Wall-clock job or step duration; `durationMs` is summed test execution time
  from the runner reports, as the recipe defines.
- Timings from non-representative matrix legs.
- Publishing times through a `ciAdapter` (this project has none).
- Changing the matrix, website change detection, or the `web` job's condition.
- Intent UI work beyond what the dashboard already does when a summary carries
  `durationMs` and a file breakdown.

Boundary assumptions:

- Intent's CI status tab already renders optional `durationMs` and per-file
  breakdowns when the summary carries them (recipe contract; confirmed on a
  real run during execution).
- The developer's `gh` can read this repository's Actions artifacts.

Deferred promises (not this increment):

- Cross-leg or historical trend views of the same timings.
- Separate setup/install time beside test execution time.

#### Key examples

1. **Both jobs report, times and files present.** A push that runs `build` and
   `web`. → The downloaded `ci-proof-summary-attempt-<n>` and Intent show, for
   `test` and for `website/frontend/features`, matching passed/failed/other
   counts and `durationMs` against the runner reports, and at least one file
   from each entry with matching counts and time.
2. **Website skipped.** A push that skips `web`. → The summary and Intent show
   `test` with times and file breakdown, and `website/frontend/features` as
   missing proof (not zero duration).
3. **Partial times in a report.** A present part whose report omits duration on
   some checks. → Those checks count as untimed in the file breakdown; the
   part still publishes, and `durationMs` reflects only timed checks.
4. **Failed tests still timed.** A failing check on a reporting job. → The
   fragment still uploads (`!cancelled()`), the failure appears in counts, and
   available durations still contribute to `durationMs` and the file row.

#### Architecture

No Accepted ADR governs CI proof (ADR-0000 adopts ADRs; ADR-0001 is the Lizard
domain glossary). Nothing conflicts. The consequential choice is to keep the
collector as vendored skill modules and add timing only through the recipe's
optional fields and project-owned report converters beside them—no fork of the
collector, no product-code change, schemaVersion remains 1.

#### Refinement notes

- **Directions compared.** (a) Recipe-native `--times` + `--by-file` on the
  existing producers after re-copying the collector — recommended: matches the
  installed contract Intent already reads, reuses SEED-035 wiring, and
  delivers both cumulative time and file breakdown. (b) Entry-level
  `durationMs` only, without `--by-file` — simpler, but misses the stated file
  breakdown. (c) Publish raw JUnit/Cucumber artifacts for Intent to parse —
  moves contract into Intent and out of this project's CI-only scope. (d) Use
  GitHub Actions step/job wall-clock — wrong metric for test execution time and
  no per-file breakdown.
- **Borrowed mechanism.** CI test-analytics pipelines (runner exporters →
  normalized attempt summary → dashboard) map producers to exporters, the
  collector to the aggregator, and Intent to the dashboard. The analogy breaks
  where live query APIs would replace a frozen attempt-scoped artifact: this
  story keeps the artifact identity rules from the recipe.
- **Strongest doubt.** Maintainers might read cumulative time as job wall-clock
  or as the whole matrix. The purpose still holds if the promise is explicitly
  summed test execution time from the reporting parts only (representative
  Python leg + website when run)—the recipe's definition—not wall-clock and
  not every matrix leg. That clarification is in Scope, not a goal change.
- **Investigation.** Facts: judged workflow is `python-app.yml` with Intent
  locations `test` and `website/frontend/features` and no `ciAdapter`
  (`.planning/open-dough.json`); SEED-035 already publishes count-only
  fragments and `ci-proof-summary-attempt-<n>`; project `scripts/ci-proof/`
  copies lack `durationMs` / `--by-file` and omit `ci-proof-figures.mjs`, while
  the installed skill scripts include them; pytest xunit1 JUnit writes
  `testcase/@time` in seconds (local probe 2026-11-07); cucumber-js 13.3.0 JSON
  formatter writes optional step `result.duration` in nanoseconds. Remaining
  hypothesis for execution: Intent's tab rendering of those optional fields on
  a live Lizard summary (recipe-stated; early real-run proof).
- **Stress-test.** Website skipped → missing website proof (example 2). Missing
  or partial durations → untimed checks, part still present (example 3). A
  failed converter or fragment step → that part missing (existing collector
  rule; no new promise). Interrupted collector → no summary artifact (existing
  failure mode). No additional failure behaviors need scope without changing
  those rules.
