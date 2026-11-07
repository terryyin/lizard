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
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

**Beneficiary:** A Lizard maintainer comparing the runtime of Python and website tests in CI.

**Goal:** Intent shows cumulative execution time and a file breakdown for the Python and website tests that report in the judged CI attempt.

**Scope:** Collect durations and repository-relative file paths from pytest JUnit and
website Cucumber reports. Keep the representative Python matrix leg used by
the existing proof and add a timed website fragment when that job runs. The
existing CI collector publishes one `ci-proof-summary-attempt-<n>` artifact
for the attempt.

Follow the [installed CI proof recipe](../../.agents/skills/dough-test-optimization/references/ci-proof-summary.md).

**Acceptance:** For a run that exercises both jobs, the downloaded summary and Intent match
the runner reports on counts and timings for each entry and a file from each.
A run that skips the website job shows its missing proof explicitly.
