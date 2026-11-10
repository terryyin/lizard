---
id: SEED-052
status: active
planted: 2026-11-09
scope: story
---

# See Lizard test case counts in Intent

<a id="publish-explicit-ci-test-counts"></a>

### See Lizard test case counts in Intent

**Identity:** SEED-052#publish-explicit-ci-test-counts
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/032-explicit-ci-test-counts/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"8c6f706c97977d02cdf9ee7e381a23c37095899557f0284e6359f48ecf7b9b7c","plan":"4604315a7da4f00008a4b9951f2c7e39f5c2bf73e20f468c139d56c131b99d11"}}
```

**Goal.** Lizard maintainers can see how many analyzer test cases and website
scenarios the selected CI attempt recorded in Intent, and compare case counts
between builds that both declare the case unit.

**Scope.** Adopt explicit test case units in the existing CI proof publisher
for the representative ubuntu-24.04 / Python 3.13 analyzer pytest leg and the
website Cucumber job. Each pytest result and each Cucumber scenario result is
one case, including failed and other outcomes. Preserve the configured
locations `test` and `website/frontend/features`, outcomes, file breakdowns,
available execution times, and attempt/revision identity.

Use Open Dough's released CI proof copy recipe and its seven collector
modules, without a Lizard-specific format. The version prerequisite is
0.3.93 or later; this preparation already has 0.3.94 installed. An older
installation is updated with `dough-update` before copying the collector.
Both producers declare `case`, and the collector expects one case part for
each location. A job that wrote no report, including a conditionally skipped
website job, stays a missing part rather than claiming a successful empty run.

Until both compared builds declare matching units, Intent reads unit-less
results as legacy test results and shows no count or test execution time
difference: interpreting those results as cases would create a misleading
comparison. Existing upstream Intent behavior supplies this compatibility;
this story changes Lizard's publisher.

Scope limits: no additional matrix legs, file-unit publication, changes to
website job selection, analyzer behavior, or new Intent UI behavior.

**Key examples.**

- Given four pytest results in two files (two passed, one failed, one
  skipped), publishing the representative leg reports four cases at `test`,
  with outcomes 2 passed / 1 failed / 1 other, retaining the two-file
  breakdown and available timings. It does not report two test files as the
  case count or multiply the count across matrix legs.
- Given a Cucumber report with a background and three scenarios (passed,
  failed, skipped), publication reports three cases at
  `website/frontend/features`, with outcomes 1 / 1 / 1. The background and
  individual steps are not additional cases; available hook and step times
  remain part of each scenario's execution time.
- Given both reports for one attempt, its summary and Intent agree with the
  runner case totals and outcomes for both locations. A missing report leaves
  that location's declared case part missing and uncounted.
- Given two migrated builds whose case totals change from 4 to 5 and from
  3 to 4, their Intent comparison shows +1 case at each location. Given an
  older unit-less build and a migrated build, the older results remain
  legacy results and neither a count nor a test execution time difference
  is shown across the incompatible units.
