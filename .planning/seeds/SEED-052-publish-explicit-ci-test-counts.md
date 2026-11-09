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
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

**Goal.** Lizard maintainers can see how many analyzer test cases and website
scenarios the selected CI attempt recorded in Intent.

**Scope.** Adopt the new format in the existing CI proof publisher for the representative
analyzer pytest leg and the website Cucumber job. Preserve suite locations,
outcomes, file breakdowns, and available execution times.

Open Dough 0.3.93 publishes these units. Update Open Dough guidance to
0.3.93 or later with `dough-update`, then copy all seven `ci-proof-*.mjs`
collector modules into `scripts/ci-proof/` through the CI proof copy recipe
(`ci-proof-evidence.mjs` and `ci-proof-options.mjs` are new). In
`.github/workflows/python-app.yml`, pass `--unit case` to both `fragment`
steps and declare `--expect test=1:case --expect
website/frontend/features=1:case`. A job that wrote no report stays a
missing part. Until both compared builds declare matching units, Intent
reads unit-less results as legacy test results and shows no count or time
difference.

**Key examples.**

- The new summary reports one test case per pytest result or Cucumber
  scenario result. For the same CI attempt, Intent and both runner reports
  agree on the case totals and outcomes.
- The `junit-outcomes.py` lines from the ubuntu-24.04 / Python 3.13 leg's
  `pytest-junit.xml` count as test cases of `test`, and the
  `cucumber-outcomes.mjs` scenario lines count as test cases of
  `website/frontend/features`; comparing two migrated builds shows each
  location's case count difference.
