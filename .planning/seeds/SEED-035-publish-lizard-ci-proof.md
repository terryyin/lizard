---
id: SEED-035
status: active
planted: 2026-11-05
planted_during: Terry Yin's request to prioritize CI proof adoption
trigger_when: A Lizard developer reviews analyzer and website test evidence for a CI run
scope: story
---

# SEED-035: Publish Lizard CI proof for Intent

<a id="publish-lizard-ci-proof"></a>

### Publish Lizard CI proof for Intent

**Identity:** SEED-035#publish-lizard-ci-proof
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/023-publish-lizard-ci-proof/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"a68915dee9715d90323b1b717bc1bc399207e8f6b44c9daf9b11fc329a49901c","plan":"973755e5a0964173517cebe32238d58327fccdd758fe3282251f90e719956839"}}
```

**Outcome:** A Lizard developer can see the analyzer and website test
outcomes in the Open Dough dashboard's **Intent → CI status** tab for
the shown Python application workflow run.

Use the [installed CI proof recipe](../../.agents/skills/dough-test-optimization/references/ci-proof-summary.md)
with [the judged workflow](../../.github/workflows/python-app.yml). Configure
`test` as an Intent location and use a named representative pytest matrix
leg for its counts so repeated runs on other versions and operating systems
do not inflate totals. Use the configured `website/frontend/features`
location when the conditional browser job runs. Keep the coverage of the
existing matrix and the condition for the website job.

**Acceptance:** A real CI run covering the website publishes both entries
and the dashboard displays the artifact's totals. A run that skips the
website job visibly lacks website proof. Test failures remain visible in
the workflow verdict and any available counts.

#### Goal

A Lizard developer looking at a Python application run in Intent's
**CI status** tab sees, beside the run's verdict, how many analyzer tests
(`test`) and website scenarios (`website/frontend/features`) that run
passed, failed or did something else, and sees plainly when the website
part was not run. This replaces reading job logs to learn what a run
proved, and gives the dashboard's CI proof its first live Lizard use.

#### Scope

Required behavior:

- `.planning/open-dough.json` declares `test` as a `python` Intent
  location, ahead of or beside the existing `website/frontend/features`
  location, selecting pytest's own files (`setup.cfg` sets
  `python_files = test*`). The existing Gherkin location, its proof
  directory and its proof generation command stay unchanged.
- One named matrix leg of the `build` job is the representative leg. Only
  it writes the `test` fragment, from a per-test pytest report, as part 1
  of 1. Recommended leg: `ubuntu-24.04` with Python `3.13`, the same
  interpreter the website job uses. The workflow names that leg where the
  fragment step's condition is written.
- The `web` job writes the `website/frontend/features` fragment from a
  per-scenario Cucumber report of its existing browser test step, as part
  1 of 1. Cucumber's own scenario results are the unit; the separate
  `test:proof` Node tests are not an Intent location and are not counted.
- One collector job in `python-app.yml` runs after `build` and `web`
  however they ended, including when `web` was skipped, expects both
  entries with one part each, and publishes
  `ci-proof-summary-attempt-<n>`.
- The CI scripts the recipe names (the four collector modules and the
  report conversions) live in this repository and the workflow runs those
  copies.
- Fragments are written after failing tests too, so failures appear in
  the counts as well as the verdict.

Required restrictions:

- Only the representative leg reports `test`. Reporting every leg would
  multiply the totals by the matrix size (14), which the developer ruled
  out.

Scope limits (not undertaken, not prevented):

- Counts from non-representative legs. A failure that happens only on
  another operating system or Python version shows in the run's verdict,
  not in the `test` counts.
- Committed pytest proof for the `test` location (a `proof` directory and
  proof generation); this story publishes CI observations only.
- Changing the matrix, its `fail-fast` behavior, the website change
  detection, or the `web` job's condition. A representative leg cancelled
  by another leg's failure shows as missing, which is truthful.
- Retained Cucumber proof under `website/frontend/proof`, which stays as
  it is.

Boundary assumptions:

- The developer's `gh` can read this repository's Actions artifacts
  (observed 2026-11-05: `gh` is signed in as `terryyin` and lists
  `python-app.yml` runs of `terryyin/lizard`).
- The installed dashboard reads `ci-proof-summary-attempt-<n>` artifacts
  (its server has a CI proof artifact reader); that it renders Lizard's
  summary is observed only on the real run.

#### Key examples

1. **Website covered, all passing.** A push that changes
   `.github/workflows/python-app.yml` (a website change path) runs `build`,
   `changes` and `web`. → The run publishes one summary with `test` (part
   1 of 1, about the analyzer suite's size, not 14 times it) and
   `website/frontend/features` (part 1 of 1, one count per executed
   scenario), and the CI status tab shows both entries' totals for that
   run.
2. **Website skipped.** A push changing only `lizard_languages/` runs
   `build`; `web` is skipped. → The collector still runs, the summary shows
   `test` counts and `website/frontend/features` as missing, and the tab
   shows the website proof as absent rather than as zero tests.
3. **A failing analyzer test on the representative leg.** A pytest test
   fails on `ubuntu-24.04` / `3.13`. → That leg and the run fail, and the
   `test` entry shows the failure in its failed count.
4. **A failure only on another leg.** A test fails only on
   `windows-latest` / `3.8`. → The run's verdict is failure; the `test`
   counts come from the representative leg and may all be passed, or show
   as missing if fail-fast cancelled that leg.
5. **Rerun.** A failed run is re-run. → The collector reads only the new
   attempt's fragments and publishes `ci-proof-summary-attempt-2`.

#### Refinement notes

- **Directions compared.** (a) The recipe's fragments and collector with
  one representative leg — recommended: it reuses maintained tooling, adds
  no test execution and matches the developer's constraint. (b) Every
  matrix leg as a part of `test` — rejected by the developer because the
  totals would inflate. (c) A separate proof-only job rerunning pytest —
  duplicates the suite's cost and reports a run other than the verdict's.
- **Borrowed mechanism.** A calibration lab records the reading of one
  designated reference instrument while every instrument only has to pass
  its tolerance check. The representative leg is the reference reading and
  the matrix verdict is the tolerance check. The analogy's limit is the
  same as scope limit 1: the reference reading says nothing about drift on
  the other instruments, so the verdict, not the counts, is the cross-leg
  signal.
- **Strongest doubt.** The recipe's website example assumes Playwright's
  test runner, but Lizard's browser tests run through cucumber-js driving
  Playwright. The website fragment therefore comes from a Cucumber report
  converted to the recipe's outcomes format, with paths made
  repository-relative (`website/frontend/features/...`) so they fall under
  the configured entry. The purpose still holds.
- **Architecture.** No Accepted ADR governs CI (ADR 0000 adopts ADRs, ADR
  0001 is the domain glossary); nothing conflicts. The added
  responsibility is CI-only: vendored collector modules, a collector job,
  and report conversions. Product code is untouched.
