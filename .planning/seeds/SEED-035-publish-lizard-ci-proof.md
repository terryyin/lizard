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
