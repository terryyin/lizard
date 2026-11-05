# Publish Lizard CI proof for Intent

**Identity:** SEED-035#publish-lizard-ci-proof
**Source:** [canonical story](../../seeds/SEED-035-publish-lizard-ci-proof.md#publish-lizard-ci-proof)
**Source review:** [source-review.md](source-review.md)

## Goal and scope

A Lizard developer sees, in Intent's **CI status** tab for a shown Python
application run, the passed / failed / other counts of the analyzer tests
(`test`) and the website scenarios (`website/frontend/features`), and sees
the website part as missing when the `web` job did not run.

Follow the [installed CI proof recipe](../../../.agents/skills/dough-test-optimization/references/ci-proof-summary.md)
in [`python-app.yml`](../../../.github/workflows/python-app.yml), the
judged workflow (`ciWorkflow` in `.planning/open-dough.json`).

**Excluded:** counts from non-representative matrix legs; committed pytest
proof for `test`; changes to the matrix, `fail-fast`, website change
detection or the `web` job's condition; the retained Cucumber proof.

**Assumptions:** `.planning/open-dough.json` sets no `ciAdapter`, so the
recipe (Open Dough 0.3.86) judges `python-app.yml` runs and its GitHub
Actions steps 3 to 6 apply; its CI-adapter path (attempt-named summaries
served by an adapter's `proof` read) does not. The developer's `gh` reads
this repository's Actions.

## Outside-in proof

| Promise (seed key example) | Owner | Observable signal |
| --- | --- | --- |
| 1 — website-covered run publishes both entries; tab shows totals | Slice 2 (`test` part from slice 1) | `gh api` lists `ci-proof-summary-attempt-1` on a real run whose `web` job ran; its JSON has `test` 1/1 and `website/frontend/features` 1/1 with counts; the tab shows both for that run |
| 2 — website skipped → website proof missing | Slice 2 | A real run with `web` skipped publishes a summary showing `website/frontend/features` part 1 of 1 missing; the tab shows it as missing |
| 3 — failure on the representative leg is counted | Slice 1 (conversion), already observed locally | Local replay below: pytest JUnit report → outcomes → fragment counts `failed`; the `!cancelled()` fragment step runs after a failed test step |
| 4 — failure only on another leg | Natural (verdict) | No added proof: the verdict is the existing workflow result |
| 5 — rerun reads only its own attempt | Recipe (attempt-named artifacts) | Artifact names carry `run_attempt`; no extra observation |
| `test` counts are one leg, not 14 | Slice 1 | Summary `test` total ≈ 3,025 checks (local suite: 3,018 passed, 7 other), not ~42,000 |
| Existing workflow structure kept | Slices 1–2 | `test/test_web_changes.py` and `test/testPythonVersionClaims.py` pass; full `nix develop -c make` |

Local gate: `AGENTS.md` requires the full suite (`nix develop -c python -m
pytest`) and `nix develop -c make` before completing a change. The real CI
run is the promised boundary; local replays settle the conversions only.

## Slices

### 1. The analyzer suite's counts reach Intent
Type: Behavior
Status: done
Proof: real `python-app.yml` run on the story branch publishes
`ci-proof-summary-attempt-<n>` with `test` part 1 of 1 and suite-sized
counts; Intent's CI status tab shows them for that run; full local suite
and `make` pass.

Accepted proof (2026-11-05):
Local: focused workflow tests 14 passed; full suite 3021/7; `make` exit 0;
junit→fragment→summarize suite-sized. Real run 42016250359 on
`cursor/publish-lizard-ci-proof-for-intent` at `c7643ce` published
`ci-proof-summary-attempt-1` with test 3021 passed / 7 other, part 1/1;
Intent CI status tab (story selected) shows the same counts.

Behavior: a push to the story branch → the `ubuntu-24.04` / `3.13` `build`
leg writes the `test` fragment from its pytest JUnit report, the collector
publishes the summary → the tab shows `test` counts.

Work:

- `.planning/open-dough.json`: add `{ "path": "test", "kind": "python",
  "include": ["**/test*.py"] }` to `intent.locations`, leaving the Gherkin
  location byte-for-byte as it is. Order: `test` first, then
  `website/frontend/features` (the `--expect` order follows it).
- Copy the four collector modules unchanged from
  `.agents/skills/dough-test-optimization/scripts/` (0.3.86, which names a
  GitHub Actions attempt by `--attempt` and `--run-id`) into
  `scripts/ci-proof/`,
  and add the recipe's `junit-outcomes.py` there.
- `python-app.yml`: workflow-level `env.CI_PROOF_REVISION`; give the `build`
  test step an `id`; run pytest with `-o junit_family=xunit1
  --junitxml=...` on every leg (cheap, keeps the command uniform) and add
  `actions/setup-node@v4` (Node 24, as the `web` job) plus the fragment and
  keep steps only on the representative leg, under
  `if: ${{ !cancelled() && steps.<id>.outcome != 'skipped' && matrix.os ==
  'ubuntu-24.04' && matrix.python-version == '3.13' }}` with a comment naming
  it the representative leg; artifact name
  `ci-proof-fragments-build-1-attempt-${{ github.run_attempt }}`.
- Add the `ci-proof` collector job: `needs: [build]`, `if: ${{ !cancelled()
  }}`, `--expect test=1`, summary retention 30 days.
- Observe: push, wait for the run, list and download the summary with
  `gh api`, and look at the tab (browser automation on the developer's
  dashboard if available, otherwise ask the developer to look).

Interim behavior (accepted, replaced by slice 2): the summary declares no
website entry, so the website location shows no CI proof for these runs.

### 2. Website scenario counts, or their absence, reach Intent
Type: Behavior
Status: planned
Proof: one real run with `web` run → summary has
`website/frontend/features` part 1 of 1 with 16-scenario-sized counts; one
real run with `web` skipped (for example a push touching only `.planning/`)
→ that part shows missing; the tab shows both states; full local suite and
`make` pass.

Accepted proof in progress (2026-11-05):
Local gates passed. Web-covered run 42017933530 at `15dc13b` summary has
`test` 3021/7 and `website/frontend/features` 16 passed part 1/1; Intent tab
shows both. Web-skipped run still owed (this plan-only push).

Behavior: a push whose changes reach the website boundary → the `web` job
writes the website fragment from its Cucumber JSON report → the tab shows
the scenario counts; a push outside it → the collector still runs and the
website part shows missing.

Work:

- Add `scripts/ci-proof/cucumber-outcomes.mjs`: read Cucumber's JSON report
  and print one `<status>\t<base>/<uri>` line per `type: scenario` element;
  `failed` when any before hook, step or after hook failed, `passed` when
  all passed, otherwise `other`; `<base>` is `website/frontend`.
- `python-app.yml` `web` job: give the browser test step an `id` and run
  `npm --prefix website/frontend run test:e2e -- --format
  json:reports/cucumber.json`; add the fragment and keep steps (part 1 of 1,
  artifact `ci-proof-fragments-web-1-attempt-...`).
- Collector: `needs: [build, web]`, `--expect test=1 --expect
  website/frontend/features=1`.
- Observe both runs and the tab as in slice 1.

## Current decisions

- Representative leg: `ubuntu-24.04` / Python `3.13`, the interpreter the
  `web` job already uses; the collector and fragment steps use Node 24 as
  the `web` job does (recipe needs ≥ 20.12).
- CI scripts live in `scripts/ci-proof/`; the workflow runs those copies.
- Keep exactly `needs: changes` and `if: needs.changes.outputs.web ==
  'true'` on `web`, no `needs:` on `build`, and no new YAML list line of
  the form `- "3.x"` anywhere in the workflow: `test/test_web_changes.py`
  and `test/testPythonVersionClaims.py` read the workflow with those
  regexes. The collector job is named `ci-proof` (hyphen), which the job
  regex in `test_web_changes.py` does not capture and which ends the `web`
  job's captured body correctly.
- Execution mode: Story Branch Mode; every push to any branch runs
  `python-app.yml`, so story-branch pushes give the real runs.

## Decisive premises

| Premise | Consuming operation | Observation (form / profile) | When / revision | Result |
| --- | --- | --- | --- | --- |
| pytest's xunit1 JUnit report names every test's repository-relative file | `python -m pytest -o junit_family=xunit1 --junitxml=F` then `junit-outcomes.py F` | Full suite in `nix develop` (Python 3.9.11, pytest 8.4.2) in this workspace. Proxy for CI's 3.13 + latest pytest: the `file` attribute is pytest's xunit1 writer, the same in pytest 8 on any interpreter | 2026-11-05, `1503696` | 3,025 testcases, 0 without `file`, all under `test/`; 3,018 passed, 7 other |
| Cucumber can add a JSON report without changing existing output | `npm --prefix website/frontend run test:e2e -- --format json:reports/cucumber.json` | Real local run (build + browser) in `nix develop`, Node 24.21 | 2026-11-05, `1503696` | progress output and `cucumber.html` unchanged; JSON has 16 scenarios with `features/<name>.feature` URIs, all steps `passed` |
| Cucumber JSON distinguishes failed and other scenarios | the conversion above | Scratch failing and undefined scenarios in this workspace, reverted | 2026-11-05 | step statuses `failed` and `undefined` → `failed` and `other` |
| The recipe's fragment/summarize produce the promised counts and missing part | `ci-proof-summary.mjs fragment` / `summarize` with this plan's entries | Installed 0.3.84 scripts on the two real local reports; installed 0.3.86 scripts (the ones slice 1 copies) replayed `fragment --attempt 1` and `summarize --run-id 5 --attempt 1` with both `--expect` entries on a scratch outcomes file | 2026-11-05; 0.3.86 at `563b534` | 0.3.84: `test: 3018 passed, 0 failed, 7 other`; `website/frontend/features: 16 passed`; without the web fragment: `part 1 of 1 missing`. 0.3.86: same flags accepted, summary keeps `runId`/`runAttempt`, passed/failed/other counted and the missing website part reported |
| The `test` Intent location lists pytest files | dashboard `python` kind with `include` | Read of `dashboard/src/intentLocationSelection.ts` (`python: .py`) in `~/git/open-dough` `d503b1dc8` | 2026-11-05 | Presence only; observed in the tab during slice 1 |
| The real run publishes, `gh` reads, and the tab renders the summary | GitHub Actions run of `python-app.yml`; `gh api .../artifacts`; dashboard CI status tab | `gh auth status` (signed in as `terryyin`); `gh api .../workflows/python-app.yml/runs` lists runs | 2026-11-05 | Route reachable; publishing and rendering are the slices' own proofs (state-changing push), so slice 1 is the early probe: if the summary does not appear or the tab does not render it, stop and replan before slice 2 |

## Concern evidence

Slice-plan refinement: not needed — two Behavior slices, each one entry's
end-to-end outcome with its own real-run proof; no fragmented or combined
concerns.

Remaining slice-specific concerns: none identified in this review. The
real-run and dashboard rendering premise is bounded by slice 1 as the
early probe.


## Story obligations

### X1-excl. Non-representative leg counts excluded
Reported: slice 1 — "source review E4/X1 — failure only on another leg shows in verdict, not necessarily in test counts"
Story clause: "Counts from non-representative legs. A failure that happens only on another operating system or Python version shows in the run's verdict, not in the `test` counts."
Disposition: excluded "Counts from non-representative legs. A failure that happens only on another operating system or Python version shows in the run's verdict, not in the `test` counts."
Commitment: E4, X1


### X2-excl. Committed pytest proof excluded
Reported: slice 1 — "source review X2 — committed pytest proof for test not undertaken"
Story clause: "Committed pytest proof for the `test` location (a `proof` directory and proof generation); this story publishes CI observations only."
Disposition: excluded "Committed pytest proof for the `test` location (a `proof` directory and proof generation); this story publishes CI observations only."
Commitment: X2

### X3-excl. Matrix and web condition unchanged
Reported: slice 1 — "source review X3 — matrix/fail-fast/web detection/condition not changed"
Story clause: "Changing the matrix, its `fail-fast` behavior, the website change detection, or the `web` job's condition."
Disposition: excluded "Changing the matrix, its `fail-fast` behavior, the website change detection, or the `web` job's condition."
Commitment: X3

### X4-excl. Retained Cucumber proof unchanged
Reported: slice 1 — "source review X4 — website/frontend/proof stays as it is"
Story clause: "Retained Cucumber proof under `website/frontend/proof`, which stays as it is."
Disposition: excluded "Retained Cucumber proof under `website/frontend/proof`, which stays as it is."
Commitment: X4

### X5-excl. CI adapter path out of scope
Reported: slice 1 — "source review X5 — adapter-attempt proof does not apply"
Story clause: "Lizard's CI is GitHub Actions with no `ciAdapter`. Open Dough 0.3.86 also lets a project read through a CI adapter show proof for an adapter's attempt; that path does not apply here and changes nothing in this story's outcome."
Disposition: excluded "that path does not apply here and changes nothing in this story's outcome."
Commitment: X5

### G1. Website entry absent from collector expect (interim)
Reported: slice 1 — "the summary declares no website entry, so the website location shows no CI proof for these runs."
Story clause: "sees, beside the run's verdict, how many analyzer tests (`test`) and website scenarios (`website/frontend/features`) that run passed, failed or did something else, and sees plainly when the website part was not run."
Disposition: interim until slices 2
