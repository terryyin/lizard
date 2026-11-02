# Ordinary website Cucumber scenario proof

**Identity:** SEED-025#cucumber-scenario-proof
**Source:** [selected story](../../seeds/SEED-025-cucumber-scenario-proof.md#cucumber-scenario-proof)
**Source review:** [independent source-only commitments](source-review.md)

## Goal and scope

Ordinary focused, tagged, parallel, retried and full website Cucumber runs
publish compact version-1 observations through a silent project-owned plugin.
Follow the installed 0.3.81 `cucumber-proof.md`; preserve output, HTML reports,
diagnostics and exit meaning. Configure the declaring Gherkin location, commit
proof from meaningful ordinary runs, and land the validated result to master.
Do not alter managed Open Dough guidance or the active parser story.

## Context and decisions

- Originating/default checkout: /Users/terryyin/git/lizard, master.
- Created owned checkout: /private/tmp/lizard-cucumber-proof,
  codex/cucumber-scenario-proof; starting revision 9c7874249b672310da711d19d9e59bb61012c060.
- Story Branch Mode; claim ac8348a1dbc29366349072c918325d58c2ade4c5 is accepted
  on origin/master; agent Hibiki-chan. Later execution increments use the
  execution branch; the authorized final Dough Land target is origin/master.
- Locked `npm ci` and ordinary `test:e2e:dry` succeed in this checkout
  (16 scenarios, 134 steps skipped). Cucumber 13.3.0 exposes native plugins
  with main-process message events. No existing project proof recorder exists:
  searches across website/scripts/test find only unrelated deployment JSON.
  Reuse Cucumber's existing Gherkin/message protocol and the current config,
  preserving progress and HTML formatters; add one local recorder.
- ADR 0000 (docs/adrs/0000-use-adrs-accepted.md) retains human decision ownership;
  ADR 0001 (docs/adrs/0001-ubiquitous-language-accepted.md) retains metric/domain
  meanings. This reversible local test integration changes neither.
- No new North Star or architecture decision is warranted. No numeric slice
  limit is supplied: one coherent behavior and runner-level proof loop.
- Follow [Open Dough's failing and flaky test policy]
  (/Users/terryyin/git/open-dough/AGENTS.md#failing-and-flaky-tests) as written
  into all further handoffs, with AGENTS.md. No base-failure exemption, retry
  until green, or duplicate repair of an exact active owner is permitted.
- Required local full proof comes from AGENTS.md and the handoff: full pytest,
  `make`, ordinary full Cucumber, and a meaningful focused Cucumber run. They
  validate implementation; do not run a suite solely to collect proof. Avoid
  simultaneous heavy runs on this machine; run the final full checks once
  after focused proof settles, unless edits/failures invalidate them.

## Outside-in proof

Drive the real Cucumber runner against isolated temporary Git repositories to
observe plain/outline identities (including escaped table cells), all outcomes
and hooks, retry finality and cumulative time, dry-run/fail-fast exclusion,
declaring-location boundaries, tested feature source hashes, start-commit
capture, atomic incremental files, interruption/concurrency and silent failures.
Assert progress/HTML output, diagnostics and exit status with and without the
plugin. Real website focused and full runs exercise the ordinary configuration
from its frontend working directory and generate the committed proof.

## Ordered slices

### 1. Record ordinary website scenario observations
Type: Behavior
Status: planned
Proof: real-runner regression tests, focused and full browser Cucumber, full
pytest and make, then exact applicable hosted Python application CI.

Behavior: Given the existing website suite and declared Gherkin location, an
ordinary run records only executed final scenario attempts in unique atomic
per-run files, without changing normal reports, diagnostics or status. Outline
identity comes from parsed source; metadata captures the run-start commit and
actual tested feature bytes. Partial/concurrent runs preserve completed work.
Recording is silent and best effort; dry-run writes nothing.

## Concern review

Refinement was unnecessary: one cohesive behavior with one runner-level proof
loop, no independent outcomes or unsettled product decision. No remaining
slice-specific concerns; the installed runner API and command readiness were
observed in this checkout. Implementation details remain with the slice.

## Learnings

- Real Cucumber dry-runs can emit undefined/ambiguous results, not only skipped
  results. Status alone is insufficient execution evidence. Invoked step/hook
  stopwatch duration distinguishes actual code (including returning skipped)
  from CLI/config/API dry-runs and fail-fast nonexecution; real-runner tests
  discriminate these paths.
- Hash the source message with Git's --stdin --path normalization: it preserves
  the loaded version even when disk/HEAD change during execution and agrees
  with hash-object file under CRLF/attributes.

## Accepted local proof

- `nix develop -c npm --prefix website/frontend run test:proof`: 11 passed
  (3.355s). Actual temporary Git/Cucumber runs and assertions in
  website/frontend/test/proof-plugin.test.js cover all recorder boundaries.
  Consumer trace: /private/tmp/lizard-cucumber-proof-consumer-trace.md, all 122
  dispositions complete; product consumers use ordinary Cucumber config.
- `nix develop -c npm --prefix website/frontend run build`: passed.
- `nix develop -c npm --prefix website/frontend run test:e2e -- features/languages.feature`:
  full actual configured selection, 16 scenarios/134 steps and 2 hooks passed
  in 25.102s. Cucumber 13 merges positional and configured paths and prints
  its normal diagnostic; selection was verified, not inferred from the name.
- `nix develop -c npm --prefix website/frontend run test:e2e -- --name 'Changing the language keeps the entered code until an example is loaded'`:
  1 scenario/10 steps and 2 hooks passed, 0.569s. Existing progress and HTML
  reports observed. Both runs produced real proof, retained with this change.
- `nix develop -c npm --prefix website/frontend run test:e2e:dry`: 16 scenarios,
  134 steps and 2 hooks skipped; proof filenames and bytes unchanged.
- Actual Open Dough dashboard parseProofFile/latestObservations accepted both
  real proof files: 17 observations, 16 latest identities, all passed. Native
  Node24 imported /Users/terryyin/git/open-dough/dashboard/src/cucumberProof.ts;
  no dashboard files were changed.
- `nix develop -c python -m pytest`: 2427 passed, 7 skipped, 13.61s.
- `nix develop -c make`: exit 0, full coverage tests and style checks passed.
- `git diff --check`: passed. Independent refactor extracted the temporary runner fixture into
  website/frontend/test/cucumber-runner.js (82 lines), leaving the same
  assertions in proof-plugin.test.js (194 lines). `test:proof` reran with
  11 passed in 3.342s; ## REFACTOR COMPLETE received. Plugin/config/browser
  and Python proof boundaries are unchanged, so full accepted proof is reused.
  Refactor trace: /private/tmp/lizard-cucumber-proof-refactor-consumer-trace.md.
- Local implementation and review are settled. Progress publication retains
  slice 1 as planned until its source's committed-proof, authorized trunk
  landing and actual hosted-CI/report obligations can be observed. No product
  implementation is deferred; the same workspace owns final delivery.
- Branch observer: coordinator codex-lizard-cucumber-20261102,
  /tmp/dough-ci-501/watch-5ZaSQr, PID 60196, yielded cell g12:22,
  session 89191, checkout /private/tmp/lizard-cucumber-proof. It observes
  codex/cucumber-scenario-proof, Python application / python-app.yml. Claim
  and preparation on trunk were unobserved by this branch-specific stream.

## Delivery and review

- Implementation ad19779ab6de44ec0bc776ec951d85b768f65f94 is accepted on
  origin/codex/cucumber-scenario-proof (suffix base ac8348a1dbc29366349072c918325d58c2ade4c5).
  Exact Python application CI succeeded: run 41365742887, attempt 1.
- Automatic retrospective completed: product advice no change, no correction
  plan or architecture conflict. Its supported process occurrence updates
  existing DD-013 in DearDough.md; no implementation/proof change.
- Explicit Dough Land retained increment publishes the reviewed checkout to
  origin/master, preserving published history. Workspace and claim remain
  until final trunk CI, source completion and ordinary story wrap-up settle.
