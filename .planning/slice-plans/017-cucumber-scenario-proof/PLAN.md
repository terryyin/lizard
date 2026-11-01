# Ordinary website Cucumber scenario proof

**Identity:** SEED-025#cucumber-scenario-proof
**Source:** [selected story](../../seeds/SEED-025-cucumber-scenario-proof.md#cucumber-scenario-proof)

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

None yet.
