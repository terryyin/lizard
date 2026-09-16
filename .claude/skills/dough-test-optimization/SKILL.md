---
name: dough-test-optimization
description: >-
  Speeds up local test feedback through profiling and measured experiments
  across related tests. Use to optimize slow tests or test-suite performance.
  Preserves behavioral coverage and confidence while pursuing fewer redundant
  cases, less code, and cleaner design. With --resolve, triages recorded
  candidates without profiling or optimizing.
---

# Speed up local test feedback

Reduce the time a developer waits for trustworthy local test results. Pursue
substantial savings through fewer redundant cases, less setup and support code,
and cleaner design together. These are ways to improve feedback time, not
substitutes for a measured speedup. Preserve behavioral coverage and confidence.

## Establish context and mode

Use the task's project. Resolve the selected local test command and scope,
test locations, runtime/service setup, applicable test and architectural rules,
and plan conventions from that project. Name genuinely missing context and
stop only its dependent step; do not invent paths, commands, or a test taxonomy.

Default mode profiles, plans, experiments, and re-profiles. A profile-only request
ends with findings. For `--resolve`, follow only
[candidate resolution](references/resolving-candidates.md).

Before assessing tests, read the shared
[behavioral test guidance](../dough-post-change-refactor/references/refactor-checks.md#tests-as-behavioral-documentation).
Apply it across the authorized optimization scope, not just the current diff.

## Establish the baseline

Run the selected local scope. Record revision, literal command, runner mode,
workers, filters, relevant environment/cache conditions, wall time, executed
case count, and available test/setup timings. Keep raw profiles out of commits.
Use only approved profile exclusions; candidate proposals remain eligible and
normal verification coverage stays intact. Report the limits of filtered runs.

Measure the ordinary local test mode even if profiling needs a different mode.
A failed or incomplete run supplies diagnostics, not a successful baseline.
Do not blindly trust timings: startup, caches, and machine load can dominate.
Compare like conditions; repeat doubtful comparisons or report them inconclusive.
Summed test durations are neither wall time nor measured CPU time.

## Investigate a useful test family

Use slow tests and aggregate setup costs to seed a hypothesis about wasted time.
Prioritize work that shortens the selected local feedback path. Look across
files and test layers for related behavior, variants, fixtures, and consumers,
including fast siblings; neither a percentile nor a file limits investigation.
Keep edits within authorized scope.

Group by a behavioral responsibility or evidenced shared cost, never by timing
rank or fixed batch size. Assign shared helper changes once with their consumers
and dependencies; do not merge unrelated behavior scenarios or count savings twice.

Stop expanding discovery when further inspection no longer changes the proposed
experiment or its proof obligations. Reopen it when results expose a missing
cause or variant. Carry the family analysis into smaller execution slices so
local edits retain the broader opportunities and surviving proof.

## Record the optimization plan

Use one plan in the project's established location and the shared
[executable plan format](../dough-story-refinement/references/planning.md#write-an-executable-plan).
Record the request and profiling evidence, developer outcome, scope, preserved
promises, baseline, family analysis, hypotheses, and proof. For each experiment,
name the expected saving, smallest meaningful change, surviving proof for any
removed cases, and focused verification/timing command. Include final re-profiling.

In resolve-only mode, reuse recorded evidence and plan any missing baseline as a
prerequisite for later execution; do not run it now.

## Experiment, learn, and reassess

Execute through [dough-execute-plan](../dough-execute-plan/SKILL.md), keeping its
implementation/refactor ownership, delivery, and normal retrospective. Make the
following loop explicit in the plan and each experiment slice:

1. **Hypothesize:** read [optimization tactics](references/optimization-tactics.md)
   and choose a promising way to remove cost across the family. Challenge the
   current test design; do not stop at the first convenient micro-fix.
2. **Try and measure:** capture a comparable focused baseline, make the smallest
   meaningful change, verify behavioral proof, and measure again before delivery.
3. **Decide:** retain a supported improvement; revise or undo this experiment's
   ineffective/regressing edits while preserving others' work. Record the
   finding rather than deliver a change merely because its tests pass.
4. **Reassess:** use the result to reconsider the family's bottleneck and strategy.
   When a time target is supplied, compare the remaining gap with realistically
   removable cost. If the remaining experiments cannot plausibly close it,
   change the strategy before continuing. Update the same plan.

Stop experimenting when no credible further saving remains within scope; record
what was tried and the remaining cost as a candidate. An unresolved product,
architectural, or confidence trade-off stops that path for a human decision.

## Verify and close

Establish replacement proof before removing or narrowing its predecessor. Verify
the changed family in normal test mode, widening to affected consumers for shared
changes. Fix flakiness at its cause; do not mask it with retries, sleeps, or skips.
For changed asynchronous/E2E synchronization, require three consecutive focused
passes or stronger project-required evidence; repetition alone proves no fix.
Commit no focus/only markers.

Re-run the selected local command under comparable conditions after the
experiments. Include replacement tests' cost when proof moved across boundaries.
If the local run did not improve, revisit the cost hypothesis before declaring
success; a faster isolated test is insufficient. A failed re-profile leaves
measurement incomplete; focused timings or CI cannot replace it.

Report local wall time and executed cases before/after, retained behavioral proof,
test/support-code simplifications, experiments rejected, delivered commits, and
remaining candidates. Distinguish measured improvement, no demonstrated gain,
and incomplete work. Do not claim speedup from fewer cases or lines alone.

Keep candidates with locations, cost, unique protection, attempted alternatives,
evidence, date, and needed decisions in the existing record or active plan.
Retain that plan and evidence for retrospective and
[story wrap-up](../dough-story-wrap-up/SKILL.md); unresolved candidates must be
resolved or transferred to the established follow-up location before cleanup.
Emit `## TEST OPTIMIZATION COMPLETE` only when the pass's planned work,
verification, measurement, and delivery are complete, stating its actual outcome.
