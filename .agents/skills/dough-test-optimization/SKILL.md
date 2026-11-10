---
name: dough-test-optimization
description: >-
  Speeds up local test feedback, starting from the CI test proof this project
  already retains: ranks removable costs, states the saving worth keeping, and
  reaches a measured keep or an evidenced stop through bounded experiments
  across related tests. Use to optimize slow tests or test-suite performance,
  or to profile tests on explicit request. Preserves behavioral coverage and
  confidence while pursuing fewer redundant cases, less code, and cleaner
  design. With --resolve, triages recorded candidates without profiling or
  optimizing. Also makes this project's CI, GitHub Actions or read through a
  CI adapter, publish the CI proof summary that Intent's CI status tab and
  this skill read.
---

# Speed up local test feedback

Reduce the time a developer waits for trustworthy local test results. Pursue
substantial savings through fewer redundant cases, less setup and support code,
and cleaner design together. These are ways to improve feedback time, not
substitutes for a measured speedup. Preserve behavioral coverage and confidence.

## Establish context and mode

Use the task's project. Resolve the requested scope, its test locations and
local test commands, this project's CI (`ciWorkflow` or `ciAdapter` in
`.planning/open-dough.json`), runtime/service setup, applicable test and
architectural rules, and plan conventions from that project. Name genuinely
missing context and stop only its dependent step; do not invent paths,
commands, or a test taxonomy.

A request to make this project's CI publish proof for Intent's **CI status**
tab or for this skill, or a tab saying the shown run or attempt has no CI
proof summary, follows only the [CI proof summary](references/ci-proof-summary.md)
recipe: it edits this project's GitHub Actions workflow, or its CI job and CI
adapter, and profiles nothing. The summary keeps each location's test cases
and test files apart, each with its own outcomes, timing and gaps, in the
units this project's reports give.

An ordinary optimization request starts from the evidence this project already
retains and runs no fresh profiling run;
[profile afresh](references/evidence-and-leads.md#profile-afresh-only-on-request)
only when the developer explicitly asks. A request for findings only ends with
discovery's report, as a [stop](#decide-stop-experiment-or-plan) does. For
`--resolve`, follow only
[candidate resolution](references/resolving-candidates.md).

An independently requested pass is admitted at its first edit, as
[admit accepted work](../dough-execute-plan/references/admit-accepted-work.md#decide-whether-a-mission-was-accepted)
says, with its story in a relevant existing seed when one fits. A `--resolve`
pass is admitted before its triage. A pass explicitly selected as
[one-shot work](../dough-execute-plan/references/one-shot.md) follows that
instead. A pass requested as a step of an active story continues under that
story. An admitted experiment or resolve-only triage that ends with no
worthwhile change
[finishes the mission](../dough-execute-plan/references/admit-accepted-work.md#finish-the-mission),
or under one-shot its
[no-change finish](../dough-execute-plan/references/one-shot.md#finish-with-no-change).

Before assessing tests, read the shared
[behavioral test guidance](../dough-post-change-refactor/references/refactor-checks.md#tests-as-behavioral-documentation).
Apply it across the authorized optimization scope, not just the current diff.

## Establish the baseline

Read this project's
[candidate record](references/candidate-record.md) when it exists. Follow that
reference for live candidates, for
[accepted costs beside their tests](references/candidate-record.md#keep-accepted-cost-beside-its-test)
and the existing tags and markers, and for later-pass preservation.

Then discover from retained evidence through
[evidence and leads](references/evidence-and-leads.md): read the requested
scope's CI proof through the lead reader, name its attempt, selection, and
gaps, extract any runner output a lead needs, and state the smallest saving
worth keeping. Discovery edits nothing, the candidate record included.

## Investigate a useful test family

Rank leads by [removable cost](references/evidence-and-leads.md#rank-removable-causes),
and use them to seed a hypothesis about wasted time. Prioritize work that
shortens the requested local feedback path. Look across files and test layers
for related behavior, variants, fixtures, and consumers, including fast
siblings; neither a percentile nor a file limits investigation. Keep edits
within authorized scope.

Group by a behavioral responsibility or evidenced shared cost, never by timing
rank or fixed batch size. Assign shared helper changes once with their consumers
and dependencies; do not merge unrelated behavior scenarios or count savings twice.

Stop expanding discovery when further inspection no longer changes the proposed
experiment or its proof obligations. Reopen it when results expose a missing
cause or variant. Carry the family analysis into the experiment, or into a
plan's slices, so local edits retain the broader opportunities and surviving
proof.

## Decide: stop, experiment, or plan

- **Stop** when the remaining credible opportunities fall below the stated
  saving, cannot justify the effort, or cannot plausibly reach the agreed
  target: the developer's target, otherwise the stated saving. A stop at
  discovery reports why, with its evidence: the leads, the stated saving, and
  the gaps.
- **Experiment** on the most promising avoidable cost with the smallest
  meaningful change. Admit the pass at that first edit and run it as one
  bounded planless execution under the ordinary request's authority, through
  [experiment, learn, and reassess](#experiment-learn-and-reassess).
- **Plan** when the evidence calls for dependent experiments, a shared harness
  redesign, or a consequential change in where behavior is proved: admit the
  pass and [record the optimization plan](#record-the-optimization-plan).

Bring the developer only consequential questions. An unresolved product,
architectural, or confidence trade-off stops that path for a human decision.

## Record the optimization plan

Use one plan in the project's established location and the shared
[executable plan format](../dough-story-refinement/references/planning.md#write-an-executable-plan).
Record the request and its evidence (the attempt read, its selection, and its
gaps), developer outcome, scope, preserved promises, the stated saving worth
keeping, family analysis, hypotheses, and proof. For each experiment, name the
expected saving, smallest meaningful change, surviving proof for any removed
cases, and focused verification/timing command.

In resolve-only mode, reuse recorded and retained evidence and plan any
missing focused baseline as a prerequisite for later execution; do not run it
now.

For an admitted pass, this plan attaches to the same story: record and publish
its preparation and continue that claim as
[continue into implementation](../dough-execute-plan/references/admit-accepted-work.md#continue-into-implementation)
describes. This changes neither this skill's measurement rules nor whether a
created plan continues into execution.

## Experiment, learn, and reassess

Execute through [dough-execute-plan](../dough-execute-plan/SKILL.md): one
bounded experiment as one planless contextual instruction, or a planned pass's
experiment slices, keeping its implementation/refactor ownership, delivery,
and normal retrospective. That delivery is this project's existing delivery,
published through
[increment and repair publication](../dough-execute-plan/references/trunk-publication.md#publish-an-execution-increment-or-repair).
A planless instruction carries the lead and its evidence, the stated saving
and its measure, the protection to keep, and the focused timing command. Make
the following loop explicit in that instruction, or in the plan and each
experiment slice:

1. **Hypothesize:** read [optimization tactics](references/optimization-tactics.md)
   and choose a promising way to remove cost across the family. Challenge the
   current test design; do not stop at the first convenient micro-fix.
2. **Try and measure:** time the chosen lead's family before and after the
   smallest meaningful change under
   [comparable conditions](references/evidence-and-leads.md#measure-like-for-like),
   and verify behavioral proof before delivery.
3. **Decide:** retain a supported improvement; revise or undo this experiment's
   ineffective/regressing edits while preserving others' work. Record the
   finding rather than deliver a change merely because its tests pass.
4. **Reassess:** use the result to reconsider the family's bottleneck and
   strategy. After a kept change, rank again, using the delivered
   revision's CI proof once normal delivery has produced it. Take an
   independent next lead the same way, plan experiments that depend on the
   kept change, or [stop](#decide-stop-experiment-or-plan). When a time
   target is supplied, compare the remaining gap with realistically
   removable cost. A checkpoint whose recorded remaining experiments still offer a
   plausible route to the target continues to the next experiment without an extra
   record; do not invent a stop merely because a target exists.
   A decisive checkpoint — one whose recorded remaining experiments cannot
   plausibly close the remaining gap — is an explicit decision obligation: before
   another dependent experiment is dispatched, record in the plan's Current
   decisions (the [executable plan
   format](../dough-story-refinement/references/planning.md#write-an-executable-plan))
   the checkpoint's measurement, the remaining-gap comparison, the invalidated
   strategy assumption, the consequence for the remaining experiments, and the
   selected authorized decision — a supported different strategy within existing
   authority, explicitly retained independently valuable work, or a stop for the
   developer when continuing would change the target, outcome, or scope. Update
   the same plan and keep the obligation mapped to its owning slice under [own
   executable proof](../dough-story-refinement/references/planning.md#own-executable-proof),
   so [execution
   decisions](../dough-execute-plan/references/execution-decisions.md#reassess-before-extending-work)
   must honor it before selecting another dependent experiment slice.

## Verify and close

Establish replacement proof before removing or narrowing its predecessor. Run
this project's ordinary verification for the affected behavior, widening to
affected consumers for shared changes, and reuse comparable proof from CI
produced by normal delivery. Follow this project's failing and flaky test
policy: fix flakiness at its cause; do not mask it with retries, sleeps, or
skips. For changed asynchronous/E2E synchronization, require three consecutive
focused passes or stronger project-required evidence; repetition alone proves
no fix. Commit no focus/only markers.

Report each measured gain in its
[measure and scope](references/evidence-and-leads.md#measure-like-for-like),
the protection retained, and any unresolved measurement. Also report
test/support-code simplifications, experiments rejected, delivered commits,
and remaining candidates. Distinguish measured improvement, no demonstrated
gain, and incomplete work. Do not claim speedup from fewer cases or lines
alone.

An admitted pass writes the candidates that remain live, with what was tried,
to the [candidate record](references/candidate-record.md), and puts each cost
it accepts [beside its test](references/candidate-record.md#keep-accepted-cost-beside-its-test),
following that reference for what stays live and for creation, conversion,
preservation, and cleanup.
Retain a planned pass's plan and evidence for retrospective and
[story wrap-up](../dough-story-wrap-up/SKILL.md).
Emit `## TEST OPTIMIZATION COMPLETE` only when the pass's stop, or its
experiments' verification, measurement, and delivery, are complete, stating
its actual outcome.
