# Execution decisions

Apply these decisions before delegation, on implementation or refactor return,
and when a wait or retry can no longer advance the work.

## Reassess before extending work

On entry, resume, and after plan changes, check the [slice limit](../../dough-story-decomposition/references/problem-decomposition.md#limit-slices-per-plan)
before dispatch. For an over-limit current or proposed plan, [stop or split](#stop-or-split-before-more-work).
Apply the [active-time limit](oversized-slice.md#track-active-time) before
dispatch and [handle an overrun](oversized-slice.md#handle-the-overrun) before
more work.

When a decisive premise fails, corrections or overruns recur, the owner asks
for simplification, or the next step depends on a paused supplier, unavailable
proof, or a human decision, hold affected work. Compare the remaining work
with the smallest authorized outcome worth delivering. Reuse evidence or run a
[bounded check](disposable-research.md) that can settle the decision. If the
blocker remains and no independent authorized work can advance the outcome,
[stop or split](#stop-or-split-before-more-work) before waiting or retrying, or
[watch a paused supplier](wait-for-proof.md#watch-a-paused-supplier) first when
the developer asks. Refresh only knowledge the observation invalidates.

When an observation invalidates an assumption for remaining work, identify the
observation and its source location, the affected assumption, the consequences
for planned behavior and proof, and the selected authorized decision. Explain
why that decision changed. Retain unresolved limits needed for a later decision,
but leave raw diagnostics at their inspectable evidence locations. Align only
the affected remaining plan through [active-plan
refinement](../../dough-story-refinement/references/planning.md#refine-the-active-plan),
replacing invalidated future detail rather than appending an alternative.
Preserve completed work, proof, decisions, and learnings whose boundaries and
assumptions remain compatible. Drop only obligations the owner has authorized
removing.

When the current slice source is a `dough-test-optimization` plan, check its
Current decisions for a decisive-checkpoint obligation before [selecting the
next slice](../SKILL.md#execute-the-next-slice). A recorded, unresolved
obligation is unresolved reassessment under this rule, not an ordinary
next slice in plan order: apply its selected authorized decision — continue only
the supported strategy, continue only the explicitly retained independently
valuable work, or stop for the developer — before another experiment
slice is selected or dispatched. Do not recompute, restate, or second-guess the
remaining-gap comparison here; `dough-test-optimization` alone owns that
judgment and the checkpoint's measurement, invalidated assumption, and
consequences it records. A plan with no recorded obligation, or one whose
obligation is already resolved, proceeds to its next unfinished slice in plan
order.

Carry forward a supported existing-solution finding. Use
[dough-pfe](../../dough-pfe/SKILL.md) for an unforeseen addition or relocation of
responsibility, or evidence invalidating the selected candidate or domain fit;
supply the relevant story, plan finding when present, candidate evidence, and
new observation. Necessary structure may cross components or process boundaries
without changing the authorized responsibility. Align the remaining plan, when
one exists, before resuming affected work. Reassessment does not authorize
unrelated cleanup or a broader outcome. If the resulting decision changes story
scope or conflicts with an Accepted ADR, keep only the affected path stopped
under the human-judgment and recorded-direction procedures below; do not erase
the observation or revise the remaining plan around the unresolved dispute.

## Stop or split before more work

Stop the affected attempt. Preserve evidence and park or revert only its
unfinished changes under the [ownership rules](oversized-slice.md). Honor
[replanning permission](#choose-replanning-permission)
and [human-judgment stops](#stop-for-human-judgment).
When authorized, [split at a safe delivered boundary](../../dough-resplit-story/SKILL.md#split-at-a-delivered-slice-boundary)
and close the retained outcome through ordinary proof, delivery, CI, review and
wrap-up. The remainder awaits its own authorized start.

If the split is blocked, keep the Taken entry and delivered history. Record the
resume condition, close observation as in [finish or stop](finish-or-stop.md) and [report an unfinished stop](other-completion-paths.md).

## Stop for human judgment

Stop only the affected path when a consequential decision remains unresolved:
user value, domain meaning or fit, structure constraining later work or
architecture, credentials or permissions, or ambiguity that could waste a
commit. For a change to the story, correction, or instruction's beneficiary, outcome, proof
or evaluation, scope, or story order, apply a change already authorized by the
human; otherwise name the affected source and field, evidence, and specific
decision needed, then wait. A retained landing that the plan's human checkpoint
names is not a separate stop: [land it under that go-ahead](#land-before-a-trunk-step-under-its-checkpoint).
Follow [learning escalation](../../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices)
before further subdivision. Use the handoff below for conflicting recorded
direction; the executing role must not revise direction to justify its work.

Deliver completed safe work under [wrap-up](wrap-up.md) before waiting unless a
refactor stop or unresolved proof failure prevents delivery. Resolve routine
naming, placement, test choices, minor refactoring, and necessary defects within
the authorized scope without another approval.

## Land before a trunk step under its checkpoint

When the selected slice's human checkpoint names a
[Dough Land](../../dough-land/SKILL.md) `--keep-worktree` landing before a step
that acts from trunk, such as a release from trunk, the developer's go-ahead for
that checkpoint authorizes both. Deliver the slice's completed work first, then
run Dough Land with `--keep-worktree` on this execution checkout, asking no
separate landing question. Perform the trunk step from the landed revision only
after Dough Land reports the landing accepted; a refused or unfinished landing
stops that step. Resume the remaining work in the same workspace.

This go-ahead covers only the checkpoint it answers. A Story Branch Mode slice
that needs trunk without such a named landing stops for that checkpoint decision
under [human judgment](#stop-for-human-judgment). Closure still integrates the story branch. The trunk
step follows its own procedure, CI and release gates.

## Resolve conflicting recorded direction

When new evidence contradicts a North Star topic cited by the active plan, the
executing role stops only the affected path and returns the topic location,
contrary evidence, affected plan work, and consequences of continuing either
way. It must not edit the topic or reinterpret it to justify the implementation.
Independently supported paths may continue when their direction, mutable state,
and proof do not depend on the conflict.

The coordinator then applies the planner judgment in [architectural
thinking](../../dough-slice-planning/references/architectural-thinking.md#reconsider-recorded-direction-during-execution)
and aligns the remaining work through [active-plan
refinement](../../dough-story-refinement/references/planning.md#refine-the-active-plan).
Resume the stopped path only after the selected direction and remaining plan
are consistent. Preserve the active story, completed proof, **Taken** backlog
entry, recorded execution checkout and branch, and worktree throughout this
handoff; do not invoke retrospective or story wrap-up or mark partial completion.

The linked planner procedure owns conflicts with an Accepted ADR. Keep the
affected path stopped; neither the executing role nor coordinator may revise
North Star direction to work around the ADR.

## Resolve a disputed plan restriction

Use [examples and constraints](../../dough-story-refinement/references/planning.md#examples-and-constraints)
when a plan, quick story, or instruction imposes or preserves a restriction
that appears supported only by scope limits, examples, or inherited code or tests.
Restrictions include prerequisites, filters and suppressed operations. Cite the
exact source contract and the conflicting story examples, scope limits, or
domain evidence. State what behavior the proposed change would alter and ask the
human to resolve the restriction. Missing independent justification is grounds
for this question, even when the error seems clear; it is not permission to
remove the restriction.

Stop the conflicting implementation, refactor, or correction-planning path and
leave disputed behavior unchanged. Return the evidence and decision needed in
the execution conversation, existing handoff, or active plan. A
behavior-preserving refactor cannot remove a contractual restriction, and passing
tests or plan compliance do not justify it.
Retain independently supported product constraints; a count limit is not
accidental merely because examples also have counts. Resume the disputed path
only under the human's decision, keeping the story and plan aligned when a plan
exists.

## Diagnose failed proof

For CI events, first use [CI observation and repair](ci-monitor.md#handle-a-notification).
For other failures, use focused diagnosis. Attribute a failure as pre-existing,
unrelated, or environmental only with bounded evidence connecting its cause to
the affected proof. Attribution explains the cause; it does not remove the
failure from required proof or change who repairs it. Apply the
[failure policy](failure-policy.md) and this project's additions as written. Leave a repair only when another active owner holds
it, preserving their work and reporting that coordination boundary, or within
the stated scope of an explicit human exception. Record the cause, supporting
observation, affected proof, and remaining defect or disposition in the
execution conversation, existing handoff, or active plan.
Successful commands need no extra record.

A passing retry, repeated test name, or successful cleanup does not establish
cause or repair. Retry once to test a stated explanation. Before every repeat
batch and every CI run, state its hypothesis and stopping count; a rerun with
no stated hypothesis is not started. If the same required
check fails again under comparable conditions, hold that repair loop and
[reassess](#reassess-before-extending-work) before another attempt. Reuse
applicable evidence and retain repair ownership. When a slice reaches its
limit with the cause still unknown, diagnose in
[evidence rounds](diagnosis-rounds.md). Stop diagnosis once the
evidence justifies a disposition. If uncertainty remains, report it and stop
for human judgment. A recorded explanation never waives required proof or the
CI repair protocol; an infrastructure finding cannot excuse a separate assertion
failure.

## Choose replanning permission

Resolve this at entry and retain it for [delegation](delegation.md) and resume.
`--replan` and `--no-replan`, or a clear current instruction, grant or deny overrun
replanning independently of whether the source is a plan, story, or instruction.
Otherwise preserve existing planning authority. Neither option authorizes new
scope, destructive action, or missing execution.

## Refine an oversized slice

See [oversized-slice handling](oversized-slice.md) for target/limit resolution,
ownership-preserving cleanup, and the replanning-disabled/replanning-allowed
paths, including the quick-execution safe stop and restart into ordinary
planning.

## Handle an implementation commit

An implementation agent committing before coordinator delivery is a process
failure. Stop and report it. A soft reset of an unpushed, attempt-owned commit
is appropriate only when safe and permitted by the human's instructions;
otherwise preserve it for human judgment. Do not continue as if wrap-up passed.
