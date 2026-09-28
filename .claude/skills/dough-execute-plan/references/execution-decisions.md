# Execution decisions

Apply these decisions before delegation and after implementation and refactoring,
even when tests pass.

## Reassess before extending work

When corrections accumulate, evidence challenges the premise, or the owner asks
for simplification, reassess whether the remaining work is necessary before
adding implementation or slices. State the smallest authorized, evaluable
outcome and compare the strongest relevant simpler choice. Reuse available
evidence. For a decisive uncertainty, retrieve only the missing authoritative
passage or run a decision-bounded investigation under [targeted retrieval and
disposable research](disposable-research.md). State the question it answers and
what it leaves unproved; necessary investigation is not forbidden merely
because its evidence is unusually large. Refresh only knowledge whose source or
applicable assumption the new observation calls into question.

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

## Stop for human judgment

Stop only the affected path when a consequential decision remains unresolved:
user value, domain meaning or fit, structure constraining later work or
architecture, credentials or permissions, or ambiguity that could waste a
commit. For a change to the story, correction, or instruction's beneficiary, outcome, proof
or evaluation, scope, or story order, apply a change already authorized by the
human; otherwise name the affected source and field, evidence, and specific
decision needed, then wait.
Follow [learning escalation](../../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices)
before further subdivision. Use the handoff below for conflicting recorded
direction; the executing role must not revise direction to justify its work.

Deliver completed safe work under [wrap-up](wrap-up.md) before waiting unless a
refactor stop or unresolved proof failure prevents delivery. Resolve routine
naming, placement, test choices, minor refactoring, and necessary defects within
the authorized scope without another approval.

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
when a plan, quick story, or instruction requires rejection that appears supported only by
fixture counts or arrangements. Cite the exact source contract and the
conflicting story examples, deferred promises, or domain evidence. State what
behavior the proposed change would alter and ask the human to resolve the
restriction. Missing independent justification is grounds for this question,
even when the error seems clear; it is not permission to remove the restriction.

Stop the conflicting implementation, refactor, or correction-planning path and
leave disputed behavior unchanged. Return the evidence and decision needed in
the execution conversation, existing handoff, or active plan. A
behavior-preserving refactor cannot remove a contractual rejection, and passing
tests or plan compliance do not justify it.
Retain independently supported product constraints; a count limit is not
accidental merely because examples also have counts. Resume the disputed path
only under the human's decision, keeping the story and plan aligned when a plan
exists.

## Diagnose failed proof

For CI events, first use [CI observation and repair](ci-monitor.md#handle-a-notification).
For other failures, use focused diagnosis. Discount a failure as pre-existing,
unrelated, or environmental only with bounded evidence connecting its cause to
the affected proof. Record the cause, supporting observation, affected proof,
and remaining defect or disposition in the execution conversation, existing
handoff, or active plan.
Successful commands need no extra record.

A passing retry, repeated test name, or successful cleanup does not establish
cause or repair. Use a targeted retry only to test a stated explanation. On
recurrence, compare current conditions with the recorded cause; reuse evidence
when applicable while retaining any remaining defect. Stop diagnosis once the
evidence justifies a disposition. If uncertainty remains, report it and stop
for human judgment. A recorded explanation never waives required proof or the
CI repair protocol; an infrastructure finding cannot excuse a separate assertion
failure.

## Require current regression proof before a live action

Given an active plan naming a regression prerequisite, when the agent reaches
an authorized live action within the slice — one that changes a live
installation, for example a restart, deployed configuration change, upgrade,
migration, or updater enrollment — confirm an accepted current observation of
that prerequisite applies to the actual candidate and conditions before
performing the action, or leave the action unperformed and report the exact
obligation and gap. Read-only observation is not a live action and is not
gated by this decision.

When no accepted observation exists, run the named regression command and
treat its result under [Diagnose failed proof](#diagnose-failed-proof); do
not invent a second proof or failure contract for this decision. A passing
result authorizes the dependent live action; retain the literal command,
candidate identity, and result as the observation that authorized it.

A retained pass authorizes reuse only while it still applies to the actual
candidate and conditions now being acted on. Confirm that correspondence
using [own executable proof](../../dough-story-refinement/references/planning.md#own-executable-proof)'s
matching promise, implementation/candidate, setup, relevant conditions, and
observation — not a recent timestamp or an identical whole-repository SHA
alone. A relevant change to the actual candidate's code, tests, dependencies,
configuration, or environment breaks that correspondence and requires
reassessment; an irrelevant change, such as documentation-only, leaves it
intact. When correspondence holds, reuse the retained pass and cite it as the
authorizing observation instead of rerunning the command. A pass observed for
a different candidate or checkout does not qualify the actual one without
this demonstrated correspondence, regardless of an unrelated passing CI
result or an unaffected CI-visible subset.

An independently passing operational or health check — for example a curl
probe or container health check — never substitutes for the missing
regression prerequisite, even while it keeps passing throughout. Operational
checks retain their own observations and timing: do not require a
post-transition observation before the transition it can only observe, and do
not treat it as satisfying this prerequisite.

If the named command fails, is absent, or is unavailable — including when a
retained pass no longer corresponds to the actual candidate — leave the live
action unperformed; do not proceed on an explanation or on unrelated green
CI. Report the exact command/obligation and gap in the existing plan or
conversation, and continue other unrelated authorized work in the same
slice. Once the command is rerun and passes for the actual candidate, perform
the live action. Missing correspondence is diagnosed and resolved through
this same obtain-or-stop path; it does not need a separate decision.

This decision does not itself grant deployment permission and adds no new
post-action observation requirement: existing deployment authority and
post-action observations are unchanged.

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
