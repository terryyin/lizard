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
commit. For a change to the story or correction's beneficiary, outcome, proof
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
when a plan or quick story requires rejection that appears supported only by
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

## Refine an oversized slice

Use this project's target, hard limit, exceptions, and repeated-overrun threshold
under [slice sizing](../../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices).
Track elapsed implementation, focused testing, and slice-local cleanup with the
host clock; exclude explicit CI repair pauses. Lack of one coherent behavior or
failure to converge also calls for refinement.

Inventory tracked and untracked changes owned by the attempt and preserve
pre-existing work. Never use broad `git checkout .` or `git clean -fd`. Unclear
ownership requires human judgment. For planned execution, safely park or revert
only attempt-owned changes, then record elapsed time, completed proof, and the
failed sizing assumption in the same plan. Invoke
[dough-slice-plan-refinement](../../dough-slice-plan-refinement/SKILL.md) only
when learning escalation permits slice refinement. The coordinator commits and
pushes the updated plan. Report `reverted and refined`, elapsed time, and
whether the hard limit applied, then restart from the plan on disk.

For quick execution, first make a safe stop in the conversation. Identify the
canonical story, elapsed time, the failed sizing assumption, completed compatible
work and proof, and every incomplete attempt-owned change. Keep completed
compatible work and proof in place. Safely park or revert only incomplete
attempt-owned changes; do not discard completed work merely to give later slices
a clean starting point. Keep backlog placement under
[Take queued work](../SKILL.md#take-queued-work). Unclear ownership stops
disposition and the dependent planning path for human judgment.

After that stop, use this project's
[ordinary story planning workflow](../../dough-slice-planning/SKILL.md) for the
remaining work when the story goal, scope, examples, and constraints are still
understood and the triggering instruction authorizes planning and continued
execution. Transfer the canonical story, relevant chat evidence, completed work
and proof, incomplete-change disposition, elapsed time, and failed sizing
assumption into the ordinary plan as source, decisions, or learnings needed for
resume. Plan only the remaining work. Do not fabricate completed planned slices,
repeat already satisfied promises, create a substitute quick-execution record,
or treat the plan as a second execution. Restart execute-plan from that plan;
ordinary plan refinement remains available before delegation. Reuse preserved
proof while its boundary remains unchanged.

If planning or continued execution is not authorized, report the safe stop and
the exact next authorization needed without creating the plan. If evidence
changes the story scope or exposes a disputed constraint, use the existing human
decision path before planning the affected work; complexity alone does not
authorize a scope change. Once that decision is resolved, refine the canonical
story as required before ordinary planning.

## Handle an implementation commit

An implementation agent committing before coordinator delivery is a process
failure. Stop and report it. A soft reset of an unpushed, attempt-owned commit
is appropriate only when safe and permitted by the human's instructions;
otherwise preserve it for human judgment. Do not continue as if wrap-up passed.
