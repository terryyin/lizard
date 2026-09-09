---
name: dough-execute-plan
description: >-
  Executes the slices in an executable plan for one selected story, with
  independent refactoring, selective formatting, plan updates, commit and push,
  and asynchronous CI repair. Use to execute a plan, run a plan, or execute
  slices; does not execute a seed or decide story scope.
---

# Execute a plan

Execute one selected story through the slices in its existing plan. The
coordinator owns delivery; implementation agents return uncommitted changes.

## Establish execution context

Read the plan and the selected story in its seed. Require an understood story
and executable slices; a seed supplies story context, not execution instructions.
Use [planning scope and lifecycle](../dough-story-refinement/references/planning.md)
for story ownership, proof, plan updates, and completion cleanup. Use
[slice decomposition](../dough-story-decomposition/references/problem-decomposition.md#decompose-slices)
for Behavior and Structure slices, sizing, and learning escalation.

Resolve from this project:

- plan path, slice status vocabulary, slice target, hard limit, and exceptions;
- navigation, focused test commands, runtime wrapper, and workflow precedence;
- selective formatting command, commit hook contract, and authorized push destination;
- generated-artifact triggers and commands when affected; and
- context required by [dough-post-change-refactor](../dough-post-change-refactor/SKILL.md).

Name missing context and stop the affected work before delegation. When another
execution tool such as GSD invokes this skill, keep the same slice delivery
contract. Do not treat that tool's phase or task as an alternative to a slice.

Read [execution decisions](references/execution-decisions.md),
[delegation](references/delegation.md), and [wrap-up](references/wrap-up.md)
before implementation. Read [CI observation](references/ci-monitor.md) before
the first push and load only the current host's notification adapter. For a
bounded investigation, use [disposable research](references/disposable-research.md).

## Execute the next slice

1. Read the plan's current slice statuses, decisions, learnings, and proof.
   Use this plan as execution and resume state; do not update a separate project
   state index. Recover an existing CI observer before considering a new one.
2. Select the next unfinished slice whose dependencies are complete. Apply
   [execution decisions](references/execution-decisions.md). For a slice that
   removes or disables behavior or state, also run the
   [destructive later-outcome check](references/destructive-later-outcome-check.md).
3. If refinement is needed and learning escalation permits it, invoke
   [dough-slice-plan-refinement](../dough-slice-plan-refinement/SKILL.md) on the
   same plan, then restart at step 1. Otherwise delegate under
   [delegation](references/delegation.md).
4. On return, recheck execution decisions. For an incomplete or oversized slice,
   follow that reference before delivery. Otherwise inspect the proof under
   [wrap-up](references/wrap-up.md#accept-proof) and confirm uncommitted work
   or an explained empty change.
5. Run [wrap-up](references/wrap-up.md#deliver-the-change) end to end. Resume
   at step 1 after a successful push, handling delivered CI events along the way.

Run slices concurrently only when their file changes, mutable state, and plan
writes do not overlap. Each slice completes its own coordinator-owned delivery
before a dependent slice starts.

## Finish or stop

On completion, a stop requiring human judgment, or cancellation, close the CI
observer through the current host adapter. Handle delivered failures, then
stop observers without waiting for CI. Report pending CI as unobserved.

When all slices are done, apply the planning reference's completion cleanup and
report completed slices, retained evidence, and observer shutdown. End with
`## PLAN EXECUTION COMPLETE` only after required delivery and shutdown succeed.

Otherwise report the active plan, next unfinished slice, preserved work and
observer state, and the decision or recovery action needed. Do not emit the
completion marker.
