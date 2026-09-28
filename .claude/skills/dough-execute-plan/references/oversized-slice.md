# Refine an oversized slice

Use this project's target, hard limit, exceptions, and repeated-overrun threshold
under [slice sizing](../../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices).
Track elapsed implementation, focused testing, and slice-local cleanup with the
host clock; exclude explicit CI repair pauses. Lack of one coherent behavior or
failure to converge also calls for refinement.

Inventory tracked and untracked changes owned by the attempt and preserve
pre-existing work. Never use broad `git checkout .` or `git clean -fd`. Unclear
ownership requires [human judgment](execution-decisions.md#stop-for-human-judgment); do not guess,
silently revert unrelated work, or continue cleanup. Apply the same ownership
stop as [delivery staging](wrap-up.md#deliver-the-change) and
[resume](../SKILL.md#continue-or-recover-at-an-execution-boundary). A verified-result
[delivery](wrap-up.md#deliver-the-change) failure or
[integration](../../dough-story-wrap-up/SKILL.md#integrate-committed-story-branch-mode-closure)
failure is ordinary recovery, not this overrun.

When replanning is disabled, do not plan, refine, or retry. First write useful
evidence under this project's executable-plan root, resolved from this project
as in [slice planning](../../dough-slice-planning/SKILL.md#resolve-execution-context).
Do not invent a location or write a plan. Evidence may be prose, snippets, or a
patch; it need not run. Then remove only current attempt-owned unfinished changes
outside that folder. Leave no abandoned incomplete code or test work elsewhere.
Preserve unrelated work and earlier delivered slices; do not undo delivered
work. Existing Taken entries stay Taken; invent no story, plan, or backlog
entry. Report the stop under [Finish or stop](../SKILL.md#finish-or-stop).

When replanning is allowed, continue as follows. For planned execution, safely park or revert
only attempt-owned changes, then record elapsed time, completed proof, and the
failed sizing assumption in the same plan. Invoke
[dough-slice-plan-refinement](../../dough-slice-plan-refinement/SKILL.md) only
when learning escalation permits slice refinement. The coordinator commits and
pushes the updated plan. Report `reverted and refined`, elapsed time, and
whether the hard limit applied, then restart from the plan on disk.

For quick execution, first make a safe stop in the conversation. Identify the
source, elapsed time, the failed sizing assumption, completed compatible
work and proof, and every incomplete attempt-owned change. Keep completed
compatible work and proof in place. Safely park or revert only incomplete
attempt-owned changes; do not discard completed work merely to give later slices
a clean starting point. Keep the same execution identity and backlog placement under
[Take or admit work](../SKILL.md#take-or-admit-work). A quick attempt that becomes
planned keeps its selected mode and checkout; Trunk Mode stays Trunk Mode in
the same worktree. Unclear ownership stops disposition and the dependent
planning path for human judgment. A [one-shot](one-shot.md) attempt instead
[escalates](one-shot.md#escalate-when-the-work-grows) into admission, which
gives it the identity to plan under.

After that stop, when the triggering instruction authorizes planning and
continued execution, use this project's
[ordinary slice planning](../../dough-slice-planning/SKILL.md) for remaining
work from the established source — the selected story or the sufficient
instruction. Replanning permission grants neither missing scope nor execution
authority. Transfer the source, relevant chat evidence, completed work and
proof, incomplete-change disposition, elapsed time, failed sizing assumption,
and retained identity into the ordinary plan as source, decisions, or
learnings needed for resume. Plan only the remaining work. Do not fabricate a
story, completed planned slices, already satisfied promises, a substitute
quick-execution record, or a second execution. Restart execute-plan from that
plan in the preserved checkout and mode; ordinary plan refinement remains
available before delegation. Reuse preserved proof while its boundary remains
unchanged.

If planning or continued execution is not authorized, or ordinary planning
returns a missing field or disputed decision, report the safe stop and that
exact need without creating the plan or fabricating a story. If evidence
changes the source scope or exposes a disputed constraint, use the existing human
decision path before planning the affected work; complexity alone does not
authorize a scope change.
