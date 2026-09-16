# Destructive later-outcome check

Run this check before delegating a destructive slice.

1. Decide from the current slice text and execution source whether the slice would
   remove or disable a user-visible path, field, persisted state, or scheduled
   behavior. If not, proceed without extra analysis or another agent.
2. If it would, compare the named removal target with the explicitly named
   outcomes in every later planned slice in document order, regardless of
   status. For quick execution there is no later slice; compare the target with
   the canonical story's promises and dependencies instead. Keep the comparison
   compact: removal target, relevant later outcome(s), and compatible or
   conflict. Do not invent implicit future requirements.
3. If no compared outcome, promise, or dependency conflicts with the target,
   proceed with normal slice decision checks and delegation.
4. In planned execution, if the current instruction is unambiguously stale
   relative to a later named outcome, update the plan and record which outcome
   exposed the conflict and why the instruction was stale. Restart
   [execution](../SKILL.md#execute-the-next-slice) at step 1. Reread
   the plan and repeat execution decisions, the slice definition check,
   dependency checks, this destructive-outcome check, and refinement checks
   before delegation.
5. If a quick story conflicts with the removal, or if both planned outcomes
   remain valid and resolving the conflict requires a value or design choice,
   stop for human judgment before implementation. Report the conflicting
   outcomes and the decision required; do not rewrite the story or create a plan
   from the quick path.

Compare later outcomes only to detect destructive conflicts. For refactoring
justification, use [dough-post-change-refactor](../../dough-post-change-refactor/SKILL.md#discover-scope).
