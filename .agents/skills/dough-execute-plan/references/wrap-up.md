# Slice wrap-up

The coordinator runs this sequence after implementation. For a CI repair, apply
the same proof and delivery gates while keeping the interrupted slice in progress.

## Accept proof

Apply [proof ownership](../../dough-story-refinement/references/planning.md#own-executable-proof).
Compare each promise with its assertion and enough setup to identify the tested
boundary. Passing commands, test names, and `proof:` summaries alone are
insufficient. Return incomplete or contradictory evidence to implementation,
naming the promise and gap; refactoring cannot supply missing behavior.

Reuse inspected proof while promises and boundaries remain unchanged. Recover
literal commands from the original handoff when possible. Rerun only for missing
or ambiguous proof, boundaries changed during wrap-up, or omitted integration
proof required by the slice. Do not sample randomly or repeat valid proof to
manufacture process compliance. Preserve failures under
[execution decisions](execution-decisions.md#diagnose-failed-proof).

Require CI-safe work before delivery: no deliberate failing tests or unfinished
end-to-end proof represented as complete. Use this project's convention to mark
unfinished proof. Do not run full CI before commit unless explicitly required.

## Deliver the change

1. Spawn a fresh agent to run
   [dough-post-change-refactor](../../dough-post-change-refactor/SKILL.md).
   Supply the slice, plan path, implementation proof, project context, and ownership
   boundaries. Keep formatting and hook-owned lint with the coordinator. Do not
   add instructions contradicting that skill's decide-before-testing contract;
   explicit human verification requests remain authoritative.
2. Inspect its report, require `## REFACTOR COMPLETE`, and recheck
   [execution decisions](execution-decisions.md). A refactor stop, missing marker,
   or unresolved proof gap prevents commit. Report unnecessary test runs as
   deviations; reuse valid evidence instead of repeating work.
3. Run this project's generator if its trigger changed and the required output was
   not already regenerated and verified during refactoring. Fix generation at
   source and validate affected consumers.
4. Run this project's selective formatting command directly once; let it select
   affected components, including a planning-only no-op. Require success before
   staging. Repair mechanical failures and repeat only when the repair
   invalidates preparation. Stop for semantic or design judgment.
5. Update the active plan and any project-required summary with learnings, slice
   status, and revised remaining slices under
   [plan refinement](../../dough-story-refinement/references/planning.md#refine-the-active-plan).
   For stale story understanding, record `awaiting story review`, identify the
   selected story in its seed and the affected field, and stop at the safe
   delivery boundary without changing other stories. This plan update alone
   does not trigger another formatting pass. A CI repair records its result
   without marking the interrupted slice done.
6. Stage only owned files or separable owned changes and inspect the staged diff.
   Stage all content only when all of it is owned. Unrelated unstaged work does
   not block delivery. Resolve unrelated staged content or ambiguous ownership
   with its owner; never silently unstage, reset, or revert another task's work.
7. Commit CI-safe work using this project's check-only lint hook on staged
   components, with no formatting or index mutation. Resolve a different hook
   contract before committing. Fix mechanical findings; stop for semantic or
   design judgment. Do not run hook-owned lint independently. If hook repairs
   invalidate preparation, rerun formatting before restaging and retrying.
8. Push to the authorized destination. Success completes routine delivery; a
   post-slice decision stop occurs after safe work is delivered. Keep the
   [CI observer](ci-monitor.md) running and handle delivered failures through its
   repair protocol. Never wait for CI or deployment after a normal or repair push.
