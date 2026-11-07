## Deliver the change

Run this sequence in the selected [execution location](execution-location.md).
Pass its checkout to every delegated refactor, generator and host operation;
an inherited working directory must not redirect delivery. This sequence does
not integrate or remove a retained branch or worktree. After the commit, publish
the owned unpublished increment or repair through
[increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair).

As each obligation finishes, retain its delivery boundary in the execution conversation:
owned unfinished paths, accepted proof and boundary, implementation or refactor return,
refactor disposition, exact observer identity when present, unpublished
candidate and final `suffixBase`, previously published base, and accepted revision and target after any rewrite.
On recovery, reconcile actual Git, agent and observer state under
[execution-boundary recovery](../SKILL.md#continue-or-recover-at-an-execution-boundary), classify the increment or repair with
[interrupted publication](trunk-publication.md#resume-an-interrupted-publication), and continue only that unfinished obligation.

After proof acceptance and the independent refactor or disposition below, fresh increments and
owned repairs use [guarded fresh preparation and delivery](fresh-increment.md)
for steps 3–8. Resolve its required project operations and publication context
first. Never follow failure with a separate commit or older-HEAD delivery.
Without publication authority, select local-only and retain its receipt.

1. Classify the uncommitted change once. Record `records-only` when it touches
   only planning, execution, finding or research records, such as plans, seeds,
   backlog, source reviews, agent records, DearDough entries or maintainer
   findings. For an owned CI repair or small change whose every
   changed concept lies within a slice's accepted refactor return (its reported
   changed paths and consumer-trace record), adding no representation, helper,
   dependency or path, record an already-traced disposition naming that return.
   Either replaces the agent; retain the disposition beside the slice in the
   plan, or in the quick delivery boundary. Otherwise, or when unsure, spawn a fresh agent to run
   [dough-post-change-refactor](../../dough-post-change-refactor/SKILL.md).
   Supply source, execution checkout/branch, slice, project context, changed paths and
   boundaries, ownership boundaries, and accepted proof/consumer trace with inspected
   setup/observation locations; include an existing plan's path and the
   dispositions since the last pass. Carry the retained
   expensive-proof decision and same-machine competing owned work. Keep formatting and
   hook-owned lint with the coordinator; uphold decide-before-testing and explicit human verification authority.
   A triggered pass keeps whole-product reach, never only the story's, slice's
   or repair's files.
2. Inspect its report, require `## REFACTOR COMPLETE`, and recheck
   [execution decisions](execution-decisions.md); only those two dispositions
   replace that marker. Use the reported changed paths,
   boundaries, and proof effects to inspect only newly affected implementation
   and observation locations. Reuse accepted inspection for boundaries reported
   and confirmed unchanged; when refactoring invalidated a boundary, accept its
   rerun or replacement proof only after inspecting the new boundary and
   observation locations under **Accept proof**. A refactor stop, missing marker
   or disposition, contradiction, or unresolved proof gap prevents commit. Report unnecessary
   test runs as deviations; reuse valid evidence instead of repeating work.
3. Run this project's generator if its trigger changed and the required output was
   not already regenerated and verified during refactoring. Fix generation at
   source and validate affected consumers.
4. Run this project's selective formatting command directly once; let it select
   affected components, including a planning-only no-op. Require success before
   staging. Repair mechanical failures and repeat only when the repair
   invalidates preparation. Stop for semantic or design judgment.
5. For planned execution, update the active plan and any project-required
   summary with learnings, accepted proof needed for reuse, and revised
   remaining slices under
   [plan refinement](../../dough-story-refinement/references/planning.md#refine-the-active-plan);
   ordinary guarded fresh delivery marks the selected slice `done` itself
   ([`progress: true`](fresh-increment.md#observe-progress-proof) when observation still needs the
   published revision).
   In Story Branch Mode, before the obligations check below, run
   `node '<installed-execute-plan>/scripts/source-freshness.mjs' check --workspace '<execution checkout>' --identity '<identity>' --remote '<authorized remote>' --target-ref 'refs/heads/<trunk>'`;
   continue on `unchanged`, and handle any other result under
   [source freshness](source-freshness.md).
   Before marking the slice done or committing, run the installed
   [story-obligations script](../scripts/story-obligations.mjs):
   `node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --slice N`.
   Use the [resolved inputs](story-obligations.md#use-the-record-at-execution-boundaries).
   A refusal blocks the done transition and commit until the named obligations
   are resolved; a `planned` status does not bypass this check.
   Publish that plan evidence without renewing readiness: follow
   [plan evidence during delivery](../../dough-product-backlog/references/record-preparation.md#plan-evidence-during-delivery).
   For stale feature-story understanding, record `awaiting story review` and
   identify the selected story in its seed and the affected field. For stale
   correction understanding, record `awaiting correction review` and identify
   the correction story's Goal or Scope in its seed, or the affected plan field;
   a plan-homed correction's plan holds both. Stop at the safe delivery boundary
   without changing other stories or the correction outcome. This plan update
   alone does not trigger another formatting pass. Record a CI repair result
   with the interrupted slice's existing status; its guarded delivery names
   the already `done` slice it repairs. For quick execution, do not
   create or update a plan, completion note, project summary, or substitute
   execution record; retain learnings, decisions, delivery progress, and proof
   in the conversation. An admitted story keeps only its scope.
   If source understanding is stale, identify the affected field and, when a
   story is the source, its canonical seed location; stop for human review
   without changing its scope or inventing a story.
6. Stage only owned files or separable owned changes and inspect the staged diff.
   Stage all content only when all of it is owned. Unrelated unstaged work does
   not block delivery. Resolve unrelated staged content or ambiguous ownership
   with its owner; never stash, reset, restage, or revert another writer's
   work to isolate this commit, or silently unstage it.
7. Commit CI-safe work, as an [agent commit](agent-commits.md) when that
   reference applies. The commit runs this project's check-only lint hook on
   staged components, with no formatting or index mutation. Resolve a different
   hook contract before committing. Fix mechanical findings; stop for semantic
   or design judgment. Do not run hook-owned lint independently. If hook
   repairs invalidate preparation, rerun formatting before restaging and
   retrying.
8. Immediately before publishing, resolve the owned unpublished suffix in the
   execution workspace. With publication authority, publish it through managed
   [increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair).
   Caller-selected current-branch and host-owned work stay in the recorded
   checkout. Without that authority, commit there, report the revision as
   committed and pending publication, and do not push; remote refs and the
   recorded checkout identity stay unchanged.
   Supply the owner inputs required
   by [managed observation](trunk-publication.md#publish-the-candidate); Codex
   uses its retained coordinator and exact stream directory. Pending human edits
   stay out of the published commit.
   On the default checkout, follow its
   [direct edit](maintain-default-checkout.md#direct-edit) checks before
   mutating it. A local commit or merge stays local; do not report it as remote
   publication. Codex, Cursor, and Claude keep the recorded checkout and
   authorized target their adapters supply. A rejected push follows
   [rejected-push recovery](trunk-publication.md#recover-a-rejected-push).
   A [publication stop](trunk-publication.md#preconditions), including a
   [rebase conflict stop](trunk-publication.md#resolve-a-publication-rebase-conflict)
   or a recovery stop, leaves that candidate unpublished and recoverable.
   A [default-checkout preservation result](maintain-default-checkout.md#preserve-pending-local-work)
   defers maintenance and does not by itself reject an accepted remote
   candidate or block a push from a separate owned workspace. Never register
   a SHA the remote has not accepted or treat that unpublished candidate as
   delivered.
   With observation attached or reused, keep that observer running and handle
   delivered failures through its repair protocol. An unavailable bridge leaves
   publications unobserved: report that gap and continue.
   Success completes routine delivery; a post-slice decision stop occurs after
   safe work is delivered. Never wait for CI or deployment after a normal or
   repair publication.
