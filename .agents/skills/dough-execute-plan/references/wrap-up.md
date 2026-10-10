# Slice wrap-up

The coordinator runs this sequence after implementation, including an owned CI
repair. Publication of a validated increment or that repair is
[increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair).

## Accept proof

Apply [proof ownership](../../dough-story-refinement/references/planning.md#own-executable-proof).
Treat the implementation return as an index, not as accepted evidence. For each
promise the return treats as covered or verified, inspect the uncommitted change
at its reported product boundary and the concrete setup and assertion or signal
locations it names. Confirm that setup supplies only the starting precondition,
that an observing assertion or signal actually exercises the claimed behavior,
and that the product at that boundary establishes the promised outcome. A
passing command, test name, `proof:` summary, prose that says the fixture covers
a behavior, or an assertion whose setup supplies the outcome does not establish
the promise. When the return presents behavior as verified without a matching
observation, or the product contradicts that claim, do not tell the developer it
is covered: return the required behavior for correction, or obtain the matching
observation within authority, before accepting it. A truthful description of
untested or out-of-scope behavior stays incomplete for that claim; it is not
verified evidence and does not invent a new story promise.

When the claimed proof used name, pattern, or other filtering, verify which
tests or observations were actually selected against the promises the return
treats as covered. Accept only promises whose selected observations match.
A zero-exit command that selected nothing, or that selected fewer observations
than the claimed promises require, leaves those promises incomplete — obtain
the missing observations (broader or corrected selection, retargeted titles, or
another sufficient check) before accepting them, or return them as uncovered.
A selected test count supports that check; it is never the full promise mapping.
Reuse a trustworthy recorded selection and result when they still match the
claimed command, filter, and candidate; do not rerun solely for process.

When the change has consumers, as
[own executable proof](../../dough-story-refinement/references/planning.md#own-executable-proof)
defines them, check its current consumers against the return's selection and the
claimed proof. Do not accept on a prior unaffected-suite or unused-consumer
exclusion when the change still reaches that consumer. Do not accept or publish
the slice while a known consumer — one the return's selection found, or one you
can name — is unrun, including one left for CI: run it, or return the slice for
it. Unrelated consumers and unchanged boundaries keep their accepted evidence.
Reuse sufficient equivalent-purpose proof; require the changed surface's suite
when it runs within the slice's focused-check time, not every suite.

When a required observation is explicitly missing from the return — including
required readiness or requeue behavior named as untested while delivery is still
treated as ready — obtain that observation within authority before accepting the
dependent promise, or name the required promise incomplete and leave its
dependent delivery unaccepted. In planned execution, record it as a story
obligation under the procedure below; quick execution keeps the same-slice rule.
If the required proof cannot be obtained, stop only that dependent path and
preserve independently valid accepted evidence.

For planned execution, record every named gap, loss, limitation, or interim
behavior as a structured [story obligation](story-obligations.md), including
reported text, the story clause, and exactly one disposition. Read the selected
story's goal, key examples, and exclusions before choosing that disposition;
the script checks structure and quotes, not that judgment. Re-read every open
interim the current slice depends on; a newly wrong result becomes a return
owned by this slice, preserving the original text and provenance. For quick
execution, apply that reference's same-slice rule without creating a plan.

Read every fixture or setup change that turns a failing scenario green against
the story's examples. When the return reshapes or
simplifies a fixture or setup, check the example in its real shape, the real
file or input it names; green on a fixture the example does not describe does
not accept it. Examine a limit that could lose the only copy of paused or saved
work for that consequence; guidance for a neighbouring case does not authorize
it.

During this slice, accept only observations supported by inspected locations and
results. Retain the promise, accepted boundary, inspected locations, and literal
command in the slice wrap-up for refactoring. Expand inspection to the smallest
relevant underlying callers, setup, assertions, or implementation
when a location is missing, the boundary is unclear, or evidence contradicts the
change. Return incomplete or contradictory evidence to implementation, naming
the promise, inspected locations, and gap; refactoring cannot supply missing
behavior.

Judge the return by its substance, not its layout. The `proof:` block under
[the return contract](delegation.md#return-a-targeted-report-with-focused-proof)
is an example representation; accept an equivalent layout that carries the same
inspectable evidence — literal commands, results, owned changes, promise
coverage, boundaries, and setup and observation locations — without a
report-only resend or formatting-only retry. Formatting never substitutes for
substance: a canonical-looking report missing the terminal result or
contradicting inspected evidence remains incomplete whatever its layout.
Explicit completion markers with a separate workflow contract, such as
`## REFACTOR COMPLETE`, stay verbatim.

Reuse accepted inspection while its promise, boundary, implementation, setup,
and observations remain unchanged. Recover literal commands from the original
handoff when possible. Rerun only for missing or ambiguous proof, a covered
boundary changed during wrap-up, or omitted integration proof required by the
slice. Do not sample randomly or repeat valid proof to manufacture process
compliance. Preserve failures under
[execution decisions](execution-decisions.md#diagnose-failed-proof).

Require CI-safe work before delivery: no deliberate failing tests or unfinished
end-to-end proof represented as complete. Use this project's convention to mark
unfinished proof. Do not run full CI before commit unless explicitly required.

## Deliver the change

Run this sequence in the selected execution location established by
[execution location](execution-location.md). Pass its checkout to every
delegated refactor, generator, and host operation; do not let an inherited
working directory redirect delivery. This slice delivery does not integrate
or remove a retained execution branch or worktree. After the commit, publish
the owned unpublished increment or repair through
[increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair).

As each obligation finishes, retain the current delivery boundary in the
execution conversation with the owned unfinished paths, accepted proof and its
boundary, applicable implementation or refactor return, exact observer
identity when present, and the unpublished candidate with its final `suffixBase`,
previously published base, and accepted revision and target after any rewrite. On recovery,
reconcile this focused record with actual Git, agent, and observer state under
[execution-boundary recovery](../SKILL.md#continue-or-recover-at-an-execution-boundary).
Classify the increment or repair with
[interrupted publication](trunk-publication.md#resume-an-interrupted-publication)
and continue that unfinished obligation only.

1. Spawn a fresh agent to run
   [dough-post-change-refactor](../../dough-post-change-refactor/SKILL.md).
   Supply the execution source, execution checkout and branch, slice,
   accepted proof with its inspected setup and observation locations, changed
   paths and boundaries, project context, and ownership boundaries; supply the
   plan path only when one exists. Keep
   formatting and hook-owned lint with the coordinator. Do not
   add instructions contradicting that skill's decide-before-testing contract;
   explicit human verification requests remain authoritative.
2. Inspect its report, require `## REFACTOR COMPLETE`, and recheck
   [execution decisions](execution-decisions.md). Use the reported changed paths,
   boundaries, and proof effects to inspect only newly affected implementation
   and observation locations. Reuse accepted inspection for boundaries reported
   and confirmed unchanged; when refactoring invalidated a boundary, accept its
   rerun or replacement proof only after inspecting the new boundary and
   observation locations under **Accept proof**. A refactor stop, missing marker,
   contradiction, or unresolved proof gap prevents commit. Report unnecessary
   test runs as deviations; reuse valid evidence instead of repeating work.
3. Run this project's generator if its trigger changed and the required output was
   not already regenerated and verified during refactoring. Fix generation at
   source and validate affected consumers.
4. Run this project's selective formatting command directly once; let it select
   affected components, including a planning-only no-op. Require success before
   staging. Repair mechanical failures and repeat only when the repair
   invalidates preparation. Stop for semantic or design judgment.
5. For planned execution, update the active plan and any project-required
   summary with learnings, slice status, accepted proof needed for reuse, and
   revised remaining slices under
   [plan refinement](../../dough-story-refinement/references/planning.md#refine-the-active-plan).
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
   with the interrupted slice's existing status. For quick execution, do not
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
   execution workspace. Publish it through managed
   [increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair)
   when this caller has publication authority.
   Caller-selected current-branch work and an already-supported host-owned
   execution stay in the recorded checkout. Create no worktree and do not
   switch branches. Without that authority, commit there and report the
   revision as committed and pending publication. Do not push. Remote refs
   stay unchanged, and the checkout identity stays the recorded path.
   With that authority, invoke the installed managed delivery entry point from
   that same checkout, passing its `HEAD` from before this operation's first
   commit as `--previously-published-base`. Handle an `unpublished-base` stop
   as [current-branch publication](trunk-publication.md#publish-an-execution-increment-or-repair)
   does.
   The receipt is the accepted SHA, the authorized target,
   and the observation result (attached, reused, or an explicit coverage gap).
   Do not run a separate observer probe, start, or `register-push` for this
   managed path, and do not copy mailbox directories by hand. A Codex
   coordinator passes its observer note's coordinator and stream directory as
   `--coordinator` and `--observer-directory`; that stream, armed at execution
   start, is the observer this delivery registers on. A pending human edit on that checkout stays out of the
   published commit.
   When the selected checkout is the default checkout, follow its
   [direct edit](maintain-default-checkout.md#direct-edit) checks before
   mutating it. A local commit or a local merge stays a local
   operation; do not report it as remote publication. Codex, Cursor, and
   Claude keep the recorded checkout and authorized target their existing
   adapters already supply. A rejected push follows
   [rejected-push recovery](trunk-publication.md#recover-a-rejected-push).
   A [publication stop](trunk-publication.md#preconditions),
   including a
   [rebase conflict stop](trunk-publication.md#resolve-a-publication-rebase-conflict)
   or a recovery stop, leaves that candidate unpublished and recoverable.
   A [default-checkout preservation result](maintain-default-checkout.md#preserve-pending-local-work)
   defers maintenance and does not by itself reject an accepted remote
   candidate or block a push from a separate owned workspace. Do not register
   a SHA the remote has not accepted, and do not treat that unpublished
   candidate as delivered.
   When observation is attached or reused, keep that observer running and
   handle delivered failures through its repair protocol. An unavailable
   bridge leaves publications unobserved: report that gap and continue.
   Success completes routine delivery; a post-slice decision stop occurs after
   safe work is delivered. Never wait for CI or deployment after a normal or
   repair publication.
