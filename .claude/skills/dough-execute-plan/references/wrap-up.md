# Slice wrap-up

The coordinator runs this sequence after implementation, including owned CI repair, using [increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair) for validated work.

## Accept proof

Apply [proof ownership](../../dough-story-refinement/references/planning.md#own-executable-proof).
For each slice, obtain a source-only review before comparing its return. Spawn a
fresh general-purpose reviewer without conversation inheritance, fork or resume;
do not reuse the implementation or refactor agent. Supply only the original source
text and explicit human amendments: the selected story's Goal, Scope, key examples
and exclusions, complete correction input, or authorized quick instruction. Give
no plan, return, product, proof, suspected omission or current-slice scope. For a
plan-homed correction, send its original correction input, not its slicing.
Tell the reviewer: use only these inputs, read no other project files, change
nothing; return source clauses with individual commitments, qualifications,
trigger/journey examples and exclusions. Separate compound promises; preserve
alternatives without turning examples into rejection rules. Derive promises,
not slice ownership or acceptance. Use supported fresh-context delegation or a
separate new session with these limits. Retain the actual reviewer launch/input
and returned commitments, or their inspectable locations, in existing acceptance
evidence; a coordinator's assertion is insufficient. If isolation or that evidence
is unavailable, name the missing review and stop dependent acceptance.

Compare that independent return with the source, then the plan, implementation
return, actual product and proof. In existing plan or acceptance evidence compare,
in any equivalent layout: applicable original trigger/setup → source-expected
result → actual observation → current or legitimate remaining owner/disposition.
Include each qualification and journey example; a heading, count or plan list
cannot stand for a compound promise. Return a concrete expected/actual mismatch
unless a real human amendment changes it. Broad accuracy or compatibility claims
cannot replace an explicit journey's expected result. Check source completeness before reuse.
Establish applicability at the current trigger or output before allowing later
ownership: a later slice cannot qualify an output that already owes the promise.
The source and human amendments outrank plan/return; correct a contrary mapping,
retaining source-correct behavior and proof. An owner-decision label is no human
amendment. Preserve legitimate remaining owners, exclusions and human
changes; resolve disputes through [human judgment](execution-decisions.md#stop-for-human-judgment),
stopping only dependent acceptance. Use existing records, not a second promise registry.

If a delegated call was interrupted, or its return arrived only after interruption, follow
[interrupted-delegation recovery](delegation.md#recover-an-interrupted-delegated-result) before accepting anything from it.
Use the return as a location index, never the promise list or accepted evidence. Inspect product, setup and observing
assertions/signals at each applicable boundary. Setup supplies preconditions; product and observation establish
the outcome. Passing commands, test names, `proof:` summaries, coverage claims or setup supplying the outcome
do not prove it. Untested or out-of-scope claims remain incomplete; they neither verify nor invent a source promise.

For filtered proof, compare actually selected tests/observations with the claimed promises.
A zero-exit command that selected nothing, or that selected fewer observations
than the claimed promises require, leaves those promises incomplete — obtain
the missing observations (broader or corrected selection, retargeted titles, or
another sufficient check) before accepting them, or return them as uncovered.
Reuse a trustworthy recorded selection and result when they still match the
claimed command, filter, and candidate; do not rerun solely for process.

When the change has consumers, as
[own executable proof](../../dough-story-refinement/references/planning.md#own-executable-proof)
defines them, verify the handoff's applicable old-reference search terms and
scope against the diff's removals, renames, or relocations of symbols or modules,
and changes to callable contracts. Inspect discovered consumers, including
unchanged tests and fixtures, then follow the changed effect through the reported
surface or entry points and compare the reached consumers with the return's
selection and proof.
A return that stops at file, selector, or helper matches leaves that trace
incomplete: inspect the enclosing observations or caller scenarios and their
setup before accepting it. Confirm sufficient current observations for the
affected purposes, including newly reached consumers. Do not accept on a prior
unaffected-suite or unused-consumer exclusion when the change reaches that
boundary. Do not accept or publish while proof for a known affected consumer is
missing, including one left unrun for CI: obtain the missing observation or
return the slice for it. Preserve sufficient reused or equivalent-purpose proof
and accepted evidence for unrelated consumers and unchanged boundaries. Require
the changed surface's suite when it runs within the slice's focused-check time,
not every suite. A discovered failure keeps the project's failure policy and
the consumer's legitimate promise; passing by supplying the outcome in setup or
weakening the assertion is not sufficient proof.

For a required behavior or observation missing from the product or return, obtain
it within authority or return it for correction before dependent acceptance. If
proof is unavailable, name the promise incomplete and stop only its dependent
delivery; preserve independently valid accepted evidence. For planned execution,
record every reported or independently discovered gap, loss, limitation or interim
behavior as a structured [story obligation](story-obligations.md), with discovery
provenance in the existing reported text or title, the story clause and exactly
one disposition before declaring dependent acceptance; a short outcome file does
not replace these obligations. The script checks structure and quotes, not judgment. Apply
[the relevant-path check](story-obligations.md#check-the-paths-before-closing-a-gap)
before closing a gap as `no user cost` or `proved`. Re-read every open
interim the current slice depends on; a newly wrong result becomes a return
owned by this slice, preserving the original text and provenance. For quick
execution, apply that reference's same-slice rule without creating a plan.

Read fixture or setup changes that turn a failing scenario green against the
story's examples. Check a reshaped example in its real shape, file or input;
green on a different fixture does not accept it. Examine a limit that could lose
the only copy of paused or saved work for that consequence; guidance for a
neighbouring case does not authorize it.

Retain the promise, accepted boundary, consumer trace, inspected setup/assertion
locations, results and literal commands for refactoring. When a location is missing,
the boundary unclear or evidence contradictory, inspect the smallest relevant
callers or implementation. Return incomplete or contradictory evidence with the
promise, inspected locations and gap; refactoring cannot supply missing behavior.

Judge substance under [the return contract](delegation.md#return-a-targeted-report-with-focused-proof).
Equivalent layouts carrying literal commands, results, owned changes, promise
coverage, boundaries, setup and observation locations need no report-only resend
or formatting retry. Missing terminal results or contradictions remain incomplete.
Explicit completion markers such as `## REFACTOR COMPLETE` stay verbatim.

Reuse accepted inspection only while its promise, boundary, implementation, setup
and observations match. Recover literal commands from the original handoff; rerun
only missing, ambiguous, changed-boundary or omitted required integration proof.
Preserve failures under [execution decisions](execution-decisions.md#diagnose-failed-proof).
For expensive proof, inspect the handoff's retained [pre-launch decision](../../dough-story-refinement/references/executable-proof.md#choose-expensive-proof-before-launch).
Reconsider changed host/workload conditions before launch; deferred proof stays incomplete with its owner and next condition.

Require CI-safe work: no deliberate failing tests or unfinished end-to-end proof
represented as complete. Mark unfinished proof by this project's convention;
run full CI before commit only when explicitly required.

## Deliver the change

Run this sequence in the selected [execution location](execution-location.md).
Pass its checkout to every delegated refactor, generator and host operation;
an inherited working directory must not redirect delivery. This sequence does
not integrate or remove a retained branch or worktree. After the commit, publish
the owned unpublished increment or repair through
[increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair).

As each obligation finishes, retain its delivery boundary in the execution conversation:
owned unfinished paths, accepted proof and boundary, implementation or refactor return,
exact observer identity when present, unpublished candidate and final `suffixBase`,
previously published base, and accepted revision and target after any rewrite.
On recovery, reconcile actual Git, agent and observer state under
[execution-boundary recovery](../SKILL.md#continue-or-recover-at-an-execution-boundary), classify the increment or repair with
[interrupted publication](trunk-publication.md#resume-an-interrupted-publication), and continue only that unfinished obligation.

After proof acceptance and the independent refactor below, fresh increments and
owned repairs use [guarded fresh preparation and delivery](fresh-increment.md)
for steps 3–8. Resolve its required project operations and publication context
first. It passes the successful commit SHA to managed delivery; failed preparation
or commit withholds delivery and observation. Never follow failure with a separate
commit or older-HEAD delivery. Without publication authority, select local-only
and retain its receipt. Existing candidates use retained candidate/base recovery.

1. Spawn a fresh agent to run
   [dough-post-change-refactor](../../dough-post-change-refactor/SKILL.md).
   Supply source, execution checkout/branch, slice, project context, changed paths and
   boundaries, ownership boundaries, and accepted proof/consumer trace with inspected
   setup/observation locations; include an existing plan's path. Carry the retained
   expensive-proof decision and same-machine competing owned work. Keep formatting and
   hook-owned lint with the coordinator; uphold decide-before-testing and explicit human verification authority.
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
   With that authority, the guarded fresh entry invokes managed delivery from
   that same checkout, passing the actual commit receipt as `validatedCandidate`
   and its `HEAD` from before this operation's first commit as
   `--previously-published-base`. Handle an `unpublished-base` stop
   as [current-branch publication](trunk-publication.md#publish-an-execution-increment-or-repair)
   does.
   The receipt names the accepted SHA, authorized target and observation result
   (attached, reused or explicit coverage gap). Supply the owner inputs required
   by [managed observation](trunk-publication.md#publish-the-candidate); Codex
   uses its retained coordinator and exact stream directory. Do not probe,
   start or register separately, or copy mailbox directories. Pending human edits
   stay out of the published commit.
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
