## Settle freshness before dependent work

For planned Story Branch execution, settle freshness when implementation returns,
before proof acceptance, the coordinator's refactor pass, or expensive boundary
and whole-suite gates. Keep focused implementation checks. Repeat this settlement
at delivery step 5 before the done mark, commit or publication: an early check
does not reserve trunk or cover later movement. Trunk Mode is unchanged.

From the execution checkout, use its Taken identity and authorized remote/trunk:

```sh
node '<installed-execute-plan>/scripts/source-freshness.mjs' check --workspace '<execution checkout>' --identity '<identity>' --remote '<authorized remote>' --target-ref 'refs/heads/<trunk>'
```

Receive and inspect the terminal result in a separate tool-result boundary before
launching dependent work. A shell chain, parallel check and delivery, or sequential
awaited calls inside one uninspected orchestration do not establish this boundary.
On `amended`, `conflict` or refusal, follow [source freshness](source-freshness.md):
absorb amendments or reconcile conflicts, handle promises and source review,
then recheck until `unchanged` or stop as directed.

On `unchanged`, [check named siblings](#check-named-siblings) if the story or plan
names any for coordination. Receive and inspect that result before dependent work
too. An affected sibling holds refactoring and expensive proof, as well as delivery,
until reconciliation and applicable renewed proof settle it. Otherwise proceed
with sufficient unchanged proof; an unchanged check adds no full run. Renew only
proof affected by reconciliation or later movement.

## Deliver the change

Use the selected [execution checkout](execution-location.md) for delegated
refactoring, generation and host operations. Delivery publishes after commit;
it neither integrates nor retires the checkout.

Retain the unfinished delivery boundary: owned paths, accepted proof, returns and
refactor disposition, observer identity, candidate/`suffixBase`, previously
published base, and accepted revision/target after any rewrite. On recovery,
reconcile actual state under [execution-boundary recovery](../SKILL.md#continue-or-recover-at-an-execution-boundary)
and [interrupted publication](trunk-publication.md#resume-an-interrupted-publication);
continue only the unfinished obligation.

After proof acceptance and the independent refactor or disposition below, fresh increments and
owned repairs use [guarded fresh preparation and delivery](fresh-increment.md)
for steps 3–8. Resolve its required project operations and publication context
first. Never follow failure with a separate commit or older-HEAD delivery.
Without publication authority, select local-only and retain its receipt.

1. Classify the uncommitted change once. Record `records-only` for changes only to
   planning, execution, finding or research records (plans, seeds, backlog, source
   reviews, agent records, DearDough and maintainer findings). An owned repair or small change wholly
   within an accepted refactor return's paths and consumer trace, adding no
   representation, helper, dependency or path, may instead name that return as
   already traced. Retain either disposition beside the slice or quick delivery
   boundary. Otherwise, or when unsure, spawn a fresh agent for
   [dough-post-change-refactor](../../dough-post-change-refactor/SKILL.md).
   Supply source, checkout/branch, slice/plan, project context, owned changed paths
   and boundaries, accepted proof/consumer trace and inspected setup/observations,
   plus dispositions since the last pass and the expensive-proof decision with
   same-machine competing owned work. Keep formatting and hook-owned lint with
   the coordinator. Uphold decide-before-testing, explicit human verification
   authority and whole-product reach beyond the story's files.
2. Require `## REFACTOR COMPLETE` unless one of those two dispositions applies,
   and recheck [execution decisions](execution-decisions.md). Inspect newly affected
   implementation and observation locations using its paths, boundaries and proof
   effects. Reuse confirmed unchanged inspection; accept invalidated proof's rerun
   or replacement under **Accept proof**. A refactor stop, missing marker or
   disposition, contradiction or unresolved proof gap prevents commit. Report
   unnecessary test runs as deviations; reuse valid evidence.
3. Run triggered generation unless refactoring already regenerated and verified
   compatible output. Fix source, regenerate and validate affected consumers.
4. Run selective formatting directly once, including a planning-only no-op, before
   staging. Require success; repair mechanical failures and repeat only if repairs
   invalidate preparation. Stop for semantic or design judgment.
5. For planned execution, record learnings, reusable proof and revised slices
   under [plan refinement](../../dough-story-refinement/references/planning.md#refine-the-active-plan).
   Guarded fresh delivery marks the slice `done`; use
   [`progress: true`](fresh-increment.md#observe-progress-proof) when proof needs
   the published revision. For planned Story Branch work, repeat
   [freshness settlement](#settle-freshness-before-dependent-work) and inspect its results before guarded fresh delivery,
   even after early settlement.
   Before the done mark or commit, run the installed [story-obligations script](../scripts/story-obligations.mjs):
   `node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --slice N`
   with [resolved inputs](story-obligations.md#use-the-record-at-execution-boundaries).
   A refusal blocks both transitions even with `planned` status. Publish plan
   evidence [without renewing readiness](../../dough-product-backlog/references/record-preparation.md#plan-evidence-during-delivery).
   Record CI repairs with the interrupted slice's existing status; guarded
   delivery names the already `done` slice repaired. Quick execution keeps its
   learnings, decisions, delivery progress and proof in conversation, creating no
   plan, completion note, summary or substitute record. An admitted story keeps
   only its scope.
   Stale understanding stops at the safe delivery boundary for human review:
   record `awaiting story review` or `awaiting correction review` and identify the
   source and affected Goal, Scope or plan field, in its canonical seed or the
   plan for a plan-homed correction. Change no story, correction outcome or scope;
   invent no story. This plan update alone needs no further formatting pass.
6. Stage only owned files or separable changes and inspect them; stage all only
   when all is owned. Unrelated unstaged work does not block delivery. Resolve
   unrelated staged content or ambiguous ownership with its owner; never stash,
   reset, restage, revert or silently unstage another writer's work to isolate it.
7. Commit CI-safe work under [agent commits](agent-commits.md) when applicable.
   The check-only hook owns staged lint without formatting or index mutation;
   resolve any other hook contract first. Fix mechanical findings and stop for
   semantic/design judgment. Do not run hook-owned lint independently. If repairs
   invalidate preparation, format again before restaging and retrying.
8. Resolve the owned unpublished suffix immediately before managed
   [increment and repair publication](trunk-publication.md#publish-an-execution-increment-or-repair).
   Supply its owner inputs (on Codex, retained coordinator and stream directory).
   Caller-selected/host-owned work stays in its recorded checkout. Without push
   authority, report the local commit pending publication and leave remote refs
   unchanged. Exclude pending human edits; apply [direct-edit checks](maintain-default-checkout.md#direct-edit)
   when mutating the default checkout.
   Follow that publication owner's stops and recovery; maintenance is separate
   from acceptance. Register only the accepted revision, keep its observer and
   handle failures. Report unavailable observation and continue. Successful
   publication completes routine delivery; deliver safe work before a post-slice
   stop. Never wait for CI/deployment after normal or repair publication.

## Check named siblings

For planned Story Branch settlement on `unchanged`, resolve the named sibling
identities from their coordination references. Use the freshness result's merge
`base` and pinned `trunkRevision` to locate incoming changes. For example, inspect
the first-parent integration adding the canonical done record against its first
parent. If that integration bundles work or closure landed separately, trace the
specific product change to evidence attributing it to the sibling. A done record
shows closure; a broad branch-to-trunk diff only locates paths.

Compare the attributed behavior on pinned trunk with the slice and its proof,
including behavior inherited from earlier slices. Shared paths are inspection
leads. Absorbing a source link leaves this product judgment open.

If the slice still relies on earlier behavior, name the sibling, pinned trunk,
attributable revision when different, affected slice and shared paths/behavior.
Preserve working changes, local HEAD and the published branch. Withhold its done
mark, commit and push until reconciliation and rechecked proof. Resolve material
attribution/impact uncertainty under [reassessment](execution-decisions.md#reassess-before-extending-work),
distinguishing it from confirmed overlap; use [human judgment](execution-decisions.md#stop-for-human-judgment)
for a consequential unresolved choice.

Continue the guarded boundary when no accepted sibling change affects the slice
or its behavior already accounts for it. Unrelated landings add no hold.
