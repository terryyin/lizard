## Settle freshness before dependent work

For planned Story Branch execution, settle freshness on implementation return,
before proof acceptance, refactoring or expensive boundary/whole-suite gates.
Keep focused implementation checks. Repeat at delivery step 5 before done, commit
or publication: earlier checks neither reserve trunk nor cover later movement.
Trunk Mode is unchanged.

From the execution checkout, use its Taken identity and authorized remote/trunk:

```sh
node '<installed-execute-plan>/scripts/source-freshness.mjs' check --workspace '<execution checkout>' --identity '<identity>' --remote '<authorized remote>' --target-ref 'refs/heads/<trunk>'
```

Inspect the terminal result as a separate tool result before dependent work.
Chained or parallel check/delivery, and sequential awaited calls in one
uninspected orchestration do not establish this boundary.
On `amended`, `conflict` or refusal, follow [source freshness](source-freshness.md):
absorb amendments or reconcile conflicts, apply changed story promises to the affected work,
then recheck until `unchanged` or stop as directed.

On `unchanged`, [check coordination siblings](#check-named-siblings) named by the
story or plan; inspect that result before dependent work too. An affected sibling
holds refactoring, expensive proof and delivery until reconciled with renewed
proof. Otherwise reuse sufficient unchanged proof; the check adds no full run.
Renew only proof affected by reconciliation or later movement.

## Deliver the change

Use the selected [execution checkout](execution-location.md) for delegated
refactoring, generation and host operations. Delivery publishes after commit;
it neither integrates nor retires the checkout.

Retain owned paths, accepted proof, returns/refactor disposition, observer identity,
candidate/`suffixBase`, published base and accepted revision/target after rewrites.
Recover actual state under [execution-boundary recovery](../SKILL.md#continue-or-recover-at-an-execution-boundary)
and [interrupted publication](trunk-publication.md#resume-an-interrupted-publication),
then continue only unfinished obligations.

After proof acceptance and independent refactor or disposition, use
[guarded fresh preparation and delivery](fresh-increment.md) for fresh increments
and owned repairs (steps 3–8). Resolve project operations and publication context
first. Failure permits neither a separate commit nor older-HEAD delivery.
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
   same-machine competing owned work and the [failure policy](failure-policy.md).
   State what the change touched as facts, including the count of tests edited
   outside its subject, and leave the remedies to the pass.
   Keep formatting and hook-owned lint with the coordinator. Uphold
   decide-before-testing, explicit human verification authority and
   whole-product reach beyond the story's files.
2. Require `## REFACTOR COMPLETE` unless one of those two dispositions applies,
   and recheck [execution decisions](execution-decisions.md). Inspect newly affected
   implementation and observation locations using its paths, boundaries and proof
   effects. Reuse confirmed unchanged inspection; accept invalidated proof's rerun
   or replacement under **Accept proof**. A refactor stop, missing marker or
   disposition, contradiction or unresolved proof gap prevents commit. Report
   unnecessary test runs as deviations, and carry a returned named follow-up
   with the slice's learnings into wrap-up.
3. Run triggered generation unless refactoring already regenerated and verified
   compatible output. Fix source, regenerate and validate affected consumers.
4. Run selective formatting directly once, including a planning-only no-op, before
   staging. Require success; repair mechanical failures and repeat only if repairs
   invalidate preparation. Stop for semantic or design judgment.
5. For planned execution, record learnings, reusable proof and revised slices
   under [plan refinement](../../dough-story-refinement/references/planning.md#refine-the-active-plan).
   Guarded fresh marks the slice `done`; use [`progress: true`](fresh-increment.md#observe-progress-proof)
   when proof needs the published revision. Repeat and inspect
   [Story Branch freshness settlement](#settle-freshness-before-dependent-work) here.
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
   Supply owner inputs (Codex: retained coordinator and stream directory).
   Caller-selected/host-owned work keeps its recorded checkout. Without push
   authority, report the local commit pending publication; change no remote ref.
   Exclude pending human edits; use [direct-edit checks](maintain-default-checkout.md#direct-edit)
   before mutating the default checkout. Follow publication stops/recovery;
   maintenance stays separate from acceptance. Register only the accepted revision,
   keep its observer and handle failures. Report unavailable observation and
   continue. Publication completes routine delivery; deliver safe work before a
   post-slice stop. Never wait for CI/deployment after normal or repair publication.

## Check named siblings

For planned Story Branch settlement on `unchanged`, resolve sibling identities
from their coordination references. Locate incoming changes with freshness's
merge `base` and pinned `trunkRevision`, e.g. compare the first-parent integration
adding the canonical done record with its first parent. For bundled work or
separate closure, trace the product change to evidence attributing it to the
sibling. A done record shows closure; a broad branch/trunk diff only locates paths.

Compare the attributed behavior on pinned trunk with the slice and its proof,
including behavior inherited from earlier slices. Shared paths are inspection
leads. Absorbing a source link leaves this product judgment open.

If a slice still relies on earlier behavior, name its sibling, pinned trunk,
attributable revision when different, affected slice and shared paths/behavior.
Preserve working changes, HEAD and published branch. Withhold done, commit and
push until reconciliation and proof recheck. Route material attribution/impact
uncertainty through [reassessment](execution-decisions.md#reassess-before-extending-work),
distinct from confirmed overlap; use [human judgment](execution-decisions.md#stop-for-human-judgment)
for consequential unresolved choices.

Continue the guarded boundary when no accepted sibling change affects the slice
or its behavior already accounts for it. Unrelated landings add no hold.
