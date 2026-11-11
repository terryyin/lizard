---
name: dough-story-wrap-up
description: >-
  Closes one completed feature story, bounded retrospective correction or
  context-only planless execution using available execution context. Assimilates
  lasting product knowledge, settles existing follow-ups and authorized product
  decisions, and removes spent plan and history with Git recovery. Reports missing
  or unfinished inputs truthfully. Use to close completed work, delete spent
  records, or continue settled planned execution under Execute Plan's continuation
  rule. When continuation carries a selected deferred process review, completes
  that review after Intent Coverage report and dispositions.
---

# Story wrap-up

Close one work item when the coordinator invokes wrap-up explicitly or through Execute
Plan's authorized [planned continuation](../dough-execute-plan/references/finish-or-stop.md#continue-settled-planned-work-into-wrap-up), retaining the same execution context and publication authority, including local-only/no-push instructions.
Use available context and optional retrospective advice; preserve lasting product knowledge and Git recovery.
When continuation carries a selected deferred process review, also
[complete that deferred process review](#complete-a-deferred-process-review).

## Resolve this project's context

Require one selected work identity. Use its canonical feature story, a
correction's minimal story linked to its plan, or its qualifying plan for a
plan-homed correction under [planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level).
For contextual work, the instruction, retained identity, changes, and proof
supply the context without a story, plan, or queue entry. If required context
is missing, name it, leave the work intact, and stop before closure.

Resolve from this project, not this skill's location:

- repository root and Git working tree;
- feature story's canonical seed location, identity, and heading or anchor conventions;
- planned work's plan location, status vocabulary, and identity;
- planless work's source, changes, available results, and retained identity;
- optional retrospective advice when present, including an empty result, and
  the product advice in the plan's [execution-complete record](../dough-execute-plan/references/finish-or-stop.md#record-execution-completion);
- Git commit conventions used to preserve a recoverable revision;
- selected mode, execution checkout and branch, authorized remote and target,
  and any originating or default checkout;
- product backlog path when the work has a **Taken** or **Backlog list** entry;
- shared seed, incoming links, assessment records, and process log
  (`DearDough.md` unless this project sets another canonical location).

Before deleting a plan that carries planned-execution identity, retain the
resolved mode, checkout, branch, remote, target, the execution checkout's
[management context](../dough-land/SKILL.md#retire-the-worktree) and its pending
[lifecycle commitments](references/lifecycle-commitments.md#carry-them-past-deletion)
in the coordinator's execution context. Do not create a parallel registry.
If required identity is missing, stop the action that needs it.

With supplied dashboard reporting context, read and retain
[dashboard completion](../dough-land/references/dashboard-completion.md), its
command and a surviving working directory before either Trunk Mode `finish`
or Story Branch retirement can remove the checkout. Prepare any attention file
outside it. Reporting is the final operation.

## Establish execution completion

- Planned work is complete when every slice is done. Run `check --planned-completion`
  and take its `pendingLifecycle` rows under [lifecycle commitments](references/lifecycle-commitments.md#read-them-at-completion).
- Work without a plan, including an investigation whose approach stayed unselected, is
  complete when its story or instruction, changes, and execution results show the promised outcome, including evidenced no-change.
- If implementation is unfinished, report it, leave active work intact, and stop.

## Assess intent coverage

After execution completion and before the before-cleanup commit, assess only planned
or planless feature stories. Corrections and contextual work proceed to knowledge
assimilation. Preserve any published Done result for this story; do not repeat the
check or migrate historical results.

Read this project's optional `.planning/open-dough.json` as a JSON object. Only boolean
`intentCoverage: true` opts in; missing file/key or `false` skips the automatic check.
Ignore unrelated keys. For unreadable, malformed or non-object JSON, or a non-boolean
`intentCoverage`, report the file/key error, skip checking, and continue independent closure.
Never coerce values or create, rewrite or repair configuration. Explicit developer
review requests remain available regardless of this preference.

When enabled and no published result exists, request one check from the installed
[Dough Intent Coverage](../dough-intent-coverage/SKILL.md) for the completed
implementation revision, supplying the story section, plan when present, and any
already available promise, plausible fault, focused command, fixture, or assertion
from review or execution. Prefer a plausible protection gap for an important
promise raised by review; otherwise let the skill choose from the key examples.
Pass hints as suggestions only; do not rediscover missing ones or move probe
mechanics here. Explicit manual requests remain valid without hints. It owns
scratch setup and restoration. Continue independent wrap-up while it runs. Retain
story, checked revision, worker continuation, saved report, and completed
dispositions in existing resumable closure context; keep the report outside
checkout cleanup.

Join before Done publication, spent-history deletion, or retirement of resume context.
Worker failure or an applicable deadline without a report leaves closure unfinished with
recovery surfaced and active sources preserved; do not manufacture a report or treat that
gap as Insufficient evidence. **Insufficient evidence** is a report; ordinary restoration
and cleanup gates still apply. Dispose findings under
[Intent Coverage findings](references/follow-up-disposition.md#dispose-intent-coverage-findings)
before the before-cleanup commit. Save the worker report verbatim, append a separately
identified wrap-up disposition section (checked revision, evidence, decision, canonical
follow-up links), and pass that combined text to
[completion](../dough-product-backlog/SKILL.md#remove-completed-items) with
`--intent-coverage`. If skipped with no supplied result, complete without coverage input.
No temporary file is the only home of the final result.

## Assimilate lasting knowledge

Move lasting behavior and design into this project's maintained code, tests,
and documentation. Describe the current product without execution narration,
impact chronology, story or plan identity, or retrospective judgments. Preserve
existing product tests and documents that already state current behavior. Do not
invent product knowledge. Before this closure deletes any spent record,
[preserve its lasting rules](../dough-slice-planning/references/architectural-thinking.md#preserve-lasting-rules-before-deletion).
For North Star topics cited, added, or revised by the completed work, apply the
shared [topic-retirement instructions](../dough-slice-planning/references/architectural-thinking.md#retire-temporary-direction-during-ordinary-wrap-up)
as part of ordinary closure.

## Settle an existing follow-up plan

When a retrospective follow-up exists or Intent Coverage findings need disposition,
read and apply [follow-up disposition](references/follow-up-disposition.md) before
closure: queue the retrospective follow-up first by default (with justified coverage
exceptions there), or drop it only on explicit human instruction.

## Keep test optimization candidates across cleanup

Before the before-cleanup commit, when the spent plan holds test-optimization findings that exist only there, or this closing work is the receiver of a transferred obligation, follow the [candidate record](../dough-test-optimization/references/candidate-record.md).

## Apply product-review decisions

When product advice or additional human input is present, apply only authorized compatible backlog and canonical-home changes, following [dough-product-backlog](../dough-product-backlog/SKILL.md) queue and active-home conventions. Without retrospective advice in the conversation, as in a fresh session, use the plan's recorded product advice; a recorded skip is a skipped review. Explicit human input wins over either advice.

Supported changes: relevant reorder, queue membership, understood new-story addition, understood bounded-correction addition, and canonical-detail edits. Apply the backlog skill's canonical-home admission rules to every new entry. Keep existing follow-up work first under [follow-up priority](references/follow-up-disposition.md#preserve-follow-up-priority-during-reconciliation) unless a justified coverage exception or later explicit human instruction changes that priority. Preserve unrelated content, still-needed acceptance work, and near-future direction.

A skipped, empty, or absent product review, or absent extra human input, introduces no mandatory question. Ordinary closure continues using the other available inputs. Leave unresolved necessary context with active work and report the choice; do not invent scope, launch discovery, or start another review.

## Complete a deferred process review

When continuation carries a selected deferred process review, complete that one
review under
[Deferred process review into wrap-up](../dough-execute-plan/references/deferred-process-review.md)
and
[review process only from a real record](../dough-execution-retrospective/SKILL.md#review-process-only-from-a-real-record)
before the before-cleanup commit. Supply the Intent Coverage report, wrap-up
dispositions, follow-up decision, available run history, elapsed time and
setup/waiting problems. Do not re-enter Select reviews or invent a
wrap-up-only process review.

## Commit closure inputs and preserve Git recovery

After supported follow-up queue changes, any deferred process review that was
due, and before deleting anything, make the current revision recoverable with
this project's ordinary Git conventions. Commit all uncommitted owned review and closure-input changes in that revision: applicable retrospective edits to the
process log in the execution checkout (including deferred process findings),
uncommitted follow-up records (including an explicitly dropped follow-up before
its deletion) and queue edits, assimilated product knowledge, and the spent
material. Records the execution already committed in its
[completion commit](../dough-execute-plan/references/finish-or-stop.md#record-execution-completion)
need no second commit. Preserve unrelated changes and include only files or
portions whose ownership is unambiguous. Resolve ownership of the
intended cleanup targets at this boundary too, before deleting any of them. Then
record that revision as the before-cleanup commit, even when the current
revision was already suitable. If commit conventions, ownership, or recovery
cannot be resolved, leave the material intact, report the gap, and do not claim
closure. In Trunk Mode, publish that commit through [wrap-up closure publication](../dough-execute-plan/references/wrap-up-closure-publication.md) before deleting spent history.

Before deletion, follow the shared [supplier dependency procedure](../dough-product-backlog/references/supplier-dependencies.md)
to retain recoverable completion evidence. Completed selected work then
[takes the current-consumer visit](../dough-product-backlog/references/supplier-dependencies.md#which-completed-work-takes-the-visit)
after accepted integration, through Trunk Mode `finish` or Story Branch
integration below, before final completion and retirement.

## Delete spent history, including shared records

After completion and Git recovery are established — including any required Trunk
Mode before-cleanup publication — delete the selected work's spent history under
[wrap-up cleanup](../dough-product-backlog/references/record-preparation.md#wrap-up-cleanup).
In Story Branch Mode, for selected work with a **Taken** entry, first run
`node '<installed-execute-plan>/scripts/source-freshness.mjs' check` with the
inputs in [source freshness](../dough-execute-plan/references/source-freshness.md#run-the-check);
on `amended` or `conflict` delete nothing and follow
[its wrap-up handling](../dough-execute-plan/references/source-freshness.md#at-wrap-ups-deletion).
Work without a Taken entry has no story section or plan to compare. On
`unchanged`, or for work without a Taken entry, delete:

- its executable plan and owned proof, evidence, and assessment records, even
  when the plan was retained or carries its execution-complete record;
- its canonical story section when one exists, and its seed only when every
  remaining section is spent;
- its **Taken** or **Backlog list** entry when one exists, through the
  [dough-product-backlog](../dough-product-backlog/SKILL.md) `complete` command,
  passing the combined Intent Coverage report only after a returned check was
  disposed; otherwise, when no check was requested or opt-in skipped, complete
  without coverage input. Include agent profile removals, done record, expiry
  removals, and catalog changes; and
- links whose sole purpose is preserving that history.

Also remove follow-up records selected for explicit drop under [follow-up disposition](references/follow-up-disposition.md);
otherwise preserve active follow-ups and their queue entries.

Remove empty directories belonging to the spent work, including untracked ones.
The current snapshot must be free of that history, both tracked and untracked,
with recovery available from the recorded before-cleanup commit.

Preserve process findings, occurrence facts, and unresolved judgments under
[process-finding recording](../dough-execution-retrospective/references/process-finding-recording.md).
Keep findings understandable when removing their spent sources.

Preserve unrelated human text, sibling stories and log entries, product and
version identity, maintained tests and documents, still-needed acceptance work,
and active follow-ups. Shared records lose only spent portions attributable to
the completed work; leave uncertain portions intact and report the ambiguity.

Keep an active follow-up's [provenance](references/follow-up-disposition.md#keep-an-active-follow-ups-provenance).
Repair Markdown links broken by cleanup without recreating spent history.
Repeating wrap-up must recognize already-completed cleanup without duplicating
edits. Missing artifacts alone do not establish completion of another work item.

## Commit final closure

Review and commit the owned closure changes using this project's Git conventions,
preserving unrelated changes. Both the before-cleanup revision and the final
closure must be committed in either execution mode, as
[agent commits](../dough-execute-plan/references/agent-commits.md) when that
reference applies; uncommitted cleanup is not completion. Report unresolved
ownership or commit failures without claiming closure.

Direct-current-branch mode stays in the recorded checkout and creates no worktree. Local-only closure commits there and reports that revision as committed and pending publication; it does not push. Publish-authorized closure uses [wrap-up closure publication](../dough-execute-plan/references/wrap-up-closure-publication.md) from that checkout; the receipt is the accepted SHA and the authorized target. Trunk Mode publishes the final-closure commit through [`finish`](../dough-execute-plan/references/wrap-up-closure-publication.md#finish-trunk-mode-closure); Story Branch Mode continues with integration and resource cleanup below.

## Integrate committed Story Branch Mode closure

Read and apply [Story Branch integration](references/story-branch-integration.md)
to the final-closure tip, passing a dashboard landing context supplied to this
launch, and require its accepted receipt before resource cleanup.

## Remove execution resources safely

Retirement removes the worktree only when its [creation record](../dough-manual-testing/references/exploration-workspace.md#close-or-retain-it)
or another record there shows this work created it; pass Dough Land's [Retire the worktree](../dough-land/SKILL.md#retire-the-worktree) ownership facts.
In Trunk Mode, `finish` retires the worktree and branch, never a remote execution branch, once its completion receipt holds `success` with confirmed shutdown;
act on its `cleanup` result there, and after an interruption rerun it as [Finish Trunk Mode closure](../dough-execute-plan/references/wrap-up-closure-publication.md#finish-trunk-mode-closure)
says. In Story Branch Mode, retire them under [Story Branch retirement](references/story-branch-integration.md#retire-story-branch-resources).
Dough Land's [refresh](../dough-land/SKILL.md#refresh-the-default-checkout) never blocks retirement. Report blocked or partial cleanup without repeating already-completed closure. Skip cleanup in direct-current-branch mode.

## Report

Apply Dough Land's shared [completion attention](../dough-land/SKILL.md#completion-attention)
rule and its [final dashboard operation](../dough-land/references/dashboard-completion.md)
with the supplied launch context, after either closure path has settled. Keep the selected identity, mode and retained execution context, before-cleanup and final-closure commits, accepted publication and completion receipts, remaining coverage, knowledge and cleanup dispositions in their existing operational evidence and lasting homes.

When attention is needed, report the affected facts and next action. Distinguish accepted trunk publication from a superseded candidate, an [`integrated_state` success](../dough-execute-plan/references/ci-completion-wait.md) from the closure's own, integration from refused cleanup, committed pending publication from an accepted receipt, and completed wrap-up from a refusal that left files intact.
Missing context, unfinished work, unresolved recovery/integration, required push, unpublished Trunk Mode closure, retained or unconfirmed observation, resource cleanup, or a [lifecycle commitment](references/lifecycle-commitments.md#report-the-outcome) without its owner's receipt prevents successful closure; name each such commitment and its owner. Local-only current-branch pending publication is not a required push.
