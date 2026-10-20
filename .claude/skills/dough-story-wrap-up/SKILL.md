---
name: dough-story-wrap-up
description: >-
  Closes one completed feature story, bounded retrospective correction or
  context-only planless execution using available execution context. Assimilates
  lasting product knowledge, settles existing follow-ups and authorized product
  decisions, and removes spent plan and history with Git recovery. Reports missing
  or unfinished inputs truthfully. Use to close completed work, delete spent
  records, or continue settled planned execution under Execute Plan's continuation
  rule. With `--process-retrospective`, it also reviews the wrap-up's own process
  and records supported findings in `DearDough.md`.
---

# Story wrap-up

Close one selected feature story, bounded retrospective correction, or contextual
instruction when the coordinator invokes wrap-up explicitly or through Execute
Plan's authorized [planned continuation](../dough-execute-plan/references/finish-or-stop.md#continue-settled-planned-work-into-wrap-up).
That continuation retains the same execution context and publication authority,
including local-only/no-push instructions. Use available execution context and
optional retrospective advice. Leave maintained product knowledge without spent
source or plan history in the current snapshot. Do not invent findings, records,
or another-conversation requirements. With `--process-retrospective`, also [review this wrap-up's process](#review-this-wrap-ups-process).

## Resolve this project's context

Require one selected work identity. Use a canonical story — a feature story, or
a correction's minimal story linked to its plan — when it is that work's active
home. A plan-homed correction uses its plan when it satisfies the correction-input
contract in [planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level);
do not require or create a seed for it. When the source is neither
a story nor a correction plan, use a contextual instruction with retained
execution identity, changes, and proof; do not require or create a story, plan,
or queue entry. Name any missing required field, leave the affected work intact,
and stop before closure. Do not invent conventions, artifacts, another review, or
claim closure.

Resolve from this project, not this skill's location:

- repository root and Git working tree;
- canonical seed location, story identity, heading or stable-anchor conventions
  for a selected feature story;
- for planned work: executable-plan location, status vocabulary, and the
  selected work's plan identity;
- for planless work: the supplied story or instruction, its changes, available
  execution results, and retained execution identity;
- optional retrospective advice when present, including an empty result, and
  the product advice in the plan's [execution-complete record](../dough-execute-plan/references/finish-or-stop.md#record-execution-completion);
- Git commit conventions used to preserve a recoverable revision;
- selected mode, execution checkout and branch, authorized remote and target
  branch, and an originating or default checkout only when one is available;
- the product backlog path when a **Taken** or **Backlog list** entry points
  at the selected work; and
- shared records that name the selected work: its seed when applicable, process
  log (`DearDough.md` unless this project sets another canonical location),
  incoming links, and assessment records.

Before deleting a plan that carries planned-execution identity, retain the
resolved mode, checkout, branch, remote, target, and the execution checkout's
[management context](../dough-land/SKILL.md#retire-the-worktree) in the
coordinator's available execution context for the remaining wrap-up actions and
report. Do not create a parallel registry. Missing identity needed by a later
action stops that action instead of reconstructing or guessing it after plan
deletion.

With supplied dashboard reporting context, read and retain
[dashboard completion](../dough-land/references/dashboard-completion.md), its
command and a surviving working directory now, before either Trunk Mode `finish`
or Story Branch retirement can remove the checkout. Prepare any attention file
outside it; reporting itself remains the final operation.

## Establish execution completion

Judge completion from the selected work and available execution evidence.

- Planned work is complete when every slice is done.
- Work without a plan, including an investigation whose approach stayed unselected, is
  complete when its story or instruction, changes, and execution results show the promised outcome, including evidenced no-change.
- Incomplete implementation leaves the affected active work intact. Report the
  unfinished implementation and stop. Do not delete an unfinished correction
  plan to manufacture a wrap-up.

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

When a retrospective follow-up exists, read and apply
[follow-up disposition](references/follow-up-disposition.md) before closure:
queue it first by default, or drop it only on explicit human instruction.
Do not complete wrap-up with an unlisted follow-up.

## Apply product-review decisions

When product advice or additional human input is present, apply only authorized
compatible backlog and canonical-home changes, following [dough-product-backlog](../dough-product-backlog/SKILL.md)
queue and active-home conventions. Without retrospective advice in the
conversation, as in a fresh session, use the plan's recorded product advice; a
recorded skip is a skipped review. Explicit human input wins over either advice.

Supported changes: relevant reorder, queue membership, understood new-story
addition, understood bounded-correction addition, and canonical-detail
edits. Apply the backlog skill's canonical-home admission rules to every new
entry. Keep existing follow-up work first unless a later explicit human
instruction changes that priority. Preserve unrelated content, still-needed
acceptance work, and near-future direction.

A skipped, empty, or absent product review, or absent extra human input,
introduces no mandatory question. Ordinary closure continues using the other
available inputs. Leave unresolved necessary context with active work and report
the choice; do not invent scope, launch discovery, or start another review.

## Commit closure inputs and preserve Git recovery

After supported follow-up queue changes and before deleting anything, make the
current revision recoverable with this project's ordinary Git conventions.
Commit all uncommitted owned review and closure-input changes in that revision:
applicable retrospective edits to the process log in the execution checkout,
uncommitted follow-up records (including an explicitly dropped follow-up before
its deletion) and queue edits, assimilated product
knowledge, and the spent material. Records the execution already committed in its
[completion commit](../dough-execute-plan/references/finish-or-stop.md#record-execution-completion)
need no second commit. Preserve unrelated changes and include only files or
portions whose ownership is unambiguous. Resolve ownership of the
intended cleanup targets at this boundary too, before deleting any of them. Then
record that revision as the before-cleanup commit, even when the current
revision was already suitable. If commit conventions, ownership, or recovery
cannot be resolved, leave the material intact, report the gap, and do not claim
closure. In Trunk Mode, publish that commit through [wrap-up closure publication](../dough-execute-plan/references/wrap-up-closure-publication.md) before deleting spent history.

Before deletion, follow the shared [supplier dependency procedure](../dough-product-backlog/references/supplier-dependencies.md)
to discover current consumers and retain recoverable completion evidence. In Trunk
Mode, resolve and publish consumer changes after accepted before-cleanup publication
and before `finish` can retire the workspace.

## Delete spent history, including shared records

After completion and Git recovery are established — including any required Trunk
Mode before-cleanup publication — delete the selected work's spent history under
[wrap-up cleanup](../dough-product-backlog/references/record-preparation.md#wrap-up-cleanup):

- its executable plan and owned proof, evidence, and assessment records, even
  when the plan was retained or carries its execution-complete record;
- its canonical story section when one exists, and its seed only when every
  remaining section is spent;
- its **Taken** or **Backlog list** entry when one exists, through the
  [dough-product-backlog](../dough-product-backlog/SKILL.md) `complete` command,
  which also deletes that work's agent profile, writes its done record,
  removes expired done records, and rebuilds the done catalog beside them, all
  owned closure changes; and
- links whose sole purpose is preserving that history.

Also remove the owned follow-up records selected for explicit drop above, after
their Git recovery is established, and their entry with `complete --dropped`.
Otherwise preserve active follow-ups and their queue entries.

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

Keep an active follow-up's queue link, correction input, and provenance. If its
source locator points into deleted predecessor history, replace the locator
with the before-cleanup commit and repository-relative path. Repair Markdown
links broken by cleanup without recreating spent history. Repeating wrap-up
must recognize already-completed cleanup without duplicating edits. Missing
artifacts alone do not establish completion of another work item.

## Review this wrap-up's process

Only with `--process-retrospective`, after spent history is deleted, apply the
shared
[process review of a run](../dough-execution-retrospective/references/process-review-of-a-run.md)
once per wrap-up, with the execution checkout as the write location. Point
evidence in deleted sources at the before-cleanup commit, as an active
follow-up's locator does above. Recorded findings are owned closure changes for
[Commit final closure](#commit-final-closure) and travel with that commit. A
rerun that finds the final closure already committed does not review again.

## Commit final closure

Review and commit the owned closure changes using this project's Git conventions,
preserving unrelated changes. Both the before-cleanup revision and the final
closure must be committed in either execution mode, as
[agent commits](../dough-execute-plan/references/agent-commits.md) when that
reference applies; uncommitted cleanup is not completion. Report unresolved
ownership or commit failures without claiming closure.

Direct-current-branch mode stays in the recorded checkout and creates no
worktree. Local-only closure commits there and reports that revision as
committed and pending publication; it does not push. Publish-authorized
closure uses [wrap-up closure publication](../dough-execute-plan/references/wrap-up-closure-publication.md)
from that checkout; the receipt is the accepted SHA and the authorized target.
Trunk Mode publishes the final-closure commit through [`finish`](../dough-execute-plan/references/wrap-up-closure-publication.md#finish-trunk-mode-closure);
Story Branch Mode continues with integration and resource cleanup below.

## Integrate committed Story Branch Mode closure

Read and apply [Story Branch integration](references/story-branch-integration.md)
to the final-closure tip, passing a dashboard landing context supplied to this
launch, and require its accepted receipt before resource cleanup.

## Remove execution resources safely

Retirement removes the worktree only when its [creation record](../dough-manual-testing/references/exploration-workspace.md#close-or-retain-it)
or another record there shows this work created it; pass Dough Land's [Retire the worktree](../dough-land/SKILL.md#retire-the-worktree) ownership facts.
In Trunk Mode, `finish` retires the worktree and branch, never a remote execution branch, once its completion receipt holds `success` with confirmed shutdown;
act on its `cleanup` result there, and after an interruption rerun it as [Finish Trunk Mode closure](../dough-execute-plan/references/wrap-up-closure-publication.md#finish-trunk-mode-closure)
says. In Story Branch Mode, retire them under that section with `--remote-branch <execution branch>` and `--contained <integrated SHA>`,
with wrap-up's gate: a `success` completion receipt for the integrated SHA whose shutdown is confirmed, and no
active checkout-bound observer still hosting the worktree under [preserve pending local work](../dough-execute-plan/references/maintain-default-checkout.md#preserve-pending-local-work).
Dough Land's [refresh](../dough-land/SKILL.md#refresh-the-default-checkout) never blocks retirement. Report blocked or
partial cleanup without repeating already-completed closure. Skip cleanup in direct-current-branch mode.

## Report

Apply Dough Land's shared [completion attention](../dough-land/SKILL.md#completion-attention)
rule and its [final dashboard operation](../dough-land/references/dashboard-completion.md)
with the supplied launch context, after either closure path has settled. Keep the selected identity, mode and retained execution context,
before-cleanup and final-closure commits, accepted publication and completion
receipts, remaining coverage, knowledge and cleanup dispositions in their
existing operational evidence and lasting homes. Do not produce a success recap
solely to repeat them.

When attention is needed, report the affected facts and next action. Distinguish
accepted trunk publication from a superseded candidate, an [`integrated_state` success](../dough-execute-plan/references/ci-completion-wait.md)
from the closure's own, integration from refused cleanup, committed pending
publication from an accepted receipt, and completed wrap-up from a refusal that
left files intact.
Missing context, unfinished work, unresolved recovery/integration, required
push, unpublished Trunk Mode closure, retained or unconfirmed observation, or
resource cleanup prevents successful closure. Local-only current-branch pending publication is not a required push.
