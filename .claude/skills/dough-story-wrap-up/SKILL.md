---
name: dough-story-wrap-up
description: >-
  Closes one completed feature story, bounded retrospective correction, or
  context-only planless execution using available execution context. Assimilates
  lasting product knowledge, handles existing follow-ups and authorized product
  decisions, removes that work's spent plan and history so Git can recover it,
  and reports truthfully when required inputs are missing or unfinished. Use to
  wrap up a story, correction, or completed contextual instruction, close
  completed work, or delete spent plan and execution history.
---

# Story wrap-up

Close one selected feature story, bounded retrospective correction, or contextual
instruction when the coordinator invokes wrap-up. Use available execution context
and optional retrospective advice. Leave this project with maintained product knowledge
and no spent source or plan history in the current snapshot. Do not invent findings,
records, or a requirement for another conversation.

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

## Queue an existing follow-up plan first

When an existing follow-up plan is present, validate it against the
correction-input contract above, then put its one canonical active home first in
the queue. Do not refine, replan, or execute it. Preserve the plan contents
needed for later execution. Handle that follow-up by its presence, including when
retrospective advice is absent.

Resolve one canonical active home under [dough-product-backlog](../dough-product-backlog/SKILL.md#canonical-active-homes):

- Queue a follow-up's story, which every new correction has, under its
  identity with the plan linked in its seed; do not also queue the plan.
- Queue a plan-homed follow-up through its plan, its canonical active home; do
  not create or recover a seed for it.
- If required correction input is missing, name the missing field and do not
  guess or queue the addition. Keep the follow-up plan and any other needed
  active-work context, report the gap, and stop before deleting the completed
  predecessor's history.

Preserve unrelated queue order after that first item, near-future direction,
and human text. Repeating wrap-up must recognize either canonical home and must
not duplicate the follow-up or queue entry.

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
applicable retrospective edits to the process log in the execution checkout, an
uncommitted active follow-up plan and its queue edit, assimilated product
knowledge, and the spent material. Records the execution already committed in its
[completion commit](../dough-execute-plan/references/finish-or-stop.md#record-execution-completion)
need no second commit. Preserve unrelated changes and include only files or
portions whose ownership is unambiguous. Resolve ownership of the
intended cleanup targets at this boundary too, before deleting any of them. Then
record that revision as the before-cleanup commit, even when the current
revision was already suitable. If commit conventions, ownership, or recovery
cannot be resolved, leave the material intact, report the gap, and do not claim
closure. In Trunk Mode, publish that commit through [wrap-up closure publication](../dough-execute-plan/references/wrap-up-closure-publication.md) before deleting spent history.

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
  which also deletes that work's agent profile, an owned closure change; and
- links whose sole purpose is preserving that history.

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

Save the final-closure tip already published on the remote execution branch.
Follow [Story Branch integration observation](../dough-execute-plan/references/wrap-up-closure-publication.md#observe-story-branch-integration)
for the target transition, publication, and shared completion on the accepted
integrated SHA. Its publication uses [Preserve published history](../dough-execute-plan/references/publish-the-candidate.md#preserve-published-history)
from the owned execution workspace, excluding any default checkout's
unrelated commits and pending human edit.

When the merge touches the product backlog, use the owned workspace's installed
merge adapter as that procedure requires. A stopped result stays as Git left it.
Resolve it through [a real conflict](../dough-product-backlog/references/merge-conflicts.md#a-real-conflict-resolve-by-hand-then-continue-through-the-same-adapter).
If the adapter and that reference are unavailable, report the gap and leave the
conflict. Stop when no coherent resolution is justified. Selected-work cleanup
alone does not prove a sibling backlog change survived.

Require that procedure's accepted receipt before resource cleanup. The receipt
is the accepted candidate SHA and the remote trunk ref. A superseded candidate
is not the receipt. Unresolved integration preserves the execution resources
and blocks completion. Do not force-push.

## Remove execution resources safely

Retirement removes the worktree only when its [creation record](../dough-manual-testing/references/exploration-workspace.md#close-or-retain-it)
or another record there shows this work created it; pass Dough Land's [Retire the worktree](../dough-land/SKILL.md#retire-the-worktree) ownership facts.
In Trunk Mode, `finish` retires the worktree and branch, never a remote execution branch, once its completion receipt confirms shutdown;
act on its `cleanup` result there, and after an interruption rerun it as [Finish Trunk Mode closure](../dough-execute-plan/references/wrap-up-closure-publication.md#finish-trunk-mode-closure)
says. In Story Branch Mode, retire them under that section with `--remote-branch <execution branch>` and `--contained <integrated SHA>`,
with wrap-up's gate: a completion receipt for the integrated SHA whose shutdown is confirmed, and no
active checkout-bound observer still hosting the worktree under [preserve pending local work](../dough-execute-plan/references/maintain-default-checkout.md#preserve-pending-local-work).
Dough Land's [refresh](../dough-land/SKILL.md#refresh-the-default-checkout) never blocks retirement. Report blocked or
partial cleanup without repeating already-completed closure. Skip cleanup in direct-current-branch mode.

## Report

Report selected work and identity, completion judgment, mode and retained
checkout/branch/remote/target, before-cleanup and final-closure commits when
deletion happened, Trunk Mode published closure SHAs, the
completion receipt (CI verdict or exact unresolved reason with shutdown
evidence), remaining CI coverage, assimilated knowledge, deleted paths, Story
Branch saved tip and integration/push results to the authorized trunk target,
each refresh result (including not applicable), worktree and branch cleanup
results (remote deletion only when verified absent), preserved material and
resources, and any gap. Distinguish a new merge from an already-integrated tip,
integration from refused cleanup, an accepted trunk receipt from a superseded
candidate, committed pending publication from an accepted receipt, and completed
wrap-up from a refusal that left files intact. End successful closure with
`## STORY WRAP-UP COMPLETE`. Missing context, unfinished work, unresolved
recovery/integration, required push, unpublished Trunk Mode closure, retained
or unconfirmed observation, or resource cleanup blocks that marker. Local-only
current-branch pending publication is not a required push.
