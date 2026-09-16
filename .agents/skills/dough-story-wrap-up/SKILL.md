---
name: dough-story-wrap-up
description: >-
  Closes one completed feature story or bounded retrospective correction using
  available execution context. Assimilates lasting product knowledge, handles
  existing follow-ups and authorized product decisions, removes that work's spent
  plan and history so Git can recover it, and reports truthfully when required
  inputs are missing or unfinished. Use to wrap up a story or correction, close
  completed work, or delete spent plan and execution history.
---

# Story wrap-up

Close one selected feature story or bounded retrospective correction when the
coordinator invokes wrap-up. Use available execution context and optional
retrospective advice. Leave this project with maintained product knowledge and
no spent source or plan history in the current snapshot. Do not invent
findings, records, or a requirement for another conversation.

## Resolve this project's context

Require one selected work identity. Use a canonical feature story only when it
is explicitly supplied as that work's active home. Otherwise use the bounded
correction plan itself when it satisfies the correction-input contract defined by
[planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level).
Do not require or create a seed for that correction. Name any missing required
field, leave the affected work intact, and stop before closure.

Resolve from this project, not this skill's location:

- repository root and Git working tree;
- canonical seed location, story identity, heading or stable-anchor conventions
  for a selected feature story;
- for planned work: executable-plan location, status vocabulary, and the
  selected work's plan identity;
- for planless feature work: the story, its changes, and available execution
  results;
- optional retrospective advice when it is present, including an empty
  result;
- Git commit conventions used to preserve a recoverable revision;
- for planned execution, its selected mode and available originating checkout,
  execution checkout and branch, and integration-target identity;
- the product backlog path when a **Taken** or **Backlog list** entry points
  at the selected work; and
- shared records that name the selected work: its seed when applicable, process
  log (`DearDough.md` unless this project sets another canonical location),
  incoming links, and assessment or recognition records.

Planless feature work needs no plan; corrections retain the plan-based identity
contract. Missing closure context leaves affected material intact: report the gap
without inventing conventions, artifacts, or another review, or claiming closure.

Before deleting a plan that carries planned-execution identity, retain the
resolved mode and checkout, branch, and target values in the coordinator's
available execution context for the remaining wrap-up actions and report. Do
not create a parallel registry. Missing identity needed by a later action stops
that action instead of reconstructing or guessing it after plan deletion.

## Establish execution completion

Judge completion from the selected work and available execution evidence.

- Planned work is complete when every slice is done.
- Planless feature work is complete when the supplied story, changes, and
  execution results show the promised outcome is delivered.
- Incomplete implementation leaves the affected active work intact. Report
  the unfinished implementation and stop. Do not delete an unfinished
  correction plan to manufacture a wrap-up.

Apply available retrospective advice under existing authority. Absent/empty advice
still permits ordinary closure from work context, implementation results, maintained
knowledge, existing follow-ups, and coordinator instructions.

## Assimilate lasting knowledge

Move lasting behavior and design into this project's maintained code, tests,
documentation, or current Accepted decisions. Describe the current product
without execution narration, impact chronology, story or plan identity, or
retrospective judgments. Preserve existing product tests and documents that
already state current behavior. Do not invent product knowledge. When a current
product fact is written only in spent execution context or review and is not
already stated in maintained documentation, write it into maintained
documentation before deleting that spent copy. Tests that exercise related
behavior do not replace that documentation step.

For North Star topics cited, added, or revised by the completed work, apply the
shared [topic-retirement
instructions](../dough-slice-planning/references/architectural-thinking.md#retire-temporary-direction-during-ordinary-wrap-up)
as part of ordinary closure.

## Queue an existing follow-up plan first

When an existing follow-up plan is present, validate it against the
correction-input contract above, then put its one canonical active home first
in the queue. Do not refine, replan, or execute it. Preserve the plan contents
needed for later execution. Handle that follow-up by its presence, including
when retrospective advice is absent.

Resolve one canonical active home under
[dough-product-backlog](../dough-product-backlog/SKILL.md#canonical-active-homes):

- If a canonical follow-up story is supplied, keep that story in its seed and
  link the plan there. Queue the story; do not duplicate it as a plan entry.
- Otherwise queue the existing correction plan directly. The plan is its
  canonical active home; do not create or recover a seed solely for queueing.
- If required correction input is missing, name the missing field and do not
  guess or queue the addition. Keep the follow-up plan and any other needed
  active-work context, report the gap, and stop before deleting the completed
  predecessor's history.

Preserve unrelated queue order after that first item, near-future direction,
and human text. Repeating wrap-up must recognize either canonical home and must
not duplicate the follow-up or queue entry.

## Apply product-review decisions

When retrospective product advice or additional human input is present, apply
only authorized compatible backlog and canonical-home changes. Follow
[dough-product-backlog](../dough-product-backlog/SKILL.md) for queue and
active-home conventions. Explicit human input wins over advice.

Supported changes: relevant reorder, queue membership, understood new-story
addition, understood bounded-correction plan addition, and canonical-detail
edits. Apply the backlog skill's canonical-home admission rules to every new
entry. Keep existing follow-up work first unless a later explicit human
instruction changes that priority. Preserve unrelated content, still-needed
acceptance work, and near-future direction.

A skipped, empty, or absent product review, or absent extra human input,
introduces no mandatory question. Ordinary closure continues using the other
available inputs. Leave unresolved necessary context with active work and
report the choice; do not invent scope, launch discovery, or start another
review.

## Commit closure inputs and preserve Git recovery

After supported follow-up queue changes and before deleting anything, make the
current revision recoverable with this project's ordinary Git conventions.
Commit all owned review and closure-input changes in that revision, including
applicable retrospective edits to the process log, an uncommitted active
follow-up plan and its queue edit, assimilated product knowledge, and the spent
material. Preserve unrelated changes and include only files or portions whose
ownership is unambiguous. Resolve ownership of the intended cleanup targets at
this boundary too, before deleting any of them. Then record that revision as
the before-cleanup commit, even when the current revision was already suitable.
If commit conventions, ownership, or recovery cannot be resolved, leave the
material intact, report the gap, and do not claim closure.

Preserve Git history as the sole recovery surface for spent execution material.
Keep current product knowledge in maintained project content.

## Delete spent history, including shared records

After completion and Git recovery are established, delete the selected work's
spent history:

- its executable plan and owned proof, evidence, and assessment records, even
  when the plan was retained at execution completion;
- its canonical story section, and its seed only when every remaining section
  is spent;
- its **Taken** or **Backlog list** entry;
- its process-log occurrences, and issues or containers left empty; and
- links whose sole purpose is preserving that history.

Remove empty directories belonging to the spent work, including untracked
ones. The current snapshot must be free of that history, both tracked and
untracked, with recovery available from the recorded before-cleanup commit.

Preserve unrelated human text, sibling stories and log entries, product and
version identity, maintained tests and documents, still-needed acceptance work,
and active follow-ups. Shared records lose only the portions attributable to
the completed work; leave uncertain portions intact and report the ambiguity.

Keep an active follow-up's queue link, correction input, and provenance. If its
source locator points into deleted predecessor history, replace the locator
with the before-cleanup commit and repository-relative path. Repair Markdown
links broken by cleanup without recreating spent history.

Repeating wrap-up must recognize already-completed cleanup without duplicating
edits. Missing artifacts alone do not establish completion of another work item.

## Commit final closure

Review and commit the owned closure changes using this project's Git conventions,
preserving unrelated changes. Both the before-cleanup revision and the final
closure must be committed in either execution mode; uncommitted cleanup is not
completion. Report unresolved ownership or commit failures without claiming
closure.

Direct-current-branch mode ends with committed closure. Story Branch Mode continues
with integration and resource cleanup below.

## Integrate committed Story Branch Mode closure

Save the committed final-closure tip and integrate it into the recorded target
from its checkout using the retained identity and this project's local merge
conventions. Preserve unrelated target work; unresolved identity or unsafe integration
leaves execution resources intact with the blocker reported. Do not rebase or wait for CI.

If integration reports conflicts, inspect the unmerged paths for this project's
product backlog (often `PRODUCT-BACKLOG.md`). Before editing or staging its
resolution, read and apply
[backlog merge conflicts](../dough-product-backlog/references/merge-conflicts.md)
from the integration checkout's installed guidance. If that reference is
unavailable, leave the backlog conflict unresolved and report the missing guidance.
Complete its staged-result verification before committing the merge; selected-work
cleanup and final-tip ancestry alone do not prove sibling backlog changes survived.
Resolve other conflicts from both sides' intended behavior, surrounding code,
history, and available work context. Verify with appropriate checks and complete
the merge. When evidence cannot justify a coherent resolution, stop and preserve
the conflict for a human decision; report the incompatible intentions or missing
decision, conflicted paths, and Git state.

Integration requires the saved final-closure tip to be an ancestor of the recorded
target containing the committed closure. Recognize an integrated tip without merging again.
Unresolved integration preserves the execution branch and worktree and blocks completion.

When the recorded target is `main`, push the integrated target to `origin`
with `git push origin main` from the target checkout after verifying integration.
This also applies to an already-integrated tip on retry. Require a successful
push before resource cleanup or claiming completion. If the push fails, retain
the execution resources and report the push failure separately from successful
local integration; do not force-push.

## Remove integrated execution resources safely

After verified integration and any required target push, remove the clean
execution worktree and its local and remote branches. Use the retained execution
identity and non-force operations; preserve unrelated resources and any unique
work. Delete the remote branch only when its tip is integrated in the remote
target. Verify removal, accept already-absent resources on retry, and report
any blocked or partial cleanup. Direct-current-branch mode needs no cleanup.

## Report

Report the selected work and its canonical identity, completion judgment,
execution mode and retained checkout/branch/target identity when applicable,
before-cleanup and final-closure commits when deletion happened, assimilated
knowledge, deleted paths, the saved execution tip and local integration result
in Story Branch Mode, the push result when the target is `main`, worktree and
local- and remote-branch cleanup results, preserved
unsupported material and resources, and any gap that blocked closure.
Distinguish a new merge from an already-integrated tip, integration success from
partial or refused cleanup, and local integration from a successful push to
`origin`. Report remote deletion only when its absence has been verified.
Distinguish a completed wrap-up from a refusal that left files intact.

End successful closure with `## STORY WRAP-UP COMPLETE`. Missing context,
unfinished work, unresolved recovery/integration, required push, or resource
cleanup blocks that marker.
