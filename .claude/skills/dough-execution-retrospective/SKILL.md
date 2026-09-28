---
name: dough-execution-retrospective
description: >-
  Reviews planned, completed planless quick, or quick-to-planned execution against
  original intent, aggregate commits, current whole-product architecture, and tests,
  including after cleanup. Process review is opt-in: it runs only when the project sets
  `skipProcessRetrospective: false` or the request asks to include it, and needs available
  agent history. Use for execution retrospective, product review, or backlog
  recommendations from current/supplied history. Supports `--skip-process` and `--skip-product`.
  May plan corrections, record process findings in `DearDough.md` (500-line warning, 1,000-line
  ceiling, recoverable lower-priority replacement on overflow), and recommend product work; never
  implements findings.
---

# Review an execution

Review implementation and product learning by default, and process when enabled. Return evidence
and needed correction plans; do not implement, commit, push, or change the backlog. Leave closure to
[dough-story-wrap-up](../dough-story-wrap-up/SKILL.md).

An execution retrospective may start once implementation is delivered while its
applicable CI result remains pending. Treat the retained observer, publication
target, accepted revision, and pending state as input, not as a missing
completion prerequisite. Review the delivered implementation while observation
continues and state which conclusions remain conditional on unresolved CI. Do
not invoke the CI wait, stop or replace its observer, acknowledge its events, or
claim final execution/review handoff; the invoking execution owns those actions
through its [completion operation](../dough-execute-plan/references/ci-monitor.md#await-the-applicable-revision-at-completion).

## Select reviews

Select reviews before focus-specific actions or context, including log-location resolution.
Independent, combinable `--skip-process` and `--skip-product` omit their analysis, suggestions,
and destination access/writes. Unresolved process selection excludes that focus too. Other
reviews retain their authority, implementation planning, and direction consideration.

Read this project's optional `<established-planning-directory>/open-dough.json` (default:
`<project-root>/.planning/open-dough.json`), not a skill-local or other project's file.
Expect a JSON object with optional boolean `skipProcessRetrospective`: `false` enables process;
missing file/key or `true` skips it like `--skip-process`. Ignore unknown keys; never create,
rewrite, or repair it.

Explicit process selection overrides storage, including errors: `--skip-process` skips;
an include-process request enables without a new flag. Clarify contradictory instructions.
Otherwise unreadable/malformed/non-object JSON or non-boolean recognized values leave selection
unresolved: report error, omit process analysis/log access, and continue independent reviews.

## Resolve this project's context

Start from a capability/story phrase, correction plan, commit, or current/supplied conversation.
Resolve project navigation, focused tests, cleanup lifecycle, relevant plan/story locations and
statuses, and direction; backlog conventions only for dependent product recommendations.
Missing decision-relevant context stops that path with a named gap, not invented conventions.
Preserve worktree changes. Separate report artifacts require a request; allowed writes are
correction plans, a new correction's minimal story, and process recording below.
Quick inputs follow the recovery rules below.

### Write only in an owned checkout

Make every allowed write in one owned checkout. When an invoking execution supplies its execution
checkout as the write location, write there; that execution commits those records in its
completion commit. Otherwise, immediately before the first write, select or reuse an owned workspace under
[prepare records in an owned workspace](../dough-story-refinement/references/preparation-workspace.md),
write there, and report the written result's pending disposition. Do not write in the checkout the review
started from unless it is that supplied execution checkout. A review that writes nothing creates no
workspace. Resolve the process-log path and correction story and plan locations in the chosen checkout.

Before residue assessment, read [refactoring](../dough-post-change-refactor/SKILL.md) and its
checks; use the smell definitions on the aggregate result without editing. For needed correction
planning, load [slice planning](../dough-slice-planning/SKILL.md#require-understood-planning-input)
and follow its bounded-input, proof, sizing, and destination gates. Load [backlog guidance](../dough-product-backlog/SKILL.md)
only for dependent enabled recommendations.

## Recover one execution

Search conversation, supplied history, current planning, then Git history. Establish whether
work was planned, explicitly planless, quick-to-planned, or planned with normal cleanup.
File absence establishes none of these. Recover original intent before judging implementation:

- **Planned:** recover the earliest execution-ready plan, story/correction input and outcome,
  then later changes supported by approval or new evidence. Partial references and removed
  plans suffice when history identifies them; removed plans remain planned executions.
- **Quick:** require conversational evidence of explicit planless selection. Recover canonical
  goal, boundaries, examples, promised proof, approved changes, related changes/commits, and
  available proof. Use current chat when sufficient, otherwise supplied transcript; invent no
  historical plan or substitute execution record.
- **Quick-to-planned:** recover conversational quick selection/attempt and the ordinary
  same-source remaining-work plan. Preserve compatible attributable quick work/proof as such,
  not earlier planned slices; recover later slices/changes normally. Both parts form one execution.

Completion requires every planned slice done (history proves deleted plans), or quick
conversation/repository proof of delivered outcome. Quick-to-planned needs a completed remaining
plan plus quick/planned proof covering the original outcome without gaps or assumed repeated work.
Missing kind, continuity, contract, completion, or proof limits dependent conclusions only.
Two equally plausible executions need user selection; continue independently supported review.
Pending CI alone does not make delivered implementation incomplete. Preserve its
explicit pending state and do not convert it into passing validation.

Manifest each related SHA with a reason from story/plan, retained published revisions, message, diff, or transcript. When those published revisions exist, they are the related set; do not attribute a whole-trunk range, interleaved sibling work, or a rewritten unpublished SHA (use its published replacement).
Inspect nearby/intervening commits and exclude unrelated work with reasons.
Ambiguous attribution limits claims; planning-only commits are provenance.
Use a net diff only for an uncontaminated range, otherwise selected patches together and files at the last related implementation commit.
Review history read-only, excluding later work.

## Consider near-future direction

Read direction once, prioritizing alignment/digression questions and explaining exceptions (e.g.
urgent fixes). If absent, report unassessed alignment and continue; never invent, propose, or edit it.
Route defects to corrections, process learning to findings, and product learning/priorities to advice.
Users own constraint/outcome changes. Historical contracts stand; later direction governs future work.

## Review the outcome

Compare original intent, boundaries, approved changes, and promised proof to aggregate code,
tests, docs, and proof. Assess only completed slices of unfinished plans; unexecuted work is not
missing behavior, nor its temporary predecessor obsolete. Findings need evidence and plausible
impact: bugs, regressions, drift/disputes, refactoring residue, architecture, or test coverage/cost.

Assess whole-product domain responsibilities, dependencies, and representations, including relevant
untouched code. Check coherent rules versus successive-example cases and explain impact, such as
divergent rules or coordinated edits. Delivery sequence cannot justify boundaries; helpers alone
cannot prove cohesion. Apply [ADR awareness](../dough-adr-awareness/SKILL.md) with human-owned conflicts.
Older weaknesses may need correction; only provenance establishes an execution regression.

Identify whether E2E tests drove development, then assess the whole suite, including older tests
and executions adding no E2E tests, under [behavioral test guidance](../dough-post-change-refactor/references/refactor-checks.md#tests-as-behavioral-documentation).
Ground retention, detail downgrades, and consolidation in coverage/cost; preserve journey
information and integration proof. Use project testing guidance or the shared black-box fallback.
Whole-suite assessment needs no full run; use focused read-only checks to confirm/dismiss findings.

Inspect worked-around/replaced branches, flags, callers, fixtures, compatibility paths, overlapping/
obsolete-internal tests, and historical documentation. Removed temporary behavior warrants negative
proof/docs only when absence remains required. Exclude cosmetic/speculative/unsupported findings,
duplicate symptoms, and approved decisions mislabelled as drift. Before planning removal of a
disputed restriction, use [plan-conflict handoff](../dough-execute-plan/references/execution-decisions.md#resolve-a-disputed-plan-restriction).
Preserve genuine constraints; await human resolution of disputed corrections while independent
review continues. Plan compliance alone does not settle justification.

## Reconcile findings with current truth

Recheck findings against current revision/worktree; report later fixes and deduplicate by root
cause. Current evidence bounds corrections, including beyond the old footprint, while preserving
[scope and promises](../dough-story-refinement/references/planning.md#examples-and-constraints).
No findings means unchanged plans; broader review grants no new feature promises.

Give suite cleanup explicit correction ownership, including older redundant tests. For consolidation,
name surviving meaningful coverage/integration proof (retained E2E may suffice). For detail downgrades,
require replacement unit coverage before narrowing/removing E2E tests. Plan cleanup; do not perform it.

- **Unfinished plan:** amend in place; retain completed evidence/resume history and numbering.
  Put corrections before remaining work, revise overlapping slices instead of duplicating them,
  and record findings/manifest as concise learning when the project format supports it.
- **Planless, completion unresolved:** return evidence and exact completion/proof/attribution gaps.
  Original unfinished work is not yet a completed-execution correction; reconstruct no plan or
  create a correction destination until the completed execution boundary is established.
- **Completed execution:** create one follow-up through slice planning in the established location:
  its minimal story in a suitable seed, linked to the plan under the
  [correction input](../dough-story-refinement/references/planning.md#choose-the-planning-level);
  original contract/manifest as provenance; current findings as scope/evidence; bounded outcome,
  concepts, impact, preserved behavior, and focused proof. Keep historical promises/attribution intact.
  Changing promises/constraints or findings that cannot form one bounded correction need user decision.

Only the designated writer changes the plan when two authorized reviews cover one execution;
the other returns evidence. Continue enabled reviews; writing the follow-up neither queues nor
authorizes it; a separate request executes the correction, and [dough-execute-plan](../dough-execute-plan/SKILL.md) publishes it through [increment and repair publication](../dough-execute-plan/references/trunk-publication.md#publish-an-execution-increment-or-repair). This review does not publish.

## Review process only from a real record

Review the coordinator's existing conversation and tool results. Inspect relevant
sub-agent history through available host access when needed. If required history
is unavailable, report the affected process review unavailable and continue
independently supported reviews. Persist supported findings under the recording
rules below; execution state follows the project's plan and Git conventions.

Use that history and the user's direction to identify waste, rule-induced churn,
missing stops, disproved sizing/decomposition, digression, and useful practices. Assess instruction
and context usability, including this review's avoidable rereading, duplication, and reconstruction.
Separate necessary investigation, observation, and inferred cost/cause. Cite recorded token counts
or repeated work with qualified cost; brevity/skipped investigation alone proves no gain. Insufficient
records limit conclusions: invent no events, edit no guidance, require no measurements or recursive
review. Process proposals stay outside correction plans.

### Record supported process findings

When enabled process review produces supported findings, read and follow
[process-finding recording](references/process-finding-recording.md). No findings
or an identified identical rereview means no log change or reference load;
skipped or unresolved process review does not access the log or its recording
references.

## Review product learning

Inspect direction and relevant queue entries/stories; unrelated urgent fixes need no queue survey.
Preserve priorities; connect supported learning to recommendations or reasoned no-change. Unread
entries stay unvalidated; inspirations are hypotheses. Unknown beneficiaries/outcomes warrant an
exploration proposal, not discovery/decomposition. Missing conventions leave conclusions provisional:
report gaps, invent no files/learning, and continue independent reviews.

Send recommendations, proposals, and unresolved choices to [story wrap-up](../dough-story-wrap-up/SKILL.md);
backlog/story edits require that workflow or separately requested [maintenance](../dough-product-backlog/SKILL.md).
Leave disputed goals/scope, conflicting priorities, and unknown beneficiaries/outcomes unresolved.

## Report

Report work identity/completion, provenance, manifest/boundary, impact-ordered findings or `none`,
and evidence limits. Classify planning as amended, new, read-only, unchanged, or unavailable pending
execution-boundary evidence, with a new follow-up's story and plan links. Report enabled
process proposals and product advice/reasoned no-change; unresolved process selection reports
its error only. Distinguish evidence from hypotheses and
recommendations from proposals/unresolved choices. When CI is pending, report its retained target
and revision as pending and make no success claim. End with
`## EXECUTION RETROSPECTIVE COMPLETE`; this marker completes review, not the
execution's final CI handoff. When this review was invoked automatically by an
active `dough-execute-plan` execution, do not end the turn at this marker:
return immediately to that execution's completion operation,
without asking for confirmation. If an authorized CI repair later changes
reviewed code, resume only the affected conclusions using the retained review
state.

Only with the evidenced attention need above, append this at the absolute end
of a standalone retrospective report. For an automatic execute-plan review,
return the attention need to the invoking execution for its final handoff:

```text
!!!!!!!!!! DEVELOPER ATTENTION REQUIRED !!!!!!!!!!
<the overlooked item, its impact, and the response needed>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
```
