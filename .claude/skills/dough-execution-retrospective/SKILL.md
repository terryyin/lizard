---
name: dough-execution-retrospective
description: >-
  Reviews planned, completed planless quick, or quick-to-planned execution against
  original intent, aggregate commits, current whole-product architecture, and tests,
  including after cleanup. Use for execution retrospective, product review, or backlog
  recommendations from current/supplied history. Supports `--skip-process`, `--skip-product`,
  and project `skipProcessRetrospective` preference. May plan corrections, record process
  findings in `DearDough.md` (500-line warning, 1,000-line ceiling, recoverable lower-priority
  replacement on overflow), and recommend product work; never implements findings.
---

# Review an execution

Review implementation, process, and product learning by default. Return evidence and needed
correction plans; do not implement, commit, push, or change the backlog. Leave closure to
[dough-story-wrap-up](../dough-story-wrap-up/SKILL.md).

## Select reviews

Select reviews before focus-specific actions or context, including log-location resolution.
Independent, combinable `--skip-process` and `--skip-product` omit their analysis, suggestions,
and destination access/writes. Unresolved process selection excludes that focus too. Other
reviews retain their authority, implementation planning, and direction consideration.

Read this project's optional `<established-planning-directory>/open-dough.json` (default:
`<project-root>/.planning/open-dough.json`), not a skill-local or other project's file.
Expect a JSON object with optional boolean `skipProcessRetrospective`: missing file/key or
`false` enables process; `true` skips. Ignore unknown keys; never create, rewrite, or repair it.

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
correction plans and process recording below. A complete correction plan needs no seed.
Quick inputs follow the recovery rules below.

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
  same-story remaining-work plan. Preserve compatible attributable quick work/proof as such,
  not earlier planned slices; recover later slices/changes normally. Both parts form one execution.

Completion requires every planned slice done (history proves deleted plans), or quick
conversation/repository proof of delivered outcome. Quick-to-planned needs a completed remaining
plan plus quick/planned proof covering the original outcome without gaps or assumed repeated work.
Missing kind, continuity, contract, completion, or proof limits dependent conclusions only.
Two equally plausible executions need user selection; continue independently supported review.

Manifest each related SHA with a reason from story/plan, message, diff, or transcript; inspect
nearby/intervening commits to exclude unrelated work. Ambiguous attribution limits claims;
planning-only commits are provenance. Use a net diff only for an uncontaminated range, otherwise
selected patches together and files at the last related implementation commit. Review history
read-only, excluding later work.

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
  original contract/manifest as provenance; current findings as scope/evidence; bounded outcome,
  concepts, impact, preserved behavior, and focused proof. Keep historical promises/attribution intact.
  Changing promises/constraints or findings that cannot form one bounded correction need user decision.

Only the designated writer changes the plan when two authorized reviews cover one execution;
the other returns evidence. Continue enabled reviews; correction refinement/execution needs a separate request.

## Review process only from a real record

Use direction and a sufficient conversation/transcript to identify waste, rule-induced churn,
missing stops, disproved sizing/decomposition, digression, and useful practices. Assess instruction
and context usability, including this review's avoidable rereading, duplication, and reconstruction.
Separate necessary investigation, observation, and inferred cost/cause. Cite recorded token counts
or repeated work with qualified cost; brevity/skipped investigation alone proves no gain. Insufficient
records limit conclusions: invent no events, edit no guidance, require no measurements or recursive
review. Process proposals stay outside correction plans.

### Record supported process findings

Use the explicit user/project `DearDough.md` location or `<project-root>/DearDough.md` for enabled
process findings. No findings means unchanged/no new log. Missing/conflicting root/location stops
recording only: return findings and continue independent reviews without inventing/searching elsewhere.

Reuse logged execution identity, else canonical plan/story plus first related implementation commit,
or a stable execution-record reference if no commit exists. Later commits/reviews/dates create no
identity. Missing/conflicting identity permits findings but no countable row or invented tracking.

For a new log, assign `DD-001` upward in supported-finding order using:

```markdown
# DearDough Process Findings

## DD-001 — <descriptive issue title>
<concise concrete description>

### Occurrences
- Execution: <stable execution identity>
  - Timestamp: <ISO 8601 occurrence time with timezone | unknown>
  - Tool: <Codex, Cursor, Claude Code, or another identified tool>
  - Model: <model identifier, when available>
  - Open Dough release: <version | unknown | unreleased | modified>
  - Evidence: <decisive compact references or locators>
  - Observed effect: <what the record shows>
  - Inference: <qualified cause, cost, or uncertainty, only when needed>
```

Use compact references and separate observation/inference. Rows count retained occurrences,
not all-time recurrence. Record supported one-offs, practices, potential general issues, and
retrospective observations with qualified generality. For each new occurrence:

- **Timestamp:** actual event time, ISO 8601 with timezone, from execution evidence or live clock;
  otherwise `unknown`, with available dates/ranges in Evidence. Never substitute review/import/
  nearby-commit times or invent date precision. Preserve timestamps; fill unknown only with event
  evidence. Older timestamp-free rows remain valid without replay rewrites.
- **Tool/model:** identify the executing tool, not reviewer; omit Model when execution evidence
  supplies none, without guessing or separate lookup. Unidentified tool means no countable row.
  Backfill older rows only with supporting evidence.
- **Release:** execution-time guidance provenance, otherwise `unknown`; not product version or
  today's checkout/installation `VERSION` unless tied to this work. Mark unreleased/modified
  guidance with available revision/base release, e.g. `modified; revision <rev>; base <version>`.
  Never mislabel it a clean release, guess, or backfill older releases; identical rereview stays unchanged.

Existing headings/descriptions/rows must safely identify issues, executions, and next ID.
Use only this log's IDs; preserve DD/adopted ODF codes, mint only `DD-NNN`. Keep notes, evidence,
release rows, and unrelated content; make the smallest supported edit. Migration, normalization,
reordering, deletion, or merging requires bounded retention below.

Match the same concrete problem/practice by decisive evidence, not wording/symptoms; reuse its code.
Recover history only for consequential identity/match questions, reusing established removed IDs.
Missing history that prevents safe identity resolution stops allocation. Otherwise an unmatched/
uncertain finding gets the next unused `DD-NNN`, with matching uncertainty stated when relevant.
Never renumber to fill gaps or restore a pruned occurrence on identical rereview.

Next DD number is one above the greater of all current DD/adopted-ODF heading numbers and
retention metadata's highest allocated local number. Never reuse removed IDs or collide across
prefixes (`ODF-001` reserves 1). Update existing high-water metadata on every allocation, even
without pruning. Uncertain historical gaps cannot be filled; report inability to allocate safely.

One execution means one row: identical/pruned rereview makes no edit; decisive new evidence or
corrected qualified conclusions enrich it, preserving prior notes. New executions add rows, symptoms do not.

For supported writes only, build the complete ordinary candidate, then follow
[bounded process-log recording](references/bounded-process-log.md) for measurement, retention,
warnings, writing/refusal, and recovery. No-findings/identical rereviews load no reference;
skipped/unresolved process accesses neither log nor reference. Ambiguity, malformed content,
unsafe retention, or write failure leaves the log byte-identical; report limitations/findings and continue.

Report path, created IDs/rows or `unchanged`/`not recorded` with reason, and required size/retention
warnings or refusal. Claim no unsuccessful write as success. The final banner requires an evidenced
overlooked request, decision, warning, failed verification, or Jidoka stop still needing user action.

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
execution-boundary evidence. Report enabled process proposals and product advice/reasoned no-change;
unresolved process selection reports its error only. Distinguish evidence from hypotheses and
recommendations from proposals/unresolved choices. End with `## EXECUTION RETROSPECTIVE COMPLETE`.

Only with the evidenced attention need above, append this at the absolute end:

```text
!!!!!!!!!! DEVELOPER ATTENTION REQUIRED !!!!!!!!!!
<the overlooked item, its impact, and the response needed>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
```
