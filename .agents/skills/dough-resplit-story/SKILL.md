---
name: dough-resplit-story
description: >-
  Resplits feature stories and maps their slice plans. Use on request, for an
  over-limit plan, or when execution risk calls for closing a delivered outcome.
---

# Resplit story

Resplit within the authorized outcome. During execution, go directly to
[split at a delivered slice boundary](#split-at-a-delivered-slice-boundary).

## Resolve context

Require a feature story, its seed and plan, project record conventions, and
backlog context when queueing applies. Identify delivered work, proof and open
decisions. Stop before writing if context is missing. For a correction,
keep its [canonical home](../dough-story-refinement/references/planning.md#choose-the-planning-level)
and return the decision needed to bound its outcome.

## Resplit before readiness

Resplit an over-limit feature plan in the same preparation. Keep the Preparing
identity on the first outcome and return the replacements' assessments. If a
split is blocked or refinement is deferred, return the reason for `not-ready`.

## Establish smaller stories

Use [dough-story-decomposition](../dough-story-decomposition/SKILL.md) to
decompose the original outcome and order the smaller stories. Supply the
original story's established decisions and learning from refinement; reuse
answered questions. Keep decomposition's seed non-executable and perform the
plan mapping below only after story boundaries and order are resolved.

Split by useful outcomes, not by distributing slices into equal-sized groups.
Account for the original scope across the new stories; surface proposed scope
removal or changed outcomes for human resolution. Preserve unrelated siblings
and stable links. Retain a trace from the original story to its replacements
using this project's lifecycle conventions, so it is not left as competing work
or recorded as delivered merely because it was split.

## Map and realign plans

Map every original slice and its promises to the story it serves. Preserve
completed work, compatible evidence, overrun history, and still-relevant
decisions. Split or realign slices spanning story boundaries; do not duplicate
implementation work or proof ownership. Record genuine dependencies explicitly.

Partition the original plan into one plan per new story using this project's
locations. Reuse the original plan for the first story where the lifecycle
allows; otherwise retain a replacement trace and retire the competing plan.
Link every plan to its canonical story and account for every original slice.

### Prepare the first outcome

[Refine the first story](../dough-story-refinement/SKILL.md) and
[realign its plan](../dough-slice-plan-refinement/SKILL.md). Check proof,
sizing, and dependencies. The first story must deliver its outcome even if
later stories are deferred. Return unresolved decisions as readiness blockers.

Keep every later story's mapped slices as provisional input and mark its plan
`awaiting story refinement — not ready for slice-plan refinement or execution`,
or the project's equivalent status. It resumes through `dough-story-refinement`
and plan realignment; old slice readiness does not lift this hold.

## Split at a delivered slice boundary

Use the last delivered boundary only if it fits the [slice limit](../dough-story-decomposition/references/problem-decomposition.md#limit-slices-per-plan)
and yields a useful outcome with proof independent of the remaining slices.
If the cut needs a human decision or leaves required proof open, return the
blocker before rewriting either story.

- Keep the original Taken identity. Narrow its source to the delivered outcome
  and check its retained proof and obligations before closure.
- Give the remainder a new identity and plan. [Map every slice and promise](#map-and-realign-plans),
  preserve compatible evidence, and resplit an oversized remainder as preparation.
- If the remainder needs the original's integrated outcome, [record a blocking dependency](../dough-product-backlog/SKILL.md#record-a-necessary-blocking-dependency).
  Then assess readiness and queue it first under [backlog ordering](../dough-product-backlog/SKILL.md#maintain-the-backlog-list).
  Record the original identity on the remainder for recovery after cleanup.

Deliver the split sources, plans and backlog changes together.

## Reconsider backlog placement

If the original story is queued, replace its entry through
[dough-product-backlog](../dough-product-backlog/SKILL.md) and rank each
selected replacement under its rules; only the highest-priority one may keep
the original position, when justified. Leave unselected candidates in their
seed and explain placement or deferral. Queue replacements of an unqueued
original only on request.

## Report

Report the replacement story links, original-to-new slice mapping, first plan's
refinement and readiness result, later plans' holds and resumption steps, backlog
placement rationale, and unresolved decisions. Resplitting grants no execution,
commit, or push; an invoking execution delivers its records. End a completed resplit with:

`## STORY RESPLIT`
