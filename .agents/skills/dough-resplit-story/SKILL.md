---
name: dough-resplit-story
description: >-
  Resplits a large story after slice-plan refinement into smaller stories and
  mapped slice plans. Use when resplitting is requested, including after a
  more-than-15-slices recommendation. Leaves the first story and plan refined;
  later plans await story refinement and backlog priorities are reconsidered.
---

# Resplit story

Replace one large story with smaller stories and partition its refined slice
plan to match. A recommendation from slice-plan refinement alone does not
authorize this workflow. Do not implement product code.

## Resolve context

Require the original story and seed, its refined slice plan, and this project's
seed, anchor, plan-location, and lifecycle conventions. Identify
completed work, evidence, current decisions, and any existing backlog entry.
Resolve backlog context when the original is queued or queueing is requested.
If required context or a linked skill is unavailable, name what is missing and
stop the affected activity before writing.

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

For the first story in the resolved order:

- Use [dough-story-refinement](../dough-story-refinement/SKILL.md) to establish
  its Goal, Scope, and Key examples, reusing compatible prior refinement.
- Realign its plan to that understanding and apply
  [dough-slice-plan-refinement](../dough-slice-plan-refinement/SKILL.md).
  Resolve scope, proof ownership, sizing, and dependencies on deferred work
  before reporting it as a refined plan. It must deliver its own outcome if
  later stories are deferred.
- Leave the story and plan as refined as if refined individually. If unresolved
  decisions prevent that result, report the resplit as incomplete rather than
  giving the first plan inherited readiness.

For every later story, retain the mapped slices as provisional planning input.
Mark its plan explicitly: `awaiting story refinement — not ready for slice-plan
refinement or execution`, using equivalent project status fields where supplied.
Record that resumption requires `dough-story-refinement` to clarify the mapped
story's goal, scope, and examples, followed by plan realignment before slice-plan
refinement or execution. Old slice readiness does not override this hold.

## Reconsider backlog placement

If the original story is queued, use
[dough-product-backlog](../dough-product-backlog/SKILL.md) to replace its entry
and reassess the selected smaller stories against the whole backlog. The
highest-priority replacement may retain the original position when justified.
Other replacements do not inherit that position or automatically follow it;
apply the backlog skill's ranking rules individually. Leave unselected
candidates in their seed and explain placement or deferral. If the original
was not queued, do not queue replacements unless requested.

## Report

Report the replacement story links, original-to-new slice mapping, first plan's
refinement and readiness result, later plans' holds and resumption steps, backlog
placement rationale, and unresolved decisions. Resplitting does not authorize
execution, commit, or push. End a completed resplit with:

`## STORY RESPLIT`
