---
name: dough-story-refinement
description: >-
  Clarifies selected stories before execution planning by establishing goal,
  scope, and key examples in each story's seed, distinguishing promises from
  rejection constraints. Adds UI or architectural
  detail only when needed. `--one-shot` refines a queued story without
  publishing an assignment and keeps the result for review, or lands it with
  `--auto-land`. Use for
  selected-story refinement, not broad problem decomposition, candidate
  selection, or slice sizing.
---

# Story refinement

Build shared understanding of one selected story, or several related stories
whose boundaries need discussion. Record Goal, Scope, and Key examples in each
story's seed.

## Choose the workflow

If the parent problem, candidate selection, or story ordering needs
reconsideration, use
[dough-story-decomposition](../dough-story-decomposition/SKILL.md).
For smaller or clearer slices, use the project's execution-plan
refinement workflow on the existing plan.

Refinement alone does not authorize planning or implementation. When the user
explicitly requests execution planning, hand off one understood story to the
project's planning workflow and continue without repeating answered questions.

## Resolve required context

Identify this project's root, selected story links and seeds,
and relevant prior decisions. When a seed is missing, resolve the canonical
seed directory, ID and filename conventions, required metadata, and stable
story-anchor convention. Resolve project paths from that repository, not this
skill's location.

Resolve the execution-planning workflow only for a requested handoff, and ADR
context only when the architectural concern requires it under the reference
below. If required context or a linked dependency is unavailable, name what is
missing and stop the affected activity. Do not invent project paths or decisions.

## Refine and report

Read and follow [planning scope and lifecycle](references/planning.md) for the
conversation, scope decisions, optional UI and architecture detail, seed updates,
and cleanup after implementation. When your instruction
carries an established preparation, follow
[established preparation](references/established-preparation.md) instead of the
workspace and announcement steps below. When the request explicitly selects
one-shot (`--one-shot`) for a queued story, follow
[one-shot refinement](references/one-shot-refinement.md) instead of the
announcement step and the default disposition. Before writing to a story's seed,
establish or reuse the required workspace under
[preparation workspace](references/preparation-workspace.md), then, for a
queued story, [announce the preparation assignment](references/preparation-assignment.md#announce-the-preparation-assignment);
refinement discussion and clarifying questions need neither on their own. After the
seed or correction-home write records goal, scope, and key examples for a work
item with a known identity, apply
[record preparation facts](../dough-product-backlog/references/record-preparation.md).

When the request includes options such as `--explore`, read
[refinement options](references/refinement-options.json) and apply the selected
options' instructions within this workflow. Options in the same group, an entry
of that file's `groups` list, are exclusive; if a request names more than one of
them, stop and report the conflict, naming the group's `label` and the flags the
request named from it. Without options, refine straightforwardly.

Report the story links, material constraints and deferred promises, and
unresolved decisions. Apply that reference's keep or discard decision, then
close or retain the workspace, when this session ends.
