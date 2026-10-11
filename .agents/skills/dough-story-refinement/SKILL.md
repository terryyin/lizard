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

Report each selected story under
[report the refinement outcome](#report-the-refinement-outcome) below.
Apply the [preparation disposition](references/preparation-disposition.md)
keep or discard decision, then close or retain the workspace, when this
session ends.

## Report the refinement outcome

End each selected story with exactly one outcome. When several stories are
refined together, judge each independently: a clear story keeps its result, and
another story's open decision stays with that story.

- **Ready for slice planning** — goal, scope, and key examples are recorded, no
  decision you need from a person remains, and the work needs planned execution.
- **Flawless — ready for execution** — the same, and the story fits one
  planless slice under
  [slice sizing](../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices),
  with no decisive premise, as
  [slice planning](../dough-slice-planning/SKILL.md#write-the-plan) defines
  it, left unobserved and no probe needed.
- **Needs human engagement** — at least one response from a person is required.

A ready outcome states the outcome, the story link, where the draft is (its
workspace, and its result commit once committed), and one concrete next step:
slice planning in that workspace, or, for Flawless, execution with an
explicit instruction to skip slice planning. When the story already has an
associated plan, the next step uses that plan instead of creating another.
Name the pending draft and any
Preparing assignment as information, not as a request to keep them. Do not
recap what the seed records, and end without a question or approval request.

Needs human engagement lists each expected response separately: what must be
answered or decided, who decides, your recommended answer when you have one,
and what continues once it is given. It covers an open goal, scope, or
constraint decision; a boundary change such as splitting, merging, or dropping
the story; missing required context; and a stopped write, recording, or
landing. Claim no readiness. When required context is missing before the seed
write, name the missing input and who can supply it, say which activity resumes
afterwards, and do not claim that refinement was recorded.

The outcome is reported, not recorded. Recording still follows
[record preparation facts](../dough-product-backlog/references/record-preparation.md):
`refined`, keeping an existing selected approach and plan association,
otherwise an unselected approach. Flawless grants no
[planless authority](../dough-product-backlog/references/record-preparation.md#planless-authority);
the developer supplies it by running the skip-planning next step. Reporting an
outcome neither creates nor renews `ready` and never selects `planless`;
[assess readiness at preparation completion](../dough-product-backlog/references/record-preparation.md#assess-readiness-at-preparation-completion)
still owns the assessment of the current story and any plan.
