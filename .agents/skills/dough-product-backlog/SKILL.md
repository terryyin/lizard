---
name: dough-product-backlog
description: Maintains and reprioritizes a product backlog list of canonical story or bounded-correction references. Use to add, take, reorder, or complete backlog items, or resolve product backlog merge conflicts. Excludes classroom and workshop exercise backlogs.
---

# Product backlog

## Required context

Before editing, identify from human instructions or repository guidance:

- Repository root and canonical backlog path.
- Canonical seed locations, seed IDs, and heading or stable-anchor conventions
  when feature-story entries are affected.
- Canonical executable-plan locations and plan identity conventions for
  bounded-correction entries or taken planned stories when they are affected.
- Decomposition, refinement, and slice-planning workflows, when needed.
- Commit conventions, if a commit is authorized.

If the backlog or the canonical home required by an affected entry cannot be
identified, ask for the missing context and stop before editing. Do not require
seed conventions when every affected entry is a bounded correction plan. If a
required workflow is unavailable, stop that activity and ask for its guidance.

## File layout

- Place **Near-future direction** immediately after the title when it exists,
  then **Taken** immediately before **Backlog list**. Retain **Taken** when it is
  empty.
- Use bullet lists. Do not number items. Put the highest-priority queued item
  first. Preserve the order of entries already in **Taken** and append each
  newly taken entry.
- In **Taken** and **Backlog list**, include only each exact work title linked to
  its canonical active home and its established identity. A feature story uses
  its heading or stable anchor plus seed ID. A bounded correction without a
  supplied story links directly to its existing plan and uses the plan identity;
  the linked path is sufficient when that is this project's identity convention.
  Taken planned stories also link directly to their slice plans. Keep details,
  estimates, dependencies, and status in the canonical home.
- Select work for the backlog list; do not inventory every candidate or turn
  the list into a roadmap or execution plan.

## Near-future direction

- Treat the direction as the short-term vision: focus effort on one customer
  value or goal.
- Use it as the most important input when deciding story scope. Include the
  outcomes needed to advance that value or goal; exclude unrelated expansion.
- Add or change the direction only on an explicit human instruction to do so.
- Otherwise preserve it exactly. If absent, leave it absent; do not infer or
  generate a direction during backlog maintenance.
- Use the direction to guide ordering. Most items should align with it; urgent
  fixes and urgent architecture changes may take priority.

## Canonical active homes

- Keep a feature story in one canonical section within its seed.
- Keep a bounded retrospective correction in its existing plan when no
  canonical story is supplied. Do not create or recover a seed solely to queue
  it. Require the plan to satisfy the correction-input contract in
  [planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level).
- When a canonical story is supplied for planned work, queue that story and link
  its plan there. Do not also queue the plan. Treat references to either home as
  the same work when checking repetition and duplicates.

## Maintain the backlog list

- Read the backlog, its direction if present, and referenced canonical homes
  before changing order.
- Follow human priority instructions. Otherwise rank by direction alignment,
  user value, learning value, and genuine product prerequisites. Preserve
  unrelated order. Do not derive priority from seed IDs or order within a seed.
- Link from related documents; do not duplicate work details or list the same
  story or correction twice within or across **Taken** and **Backlog list**.
- Preserve stable anchors when renaming or moving stories. Update incoming links.
- Add a feature story only with a named beneficiary and evaluable outcome. If
  either is unresolved, use this project's decomposition workflow and route
  selected-story detail to refinement, then slice planning. Add a bounded
  correction only when its plan satisfies the correction-input contract above;
  otherwise name the missing field and leave the plan and queue unchanged. Do
  not use decomposition to fabricate a story. Carry the direction into
  applicable workflows as the primary input for scope decisions.
- Place unfinished prerequisites before dependent work. If this conflicts with
  explicit human ordering, cite the affected entries and ask the human to resolve
  the conflict before changing their order. Do not invent technical preparation
  stories.
- Reprioritizing does not authorize execution or cancel other candidates.

## Take queued work for execution

Move an entry from **Backlog list** to **Taken** only when execution of its
authorized plan or explicitly selected planless quick story is starting.
Refinement, planning, and an intention to execute leave it in the queue. If execution context or authorization fails before execution starts,
leave the entry unchanged.

Preserve the title, canonical link, and identity. Add any missing slice-plan
link for a planned story, including on resume; stop if its plan is unresolved.
Quick stories need no plan, and corrections need no duplicate plan link.

Move the entry to the end of **Taken** in one backlog update. On resume, do not
duplicate or reorder it. Do not fabricate absent entries; stop if an expected
queued entry cannot be moved unambiguously.

Once execution starts, leave the entry in **Taken** across pauses, failures,
resumption, successful plan completion, and retrospective. Returning cancelled
work to the queue requires an explicit backlog-maintenance decision.

## Remove completed items

- Route completed story or correction closure through
  [dough-story-wrap-up](../dough-story-wrap-up/SKILL.md). It removes the entry
  from its active list and owns applicable seed, plan, and proof cleanup.
- Standalone maintenance may remove a completed item from either active list
  when the human asks only for backlog maintenance. The applicable seed, plan,
  and proof remain available for later story wrap-up.

## Resolve Git conflicts

When an authorized merge, rebase, or cherry-pick conflicts in the product
backlog (often `PRODUCT-BACKLOG.md`), read and apply
[backlog merge conflicts](references/merge-conflicts.md) before editing or staging
its resolution. Complete its staged-result verification before continuing the Git
operation. If the reference is unavailable, preserve the conflict and report the
missing guidance.

## Check and report

- Check exact titles, links, duplicates across both active lists, and
  prerequisite order in the queue.
- Check section order, bullet formatting, and direction alignment when present.
  Confirm the direction is unchanged unless explicitly instructed by a human.
- Summarize changes and reasons briefly.
- Follow commit conventions when authorized. Backlog maintenance alone does not
  authorize a commit or push.
