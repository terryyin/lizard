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
- Canonical executable-plan locations for plan-homed correction entries or
  taken planned stories when they are affected.
- [Work item identity](references/identity.md), which is the single contract
  for what identifies an entry and what only navigates to it.
- [Record preparation facts](references/record-preparation.md), when
  decomposition, refinement, slice planning, plan refinement, execution,
  resume, or wrap-up writes or consumes structured preparation or readiness
  assessment in a canonical home through `record-state` / `read-state`.
- Decomposition, refinement, slice-planning, plan-refinement, execute-plan, and
  wrap-up workflows, when needed.
- Commit conventions, if a commit is authorized.

If the backlog or the canonical home required by an affected entry cannot be
identified, ask for the missing context and stop before editing. Do not require
seed conventions when every affected entry is a plan-homed correction. If a
required workflow is unavailable, stop that activity and ask for its guidance.

## File layout

- Place **Near-future direction** immediately after the title when it exists,
  then **Taken** immediately before **Backlog list**. Retain **Taken** when it is
  empty.
- Use bullet lists. Do not number items. Put the highest-priority queued item
  first. Preserve the order of entries already in **Taken** and append each
  newly taken entry.
- In **Taken** and **Backlog list**, include only each exact work title linked to
  its canonical active home and its recorded identity, as
  [work item identity](references/identity.md) defines them. A plan-homed
  correction links directly to its existing plan. Taken planned stories,
  including correction stories, also link directly to their slice plans. Keep details,
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
- A new bounded retrospective correction's canonical home is its minimal
  story, linked to its plan, under the correction-input contract in
  [planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level).
- A correction whose plan was already its canonical home stays there under its
  recorded identity. Do not migrate it or create a seed to queue it.
- For planned work with a story, queue that story and link its plan there. Do
  not also queue the plan. Treat references to either home as the same work
  when checking repetition and duplicates; `add` and `take` refuse a plan
  listed as separate work.

## Maintain the backlog list

- Read the backlog, its direction if present, and referenced canonical homes
  before changing order.
- Follow human priority instructions. Otherwise rank by direction alignment,
  user value, learning value, and genuine product prerequisites. Preserve
  unrelated order. Do not derive priority from seed IDs or order within a seed.
- Link from related documents; do not duplicate work details or list the same
  story or correction twice within or across **Taken** and **Backlog list**.
- When a story or correction is renamed or moved, update the entry's link and
  carry its recorded identity across unchanged, under
  [work item identity](references/identity.md). Update incoming links.
- Add a feature story only with a named beneficiary and evaluable outcome. If
  either is unresolved, use this project's decomposition workflow and route
  selected-story detail to refinement, then slice planning. Add a bounded
  correction only when its story and plan, or its plan-homed record, satisfy the
  correction-input contract above; otherwise name the missing field and leave
  the story, plan, and queue unchanged. Do
  not use decomposition to fabricate a story. Carry the direction into
  applicable workflows as the primary input for scope decisions.
- Place unfinished prerequisites before dependent work. If this conflicts with
  explicit human ordering, cite the affected entries and ask the human to resolve
  the conflict before changing their order. Do not invent technical preparation
  stories.
- Reprioritizing does not authorize execution or cancel other candidates.

## Take queued work for execution

Invoke the installed `scripts/product-backlog.mjs take` operation only as
execution of the selected authorized plan or explicitly planless quick story
starts. Refinement, planning, intent, or missing execution context or authority
leaves the entry queued. Follow
[execution and resume](references/record-preparation.md#execution-and-resume)
for readiness; Take and resume neither record nor infer it.

Preserve the title, canonical link, and identity. A planned story requires a
resolvable link to its slice plan or a section of it, including on resume.
Quick stories need no plan; plan-homed corrections need no duplicate plan link.

Move the entry to the end of **Taken** once. On resume, do not duplicate or
reorder it. Refuse an absent or ambiguous entry instead of fabricating one.

Leave started work in **Taken** across pauses, failures, resume, completion, and
retrospective; returning cancelled work requires an explicit backlog decision.

## Remove completed items

- Route completed story or correction closure through
  [dough-story-wrap-up](../dough-story-wrap-up/SKILL.md). It removes the entry
  from its active list and owns applicable seed, plan, and proof cleanup.
- Standalone maintenance may remove a completed item from either active list
  when the human asks only for backlog maintenance. The applicable seed, plan,
  and proof remain available for later story wrap-up.

Either path removes the entry with the installed `scripts/product-backlog.mjs
complete` operation. It also deletes the execution agent profile under
`agents/` beside the backlog that names the same identity, releasing that agent
name; commit that deletion with the backlog change. A preparation assignment
profile stays until its own release.

## Direct edits may be denied in Claude Code, Codex, or Cursor

An installed Claude Code project may deny a direct `Edit`/`Write`/
`MultiEdit`/`NotebookEdit` attempt on the resolved product backlog path,
an installed Codex project may deny an `apply_patch` attempt there, and an
installed Cursor project may deny a `Write`/`StrReplace`/`Delete` attempt
there. Each reports the denial before any bytes change. This is expected:
invoke the installed `scripts/product-backlog.mjs` operation instead of a
direct hand-edit. Reads, edits to other files, and shell-run commands (including
shell redirection into the backlog file) are unaffected.

## Merge, rebase, or cherry-pick the backlog across branches

An authorized merge, rebase, or cherry-pick that combines two sides of the
product backlog (often `PRODUCT-BACKLOG.md`) — not an ordinary same-branch
add/take/place/complete — is run through this project's installed product
backlog Git adapters from the start, before Git ever reports a conflict; a
clean Git result can still combine the backlog wrongly. Read and follow
[reconcile product backlog Git operations](references/merge-conflicts.md) for
how to resolve the installed adapters, run the matching operation, resolve a
real conflict, and validate a clean-but-disputed result, including its
fallback for when the adapters are unavailable or do not cover the conflict.
If neither the adapters nor that reference are available, preserve the
conflict and report the missing guidance.

## Check and report

- Check exact titles, links, duplicates across both active lists, and
  prerequisite order in the queue.
- Check section order, bullet formatting, and direction alignment when present.
  Confirm the direction is unchanged unless explicitly instructed by a human.
- Summarize changes and reasons briefly.
- Follow commit conventions when authorized. Backlog maintenance alone does not
  authorize a commit or push.
