---
name: dough-product-backlog
description: Maintains and reprioritizes a product backlog list of story references. Use to add, reorder, or complete backlog items. Excludes classroom and workshop exercise backlogs.
---

# Product backlog

## Required context

Before editing, identify from human instructions or repository guidance:

- Repository root and canonical backlog path.
- Canonical seed locations, seed IDs, and heading or stable-anchor conventions.
- Decomposition, refinement, and slice-planning workflows, when needed.
- Commit conventions, if a commit is authorized.

If the backlog or required seeds cannot be identified, ask for the missing
context and stop before editing. If a required workflow is unavailable, stop
that activity and ask for its guidance. Allow removed seeds only for Recently
done entries as described below.

## File layout

- Place **Near-future direction** immediately after the title, the **Backlog
  list** next, and **Recently done** last.
- Use bullet lists. Do not number items. Put the highest-priority backlog item first.
- In the backlog list, include only each exact story title linked to its heading
  or stable anchor, plus its seed ID. Keep story details, estimates, dependencies,
  and status in the story's section within its seed.
- Select stories for the backlog list; do not inventory every candidate or turn
  the list into a roadmap or execution plan.
- Keep the ten most recently completed items in Recently done, newest first;
  keep all if fewer than ten. Use linked titles and seed IDs while seeds exist.
  If a completed story's seed has been removed, keep only its exact title as
  plain text. Use Git history to recover its definition or refinement if needed.

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

## Maintain the backlog list

- Read the backlog, its direction if present, and referenced story sections
  before changing order.
- Follow human priority instructions. Otherwise rank by direction alignment,
  user value, learning value, and genuine product prerequisites. Preserve
  unrelated order. Do not derive priority from seed IDs or order within a seed.
- Keep each story in one canonical section within a seed. Link from related
  documents; do not duplicate story details or list the same story twice.
- Preserve stable anchors when renaming or moving stories. Update incoming links.
- Add only stories with a named beneficiary and evaluable outcome. If either
  is unresolved, use this project's decomposition workflow. Route selected-story
  detail to refinement, then slice planning. Carry the direction into these
  workflows as the primary input for scope decisions.
- Place unfinished prerequisites before dependent stories. If this conflicts
  with explicit human ordering, cite the stories and ask the human to resolve
  the conflict before changing their order. Do not invent technical preparation
  stories.
- Reprioritizing does not authorize execution or cancel other candidates.

## Complete items

- On completion, verify evidence, record completion in the story's section
  within its seed, and move the entry to the top of Recently done. If this creates
  eleven entries, remove only the oldest history entry. Retain the completion
  record in the story's section within its seed.
- If a completed story's seed is later removed, retain its title in Recently done
  without the link or seed ID. Do not remove it before it ages out of the last ten.

## Check and report

- Check exact titles, links, duplicate outcomes, and prerequisite order. For a
  removed completed-story seed, check the retained title against Git history
  when needed; do not leave a broken link.
- Check section order, bullet formatting, and direction alignment when present.
  Confirm the direction is unchanged unless explicitly instructed by a human.
- Check Recently done contains at most ten completions, newest first.
- Summarize changes and reasons briefly.
- Follow commit conventions when authorized. Backlog maintenance alone does not
  authorize a commit or push.
