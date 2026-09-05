---
name: product-backlog
description: Organize, update, and reprioritize Lizard's ordered product backlog of story references across seeds. Use for backlog ordering and maintenance; story details belong in their home seeds.
---

# Product backlog

## Product settings

- Repository: the Lizard checkout containing this skill (three directories
  above this skill directory). Resolve the following paths from that root.
- Canonical backlog: `.planning/PRODUCT-BACKLOG.md` in that repository.
- Story homes: `.planning/seeds/SEED-NNN-*.md`.
- Required file layout: a short **Near-future direction** section immediately
  after the title, followed by the unfinished story queue, and a **Recently
  done** section at the bottom.
- The direction section states the general product goal for the near future.
  Most backlog items should align with that direction and goal; urgent fixes
  and urgent architecture changes are exceptions and may take priority.
- Queue format: one numbered list, highest priority first. Each entry contains
  only the exact story title linked to its heading or stable anchor, and its
  home seed ID. Keep requirements, estimates, dependencies, and status in the seed.
- The queue is a selection of unfinished stories, not an exhaustive inventory
  of seeds, a milestone roadmap, or an execution plan.
- Recently done lists the ten most recently completed items, newest first
  (or all completed items if fewer than ten). Use the same linked title and
  seed ID format as the queue; completion details stay in the home seed.

## Maintain the queue

Read the backlog, including its near-future direction, and the referenced story
sections before changing order. Follow the owner's priority instructions;
otherwise order by alignment with the direction, user value, learning value,
and genuine product prerequisites, allowing the urgent exceptions above.
Preserve unrelated order.
Do not infer that seed number or order within a seed determines global priority.

One story has one canonical home even when its journey crosses several
features. Related seeds may link to it; do not duplicate its requirements or
enqueue the same outcome twice. Preserve stable story anchors when renaming or
moving a story, and update incoming links when needed.

Add only actual stories with a named beneficiary and evaluable outcome. Use
the repository's story-decomposition skill when those are unresolved; use
story-refinement for selected-story detail, then slice-planning. Reprioritizing
does not authorize execution or mean other seed candidates are cancelled.

On completion, verify evidence, record completion in the home seed, and move
the queue entry to the top of Recently done. Keep at most ten entries: when a
new completion makes eleven, drop the oldest entry at the bottom, retaining
its completion record in the seed. On deferral, remove the queue entry while
retaining the seed story; deferred items do not belong in Recently done.
If a dependency is unfinished, place it first or explain the ordering conflict
to the owner; do not manufacture technical preparation stories.

Check every link and exact title, duplicate outcomes, and prerequisite order.
Check that the direction section comes first, most queued items align with it,
and Recently done comes last with at most ten entries in newest-first order.
Summarize what moved and why. Follow repository commit conventions; backlog
maintenance alone does not authorize a commit or push.
