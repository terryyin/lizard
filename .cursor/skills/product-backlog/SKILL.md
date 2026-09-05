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
- Format: one numbered list, highest priority first. Each entry contains only
  the exact story title linked to its heading or stable anchor, and its home
  seed ID. Keep requirements, estimates, dependencies, and status in the seed.
- This is a selected queue of unfinished stories, not an exhaustive inventory
  of seeds, a milestone roadmap, or an execution plan.

## Maintain the queue

Read the backlog and the referenced story sections before changing order.
Follow the owner's priority instructions; otherwise order by user value,
learning value, and genuine product prerequisites. Preserve unrelated order.
Do not infer that seed number or order within a seed determines global priority.

One story has one canonical home even when its journey crosses several
features. Related seeds may link to it; do not duplicate its requirements or
enqueue the same outcome twice. Preserve stable story anchors when renaming or
moving a story, and update incoming links when needed.

Add only actual stories with a named beneficiary and evaluable outcome. Use
the repository's story-decomposition skill when those are unresolved; use
slice-planning only after a story is selected for implementation. Reprioritizing
does not authorize execution or mean other seed candidates are cancelled.

On completion, verify evidence, record completion in the home seed, and remove
the queue entry. On deferral, remove the entry while retaining the seed story.
If a dependency is unfinished, place it first or explain the ordering conflict
to the owner; do not manufacture technical preparation stories.

Check every link and exact title, duplicate outcomes, and prerequisite order.
Summarize what moved and why. Follow repository commit conventions; backlog
maintenance alone does not authorize a commit or push.
