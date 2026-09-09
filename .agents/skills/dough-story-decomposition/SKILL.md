---
name: dough-story-decomposition
description: >-
  Challenges and decomposes a broad product problem into ordered
  Valuable/Visible/Vertical stories with rough effort hypotheses. Use when the
  parent problem, candidate stories, or learning priority are unresolved.
  Writes one non-executable seed.
---

# Story decomposition

Produce one human-reviewed seed containing a clear parent problem and ordered
candidate stories. Do not inspect implementation, perform technical design, or
write an executable plan.

## Choose the workflow

Use this skill when the beneficiary, problem, desired effect, or value is
unclear; a request prescribes a solution without establishing why; several
outcomes or story boundaries must be chosen; or the first increment's value or
learning priority is disputed.

For selected-story goal, scope, and examples, use
[dough-story-refinement](../dough-story-refinement/SKILL.md). Hand off one
understood story to the project's execution-planning workflow only when the
user explicitly requests planning. A seed or backlog selection does not
authorize execution.

## Resolve required context

Read the user's instructions and repository guidance to identify:

- The repository root, supplied seed or canonical seed directory, seed ID
  allocation and filename conventions, and stable story-anchor convention.
- Required seed metadata and lifecycle vocabulary.
- The project's definitions of S, M, and L effort bands.
- The canonical backlog path, only when queueing or reprioritizing is requested.
- The execution-planning workflow, only when that handoff is requested.

Resolve project paths from this project, not this skill's location.
If context or a linked dependency needed for the current action is unavailable,
name it, ask only for the missing information, and stop the affected activity
before writing.

## Decompose and record

Read and follow [problem decomposition](references/problem-decomposition.md)
to establish human-owned framing decisions, challenge alternatives, and select,
size, and order stories. Then use [seed format](references/seed-format.md) to
update the supplied seed or create a new seed. Leave the seed uncommitted for
review unless the user explicitly requests a commit.

Report the seed path, recommended first story, rejected simpler alternative,
effort distribution, and open decisions. After writing the seed, end with:

`## STORY DECOMPOSITION WRITTEN`
