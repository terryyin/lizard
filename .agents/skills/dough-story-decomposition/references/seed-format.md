# Seed format

Update the supplied seed or allocate a new seed ID and filename in the project's
canonical seed directory. Keep one parent problem and its candidate stories in
one seed, with one canonical section per story. A seed is non-executable
planning input.
Keep each story in one seed, even when its journey crosses concerns in other
seeds, and link related seeds instead of duplicating story details. Preserve
sibling stories, existing metadata, and anchors.

Map the metadata below to the project's field names and lifecycle vocabulary:
seed identity, dormant status, creation date, creation context, resurfacing
trigger, and whole-set size. Do not invent values; resolve required missing
context before writing.

```yaml
---
id: <seed ID>
status: <this project's dormant status>
planted: <creation date>
planted_during: <milestone, phase, or context>
trigger_when: <when this problem should surface>
scope: <whole-set size under this project's conventions>
---
```

```markdown
# <seed ID>: <parent problem or desired effect>

## Why This Matters

<beneficiary, current problem, desired effect, evidence>

## Alternatives and Decision

<recommended direction, strongest simpler alternative, assumptions>

## Story Decomposition

### 1. <observable outcome>

- **For / why:** ...
- **Evaluation:** ...
- **Value / learning:** ...
- **Effort hypothesis:** S | M | L — confidence and assumptions
- **Depends on:** none, or a genuine product prerequisite
- **Safe stopping point:** value retained if later stories are cancelled, and
  safety conditions this story must satisfy independently

## Ordering and Scope Reduction

<ordering rationale, safe stopping points, first-to-drop stories>

## Open Decisions

<only decisions that change story selection or order>

## When to Surface

<trigger>

## Breadcrumbs

<supplied stories, explicit system requirements, or references; no code audit>
```

## Backlog boundaries

Only when the user asks to queue or reprioritize stories, update the canonical
product backlog as a global ordered list of story titles linked to their
sections in seeds, with seed IDs. Keep story details in the seeds. Use the project's
stable-anchor convention for new queued stories; local numbering is not global
priority. Leave unqueued candidates in their seeds.
