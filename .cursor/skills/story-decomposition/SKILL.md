---
name: story-decomposition
description: >-
  Challenge and decompose a broad product problem into ordered
  Valuable/Visible/Vertical stories with rough effort hypotheses. Use when the
  why, expected outcome, learning priority, or story boundaries are unresolved.
  Writes one non-executable seed. Use slice-planning once one story is selected.
---

<objective>
Produce one human-reviewed seed containing a clear parent problem and ordered
candidate stories. Apply `.cursor/rules/problem-decomposition.mdc`. Do not
inspect implementation or perform technical design to refine estimates.
</objective>

<input_gate>
Use this skill when at least one is true:

- the beneficiary, problem, desired effect, or value is unclear;
- the request prescribes a solution without establishing why it is needed;
- multiple product outcomes or story boundaries must be chosen;
- the highest-value or highest-learning first increment is disputed.

If one 3V story is already selected and bounded, use **slice-planning**.
</input_gate>

<required_human_decisions>
Before writing the seed, use an explicit answer the human already gave or ask
the human to accept/revise your proposed answer for:

| Decision | Required answer |
|----------|-----------------|
| Beneficiary | Who experiences the problem or evaluates the outcome? |
| Current problem | What happens now, including the workaround? |
| Desired effect | What observable change would be worth having? |
| Value now | Why act now rather than defer or do nothing? |
| Simpler alternative | What is the strongest smaller, manual, or existing-tool option, and why is it insufficient? |
| Highest learning | Which assumption should the first story test? |
| Constraints | Which boundaries are problem facts rather than proposed design? |

- Do not repeat questions already answered.
- Ask only questions whose answers can change story selection or order.
- Ask at most three closely related questions per turn.
- State the current hypothesis and recommended answer with each question.
- If competing answers materially change the decomposition, stop and wait for
  the human instead of choosing silently.
</required_human_decisions>

<process>

<step name="frame_and_challenge">
Write the parent problem as:

```text
For <beneficiary>, <current problem> should change to <desired effect>, within
<genuine constraints>.
```

Evaluate these options explicitly:

1. Do nothing or defer.
2. Make a smaller behavior change.
3. Use a manual or existing-tool workflow.
4. Pursue the requested direction.

Recommend one. Record the evidence, assumptions, and why the strongest rejected
alternative is insufficient.
</step>

<step name="cut_candidate_stories">
For each candidate:

1. Name one user or stakeholder outcome.
2. Apply the 3V gate in `problem-decomposition.mdc`; reject failures.
3. State how the beneficiary evaluates the outcome.
4. State its user value or the consequential assumption it tests.
5. Name only genuine product prerequisites.
6. State the value that remains if later stories are cancelled and any safety
   condition this story must satisfy on its own.

Do not add file-level tasks, technical layers, APIs, or implementation design.
Acceptance examples are optional here; include one only when it changes the
story boundary.
</step>

<step name="estimate_and_order">
Use the S/M/L story bands from `problem-decomposition.mdc` without code
inspection.

- Record band, confidence, and assumptions.
- Split a likely larger-than-L candidate using an allowed splitting move.
- Order by user value, then learning value, then genuine prerequisites.
- Move a later story earlier when it delivers more value or tests a more
  consequential assumption sooner.
- List stories in first-to-drop order for scope reduction.
</step>

<step name="write_the_seed">
Update a supplied seed or create a `SEED-NNN` higher than the current maximum
under `.planning/seeds/`. One seed represents the parent problem; its stories
are not separate seed files.

Preserve the GSD seed frontmatter:

```yaml
---
id: SEED-NNN
status: dormant
planted: YYYY-MM-DD
planted_during: <current milestone/phase or context>
trigger_when: <when this problem should surface>
scope: <small | medium | large | unknown for the whole set>
---
```

Use this body:

```markdown
# SEED-NNN: <parent problem or desired effect>

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

## Ordering and Scope Reduction
<ordering rationale, safe stopping points, first-to-drop stories>

## Open Decisions
<only decisions that change story selection or order>

## When to Surface
<trigger>

## Breadcrumbs
<supplied requirements or references; no code audit>
```

Do not write an executable PLAN. Leave the seed uncommitted for review unless
the user explicitly asks for a commit.
</step>

</process>

<success_criteria>
- All required human decisions are answered or explicitly open.
- The strongest simpler alternative is evaluated.
- Every candidate passes the 3V gate and has observable evaluation.
- Estimates include band, confidence, and assumptions.
- Ordering follows value, learning, and genuine prerequisites.
- The output is one non-executable seed.
- Final response ends with `## STORY DECOMPOSITION WRITTEN`.
</success_criteria>

<output>
Report the seed path, recommended first story, rejected simpler alternative,
effort distribution, and open decisions.

```text
## STORY DECOMPOSITION WRITTEN
```
</output>

<out_of_scope>
- Implementation inspection or technical design.
- Executable planning or implementation.
- Technical-layer, activity, or team-based stories.
</out_of_scope>
