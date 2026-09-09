# Problem decomposition

## Choose the decomposition level

Do not plan a lower level while a higher-level decision remains unresolved.

| Level | Use when | Result |
| --- | --- | --- |
| **Problem or capability** | The beneficiary, problem, desired effect, constraint, or direction is unclear | One evaluable decision, assumption, or outcome |
| **Story** | The problem is understood but several useful outcomes are possible | Ordered Valuable, Visible, Vertical stories |
| **Slice** | One story's goal, scope, and key examples are understood | Behavior slices and only immediately enabling Structure slices |

The levels are fractal: name the evaluator and observable result, split
independent outcomes, order by value and learning, and make every stopping point
safe. When evidence invalidates a parent decision, stop and return to that
level rather than continuing to subdivide its children.

## Establish the human-owned decisions

Before writing the seed, use an explicit answer the human already gave or ask
them to accept or revise your proposed answer for each decision:

| Decision | Required answer |
| --- | --- |
| Beneficiary | Who experiences the problem or evaluates the outcome? |
| Current problem | What happens now, including the workaround? |
| Desired effect | What observable change would be worth having? |
| Value now | Why act now rather than defer or do nothing? |
| Simpler alternative | What is the strongest smaller, manual, or existing-tool option, and why is it insufficient? |
| Highest learning | Which assumption should the first story test? |
| Constraints | Which boundaries are problem facts rather than proposed design? |

Reuse answered questions. Ask only questions that can change story selection
or order, at most three closely related questions per turn. State the current
hypothesis and recommended answer with each question. If competing answers
materially change the decomposition, wait for the human instead of choosing
silently. Mark unresolved decisions explicitly; do not record proposals as
human decisions.

## Frame and challenge the problem

Write the parent problem as:

```text
For <beneficiary>, <current problem> should change to <desired effect>, within
<genuine constraints>.
```

Explicitly evaluate doing nothing or deferring, a smaller behavior change, a
manual or existing-tool workflow, and the requested direction. Recommend one.
Record the evidence, assumptions, and why the strongest rejected alternative
is insufficient.

## Select candidate stories

Frame each candidate story as a possibility worth pursuing for user or learning
value. Invite imagination and discussion; allow incomplete understanding to
evolve. Let the journey cross features and system boundaries. When selecting
stories for planning, clarify an observable outcome. Cut around behavior, a
product decision, risk, or learning question. Decompose only enough candidates
to answer the current value or learning question; do not exhaust a feature for
completeness.

Select a candidate story for planning when all three answers are yes:

1. **Valuable:** Does it change an outcome for a named user or stakeholder?
   “Needed for later work” is insufficient.
2. **Visible:** Can that person evaluate the result without inspecting
   implementation?
3. **Vertical:** Does it work end to end across every required layer?

Revise failures. Put necessary non-3V work inside the story it enables. Do not
create stories around technical layers, components, activities, specialists,
or teams, or add file-level tasks, APIs, or implementation design.

For each retained candidate, name its evaluator and evaluation signal, user
value or consequential assumption tested, genuine product prerequisites, and
boundaries distinguishing it from siblings. State the value retained if later
stories are cancelled and the safety conditions it must satisfy on its own.
Merge unused preparation into the behavior it enables.

Include an acceptance example, counterexample, boundary, or exception only when
it changes the story boundary. Use pre-condition → trigger → result for behavior;
do not enumerate an exhaustive acceptance suite.

## Split and size

Split a candidate with two independently useful outcomes or acceptance signals.
Use these splitting moves:

- Narrow the beneficiary, pre-condition, or data variation.
- Deliver one independently usable, observable workflow step.
- Separate common behavior from a later special policy or exception.
- Separate a cheap assumption test from the broader outcome it may justify.
- Use interim behavior for usable value or earlier end-to-end evidence; name
  the later replacement that removes it.

Choose breadth-first or depth-first cuts by earlier value or learning while
retaining an externally evaluable result. Start with a concrete case before a
general solution; extract abstractions after repetition. Keep a prototype
bounded to the cheapest evidence needed for its question.

Estimate comparatively using the project's S/M/L definitions, without code
inspection or implementation design. Record the band, confidence, and
assumptions. Resolve missing band definitions before writing estimates. Split
a likely larger-than-L story using the moves above; do not equalize estimates
by making cuts that fail the 3V gate.

## Order and reassess

Order stories by user value, then learning value, then genuine product
prerequisites. Move a later story earlier when it delivers more value or tests
a more consequential assumption sooner, unless a genuine prerequisite prevents
it. Record the rationale, safe stopping points, and first-to-drop order for
scope reduction.

When evidence invalidates the parent outcome, story boundary, or ordering, stop
at a safe boundary and revisit the highest affected resolution with the human.
Use [dough-story-refinement](../../dough-story-refinement/SKILL.md) for changes
to selected-story goal, scope, or examples. Do not silently cancel remaining
scope or rewrite siblings. Keep stories as planning input; enduring behavior
belongs in executable examples and product documentation.

## Decompose slices

Use slices only after one story is understood and executable planning
is authorized. Every slice is exactly one of:

| Type | Required content | Reject when |
| --- | --- | --- |
| **Behavior** | Pre-condition, trigger, one externally observable postcondition, and outside-in proof | It contains independent postconditions or proof loops |
| **Structure** | Internal change, unchanged external behavior, and the immediate next Behavior it enables | It prepares for anything beyond that Behavior |

Tie every Behavior to included story scope or a key example. Put Structure
immediately before its Behavior. Use the splitting moves above at this finer
resolution, but do not split tests from the Behavior they prove or create slices
around files, layers, components, specialists, or activities.

Refine a slice when it has independent postconditions or proof loops, separable
implementation beats before a green result, hidden preparation, low confidence
at an execution or integration boundary, or a plausible path beyond the
project's hard limit. Keep a multi-beat outside-in scenario explicitly
unfinished until it is green; never make a CI-breaking state a delivery
boundary.

## Size and escalate slices

Use this project's slice target, hard limit, exceptions, and
repeated-overrun threshold. Include implementation, focused verification, and
slice-local cleanup in the sizing hypothesis. Resolve missing limits before
claiming that a plan is ready for direct execution.

At the target, check for independent outcomes or hidden preparation and split
when found. At the hard limit, stop unless a stated focused-test or external-wait
exception applies. Preserve the learning and safely park or revert only
attempt-owned work; preserve developer and unrelated work, and stop for human
judgment when ownership is unclear. Renaming, retrying, or splitting a slice does
not erase an overrun.

After a completed slice, update the same plan when evidence changes only its
remaining slices. When evidence changes the story's goal, examples, scope, or
sibling order, record the affected story and stop for human review. When the
project's repeated-overrun threshold is reached, reassess the story boundary
before another slice-only refinement. Record delivered value, remaining scope,
and a concrete sizing reason for continuing. Preserve compatible work and proof;
discard them only for stated incompatibility or safety.
