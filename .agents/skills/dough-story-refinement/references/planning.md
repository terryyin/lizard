# Planning scope and lifecycle

## Choose the planning level

Refine an unresolved selected story in its seed. Once its goal, scope, and key
examples are understood and executable planning is authorized, write or refine
one active executable plan. Do not use an execution plan to decide story scope,
and do not turn a story seed directly into executable work.

## Refine story understanding

Read the selected stories and relevant prior discussion. Reuse answers already
given; ask only questions that change understanding, with a concise proposed
answer. Do not turn refinement into a questionnaire or mandatory approval
ceremony. Mark unresolved decisions explicitly; do not present proposals as
human decisions.

For each story, establish:

- **Goal:** beneficiary, desired change, and contribution to the business goal.
  Keep the story's observable outcome distinct from the broader ambition.
- **Scope:** included behavior, material exclusions, and boundary assumptions.
- **Key examples:** concrete pre-condition → trigger → result situations that
  explain the scope. Include boundaries or exceptions when they resolve
  ambiguity; do not enumerate a complete test suite.

Prefer the smallest useful outcome. Clarify uncertain additions when possible;
otherwise exclude them and report what was considered. If exclusion prevents
the stated goal or examples from working, resolve the question before dependent
planning or implementation. Necessary implementation details are not extra
product scope; speculative generality is.

Add **UI** descriptions or sketches only when interaction or presentation needs
agreement. Add **Architecture** only for a new consequential concern; consult
[dough-adr-awareness](../../dough-adr-awareness/SKILL.md) and relevant Accepted
ADRs. Inspect existing behavior or code only to resolve a concrete question,
without turning refinement into technical planning. Omit unused optional sections.

## Update the story in its seed

Follow the shared
[seed format](../../dough-story-decomposition/references/seed-format.md) for
seed ownership, metadata, anchors, and backlog boundaries. Expand each selected
story section with the understanding above, replacing overlapping detail.
Record only open questions that affect that story. When refining several
stories, keep each outcome and boundary separate; do not merge them into one
delivery by implication.

If no seed exists, create one using that format. Do not invent parent-problem
decisions to fill it; route unresolved framing or candidate selection to
[dough-story-decomposition](../../dough-story-decomposition/SKILL.md).
Do not create a separate refinement file.

Discuss goal or scope changes with the human and keep the story in its seed and
any active plan aligned; discovery alone does not authorize expansion. Do not
silently cancel remaining scope or change siblings. Preserve compatible work
and evidence when revising boundaries. Use the project's own locations and
workflows for executable plans, phase artifacts, and project memory. Keep
planning-only numbering out of product code, tests, and permanent documentation.

## Write an executable plan

Resolve this project's executable-plan location, format additions,
status vocabulary, and lifecycle. Do not infer a deprecated location or create a
second plan for the same work.

Keep only information needed for execution, proof, review, or resume:

1. **Source** — selected story or decision link when applicable.
2. **Goal and scope** — one selected outcome, material exclusions, and
   assumptions.
3. **Outside-in proof** — key examples and their observable test or
   demonstration signals.
4. **Ordered slices** — capability-named heading, Behavior or Structure type,
   status, and proof.
5. **Current decisions** — only choices constraining remaining work.
6. **Learnings** — only discoveries changing assumptions or remaining slices.

Use this project's equivalent slice format when supplied; otherwise use:

```markdown
### N. Capability outcome
Type: Behavior | Structure
Status: planned | in-progress | done
Proof: <observable signal and focused verification>

Behavior: <pre-condition → trigger → externally observable postcondition>
```

For Structure, replace `Behavior` with the internal change and the immediate
next Behavior it enables. Planning numbers stay in planning artifacts; product
code, tests, and enduring documentation remain capability-named.

## Own executable proof

Map every checkable final-state promise in the selected story and current
decisions to an owning slice and observable proof. Inline links or a compact table
are sufficient. Include applicable promises, not broader aspirations. Passing
commands without the promised observation does not establish completion.

Preserve mappings through refinement, replacement, and resume. Repoint promises
before declaring replacement slices ready; orphaned promises leave the plan
incomplete. Preserve completed evidence unless a changed boundary invalidates
what it covers. For interim replacements, align affected callers, fixtures,
assertions, and documentation with the final success and rejection behavior.

Choose proof at the stable boundary of the promise:

| Situation | Proof |
| --- | --- |
| Main user behavior | Targeted end-to-end check or another real high-level boundary |
| Edge, error, or pure contract | Focused unit proof |
| Existing untested behavior | Regression proof before changing it |
| Structure slice | Existing external behavior remains green |
| Interim behavior | Name the later slice that removes or replaces it |

Run focused relevant checks at slice boundaries. Require broader suites only when
this project's workflow or user requires them. When asynchronous ownership
changes, prove that the named lifecycle owner observes background failure in
time and performs applicable cleanup after failure or shutdown; an awaited
exception alone proves neither. Derive timing from the selected lifecycle
contract rather than an arbitrary timeout.

## Refine the active plan

Edit the same plan in place. Preserve completed slices and resume-useful history,
replace obsolete planned detail rather than appending a competing breakdown,
and record only learnings that affect remaining work. Apply
[slice decomposition](../../dough-story-decomposition/references/problem-decomposition.md#decompose-slices)
and its sizing and escalation rules before declaring execution ready.

After an execution overrun, record elapsed time, completed evidence, the failure
or thrash point, and the sizing assumption that proved false. Replace slices
only when learning escalation permits, and change later slices only when the
same disproved assumption applies. Preserve a stated focused-test or
external-wait exception that decomposition cannot reduce.

## Clean up after implementation

Keep enduring behavior in tests and product documentation, and enduring design
in code and ADRs. Once that knowledge is captured, reduce each implemented
story's refinement detail to Goal and Scope, including exclusions. Remove spent
examples, UI sketches, and architectural discussion; preserve its anchor,
completion status, and unfinished siblings. Retain still-needed detail until
the enduring knowledge has a home.

When the executable plan completes, remove spent diary and obsolete summary
detail according to this project's lifecycle. Preserve unfinished scope and any
evidence still needed for resume or review.
