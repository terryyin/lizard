# Planning scope and lifecycle

## Choose the planning level

Refine unresolved stories in their seeds. Once goal, scope, and key examples are
understood and planning is authorized, write/refine one active executable plan.
A plan does not decide story scope; a seed alone does not authorize execution.

An evidenced bounded retrospective correction may instead use its active plan
as the authoritative input when the plan contains its source and provenance,
beneficiary and bounded outcome, current findings and scope, preserved promises
and genuine constraints, observable proof ownership, current decisions, and
executable slices. An existing seed can add context but is not required. Name
whichever required correction field is missing and stop that path; do not create
or recover a seed to fill the gap. Resolve disputed product constraints through
the shared
[plan-conflict handoff](../../dough-execute-plan/references/execution-decisions.md#resolve-a-disputed-plan-restriction),
leaving the decision with the human.

Planning and refinement never supply execution authority. Execute only when the
current human or invoking workflow instruction authorizes execution; a
planning-only handoff stops before implementation even when the plan is complete.

## Refine story understanding

Read selected stories and relevant discussion; reuse prior answers. Ask only
questions that change understanding, with concise proposed answers, without a
questionnaire or approval ceremony. Distinguish unresolved proposals from decisions.

For each story, establish:

- **Goal:** beneficiary, desired change, and contribution to the business goal.
  Keep the story's observable outcome distinct from the broader ambition.
- **Scope:** required behavior, justified rejection constraints, deferred
  promises, and boundary assumptions, using the distinction below.
- **Key examples:** concrete pre-condition → trigger → result situations that
  explain the scope. Include boundaries or exceptions when they resolve
  ambiguity; do not enumerate a complete test suite.

Prefer the smallest useful outcome. Clarify uncertain additions when possible;
otherwise exclude them and report what was considered. If exclusion prevents
the stated goal or examples from working, resolve the question before dependent
planning or implementation. Necessary implementation details are not extra
product scope. Naturally general behavior need not add delivery or verification
commitments; speculative capabilities are extra scope.

Add **UI** descriptions or sketches only when interaction or presentation needs
agreement. Add **Architecture** only for a new consequential concern; consult
[dough-adr-awareness](../../dough-adr-awareness/SKILL.md) and relevant Accepted
ADRs. Inspect existing behavior or code only to resolve a concrete question,
without turning refinement into technical planning. Omit unused optional sections.

## Examples and constraints

Examples demonstrate required behavior; their counts and arrangements do not
forbid unlisted cases. Require an independent domain or product requirement to
justify rejection, and cite it when recording negative acceptance. Do not invent
that justification when it is missing; record any unresolved constraint decision.
Deferred promises state what this delivery does not commit to build or verify,
not what the product must reject.

For example, £10 + £20 gives a £30 basket. Summing prices also handles a valid
third item; its absence from the example justifies neither rejection nor a
separate handler. An explicit item limit does justify rejection. Deferring discounts
adds no discount machinery and leaves naturally supported baskets intact.

Story refinement sets delivery commitments, not product implementation
boundaries. Implementation may change any product parts needed for the promised
outcome while respecting genuine product constraints and architectural decisions.
Implement the simplest understood domain rule that satisfies the current
examples and constraints. Story membership alone does not justify a structural
boundary or structure for deferred behavior.

## Update a feature story in its seed

For feature stories, use this section. Corrections keep outcome, scope, proof,
and decisions in their active plan under the correction-input contract above.

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

Resolve the project's plan location, format additions, statuses, and lifecycle;
use one plan for the work, avoiding deprecated locations.

Keep execution, proof, review, and resume information:

1. **Source** — selected story or decision link, or retrospective findings and
   execution provenance for a correction.
2. **Goal and scope** — one selected outcome, material exclusions, and
   assumptions.
3. **Outside-in proof** — key examples and their observable test or
   demonstration signals.
4. **Ordered slices** — capability-named heading, Behavior or Structure type,
   status, and proof.
5. **Current decisions** — only choices constraining remaining work.
6. **Learnings** — only discoveries changing assumptions or remaining slices.

Once planned execution starts, keep durable resume state in this same plan:
the established execution identity, current decisions, consequential learnings,
and accepted proof that later work may reuse. For each retained proof, name its
promise, covered boundary, inspected setup and observation locations, literal
command, and result. A slice status records the plan's current assessment; it
does not by itself establish that the corresponding changes were committed or
pushed.

Keep live operational state in the execution conversation: active agent and
refactor returns, ownership and paths of unfinished changes, current delivery
boundary, and the exact CI observer identity and coverage when one exists. Git,
agent, and observer state remain the evidence of what actually happened. If an
interruption or changed observation produces a decision that constrains later
work, retain that decision or learning in the plan and replace stale detail.
Do not create a checkpoint file, duplicate raw output, or add delivery statuses
to make this state recoverable.

Use the project's slice format, or this fallback:

```markdown
### N. Capability outcome
Type: Behavior | Structure
Status: planned | done
Proof: <observable signal and focused verification>

Behavior: <pre-condition → trigger → externally observable postcondition>
```

For Structure, replace `Behavior` with the internal change and the immediate
next Behavior it enables, or the directly owned retrospective correction under
[slice decomposition](../../dough-story-decomposition/references/problem-decomposition.md#decompose-slices). Planning numbers stay in planning artifacts; product
code, tests, and enduring documentation remain capability-named.

## Own executable proof

Map every checkable final-state promise in the selected story or bounded
retrospective correction and current decisions to an owning slice and observable proof. Inline links or a compact table
are sufficient. Include applicable promises, not broader aspirations. Passing
commands without the promised observation does not establish completion.

Preserve mappings through refinement, replacement, and resume. Repoint promises
before declaring replacement slices ready; orphaned promises leave the plan
incomplete. Preserve completed evidence unless a changed boundary invalidates
what it covers. For interim replacements, align affected callers, fixtures,
assertions, and documentation with the final success and rejection behavior.

Choose the smallest sufficient proof at the stable boundary of the promise.
Inspect assertions and setup: distinguish starting preconditions from behavior
the product promises to establish. A fixture or seam supplying that behavior
leaves it unproved; keep the evidence for what it actually observes. An inner
operation finishing does not prove completion for its caller.

Before changing a shared operation or choosing its proof, inspect affected production
call sites, reusing available product-wide search. Derive obligations from each caller's
actual use, not method name or dominant use; exclude unrelated consumers. Incompatible
purposes each need an observation; equivalent purposes may share sufficient proof.
For unresolved domain purpose, ask precisely about that caller's requirement and stop
its dependent obligation until answered rather than guessing policy.

For artifact-preservation promises, identify installation, physical store, and
predecessor using project-supplied identities/scope. Same-store continuity proves no
transfer from another store. Surface target/scope conflicts before dependent work
(e.g. preserving a Docker volume while the owner's native data lives elsewhere).
Migration needs authority and proof; deferred migration is not completed. Ordinary
single-store continuity reuses matching evidence without inventing a predecessor or
migration task. Apply conditionally, not as a mandatory story section.

When acceptance needs a pre-change observation, obtain it before dispatching the
change that would invalidate it. Reuse adequate baselines with known matching revision
and relevant environment/selection conditions. Missing/failed prerequisites stop only
the dependent path; name the gap and continue unrelated work. If the original baseline
is unrecoverable, label a reconstructed comparison and prove revision/conditions
comparable; otherwise its claim (e.g. speedup) remains unproved. Apply conditionally,
without benchmarking every story or delaying independent work/setup.

Reuse sufficient evidence. Obtain only missing observations within authorized
work; if unavailable, report what is covered and the specific unproved promise.
That promise remains incomplete; reporting the gap does not fulfill or remove
it. Limit success claims to the observed cases.

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
and its cumulative design assessment, sizing, and escalation rules before
declaring execution ready.

When evidence changes an execution assumption, apply [execution reassessment](../../dough-execute-plan/references/execution-decisions.md#reassess-before-extending-work).
Retain only consequential information needed later; leave raw diagnostics at their
source. Replace affected future detail/proof mappings and record the changed decision
once. Preserve compatible completed slices, proof with unchanged boundaries, and valid
decisions/learnings; replanning alone warrants no unrelated refresh or history rewrite.

Do not make a disputed story-scope or Accepted-ADR decision through plan editing.
Record the affected source and field and keep that path stopped under execution's
human-decision procedure; independently supported remaining work may continue.

After an execution overrun, record elapsed time, completed evidence, the failure
or thrash point, and the sizing assumption that proved false. Replace slices
only when learning escalation permits, and change later slices only when the
same disproved assumption applies. Preserve a stated focused-test or
external-wait exception that decomposition cannot reduce.

## Clean up after implementation

Keep enduring behavior in tests and product documentation, and enduring design
in code and ADRs. Once that knowledge is captured, reduce each implemented
feature story's refinement detail to Goal and Scope, including exclusions,
while the plan and review evidence still exist. A bounded correction remains in
its plan; do not create a seed as cleanup ceremony. Retain unfinished siblings
and correction plans.

When the executable plan completes, keep the plan, its feature-story source when
applicable, and review inputs for retrospective and
[dough-story-wrap-up](../../dough-story-wrap-up/SKILL.md). Do not delete spent
source or plan history, create a completion record, or trim review inputs here.
Wrap-up owns that closure.
