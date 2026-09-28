# Planning scope and lifecycle

## Choose the planning level

Refine unresolved stories in their seeds. Once goal, scope, and key examples are
understood and planning is authorized, write/refine one active executable plan.
A plan does not decide story scope; a seed alone does not authorize execution.

An evidenced bounded retrospective correction is a work item with correction
input instead of a refined feature story. A new correction gets a minimal story
in a suitable seed before its plan is written: reuse the reviewed story's seed
or another seed whose problem hosts it, and create one under the
[seed format](../../dough-story-decomposition/references/seed-format.md) only
when none fits. That story records its `**Identity:**`, a **Goal** naming the
beneficiary and bounded correction outcome, the bounded **Scope**, and a link
to the plan; it adds no feature promise. The plan records the story's identity
under [work item identity](../../dough-product-backlog/references/identity.md)
and holds the rest of the correction input: source and provenance, current
findings, preserved promises and genuine constraints, observable proof
ownership, current decisions, and executable slices.

An existing correction whose plan is already its canonical home keeps that home
and its recorded identity, with the whole correction input in that plan. Do not
migrate it or create a seed for it. For either kind, name whichever
required correction field is missing and stop that path; do not invent it.
Resolve disputed product constraints through the shared
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

For feature stories, use this section. A correction story stays minimal under
the correction-input contract above; findings, proof, and decisions stay in its
plan.

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

Preserve the story's anchor and recorded identity when you rename or move its
seed, updating incoming links instead: relocating a story does not re-identify it.
After the selected story section records goal, scope, and key examples for a
known identity, apply
[record preparation facts](../../dough-product-backlog/references/record-preparation.md).

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

1. **Source** — selected story, remaining-work instruction, or decision link;
   or retrospective findings and execution provenance for a correction. Carry
   the work item's recorded identity into the plan under
   [work item identity](../../dough-product-backlog/references/identity.md): the
   link says where the story or plan is now, and renaming or moving either does
   not change the recorded value.
2. **Goal and scope** — one selected outcome, material exclusions, and
   assumptions.
3. **Outside-in proof** — key examples and their observable test or
   demonstration signals.
4. **Ordered slices** — capability-named heading, Behavior or Structure type,
   status, and proof.
5. **Current decisions** — only choices constraining remaining work.
6. **Learnings** — only discoveries changing assumptions or remaining slices.

At execution completion, the executing agent adds one more element after the
ordered slices: the
[execution-complete record](../../dough-execute-plan/references/finish-or-stop.md#record-execution-completion),
a `## Execution complete` section whose `Product advice:` entry is required.
Do not write it while planning. The record is not a plan-level status line;
plans define none.

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

After the plan file exists for a work item with a recorded identity, apply
[record preparation facts](../../dough-product-backlog/references/record-preparation.md)
for the planned approach (omit assessment on that write).

## Own executable proof

Proof ownership for executable plans lives in
[own executable proof](executable-proof.md).

## Refine the active plan

Edit the same plan in place. Preserve completed slices and resume-useful history,
replace obsolete planned detail rather than appending a competing breakdown,
and record only learnings that affect remaining work. Apply
[slice decomposition](../../dough-story-decomposition/references/problem-decomposition.md#decompose-slices)
and its cumulative design assessment, sizing, and escalation rules when changing
remaining-slice design. After a preparation plan rewrite (slice planning or
slice-plan refinement) for a work item with a recorded identity, reassess through
[assess readiness at preparation completion](../../dough-product-backlog/references/record-preparation.md#assess-readiness-at-preparation-completion);
do not grant Take or execution from that record. After an ordinary execution
delivery update that records slice status or accepted proof without that
preparation rewrite, follow
[plan evidence during delivery](../../dough-product-backlog/references/record-preparation.md#plan-evidence-during-delivery)
and do not renew readiness.

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
while the plan and review evidence still exist. A correction story is already
minimal, and a plan-homed correction stays in its plan; do not create a seed as
cleanup ceremony. Retain unfinished siblings and correction plans.

When the executable plan completes, keep the plan, its feature-story source when
applicable, and review inputs for retrospective and
[dough-story-wrap-up](../../dough-story-wrap-up/SKILL.md). Do not delete spent
source or plan history, create a completion record, or trim review inputs here.
Wrap-up owns that closure.
