# Planning scope and lifecycle

## Choose the planning level

Refine unresolved stories in their seeds. Once goal, scope, and key examples are
understood and planning is authorized, write/refine one active executable plan.
A plan does not decide story scope; a seed alone does not authorize execution.

An evidenced bounded retrospective correction uses correction input instead of
a refined feature story. Before planning, give a new correction a minimal story
in the reviewed story's seed or another seed hosting the problem; create one
under [seed format](../../dough-story-decomposition/references/seed-format.md) only when none fits.
The story records its `**Identity:**`, allocated under
[allocate a new identity](../../dough-product-backlog/references/identity.md#allocate-a-new-identity),
a **Goal** naming the beneficiary and bounded correction outcome, bounded
**Scope**, and its plan link, adding no promise about a product intent. The plan
records the story's identity under
[work item identity](../../dough-product-backlog/references/identity.md) and
holds the remaining input: source and provenance, current findings, preserved
promises and genuine constraints, observable proof ownership, current decisions,
and executable slices.

Existing plan-homed corrections keep their home, recorded identity, and complete
input in that plan; do not migrate them or create seeds. For either kind, name
missing required correction fields and stop rather than inventing them. Resolve
disputed product constraints through the shared
[plan-conflict handoff](../../dough-execute-plan/references/execution-decisions.md#resolve-a-disputed-plan-restriction),
leaving the decision with the human.

Planning and refinement never supply execution authority. Execute only under a
current human or invoking-workflow instruction authorizing implementation.

## Refine story understanding

Read the selected stories and discussion; reuse prior answers and distinguish proposals from decisions.
Ask only understanding-changing questions with concise proposed answers, without a questionnaire or approval ceremony.

For each story, establish:

- **Goal:** beneficiary, desired change, and contribution to the business goal.
  Keep the story's observable outcome distinct from the broader ambition.
- **Scope:** required behavior, required restrictions, scope limits, and boundary
  assumptions, using the distinction below.
- **Key examples:** concrete pre-condition → trigger → result situations that
  explain the scope. Include boundaries or exceptions when they resolve
  ambiguity; do not enumerate a complete test suite.

Prefer the smallest useful outcome. Clarify uncertain additions when possible;
otherwise exclude them and record what was considered with the story's
deferred promises or the plan's exclusions. If exclusion prevents
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

Separate **scope limits** (work this increment does not undertake) from
**required restrictions** (product behavior limited for an independently
justified product need). Examples demonstrate required behavior; their counts
and arrangements do not forbid unlisted cases. Before adding a restriction or
preserving one implicated by the change, identify its independent product need
or actual safety/correctness risk. This includes prerequisites, filters, count
limits, and suppressed operations. Scope limits, examples, existing code and
tests alone do not justify a required restriction. Cite the reason when making
a restriction an acceptance requirement; leave consequential unresolved
restriction decisions with the human.

Deferred promises limit delivery and verification commitments; they do not
require preventing other cases. A capability may remain unimplemented, and a
guard may protect a known implementation limitation: identify the limitation
and risk rather than treating the delivery boundary as its justification.

For example, £10 + £20 gives a £30 basket. Summing prices also handles a valid
third item; its absence from the example justifies neither rejection nor a
separate handler. An independently justified item limit does justify rejection.
Deferring discounts adds no discount machinery and leaves naturally supported
baskets intact.

Story refinement sets delivery commitments, not product implementation
boundaries. Implementation may change any product parts needed for the promised
outcome while respecting genuine product constraints and architectural decisions.
Implement the simplest understood domain rule that satisfies the current
examples and constraints. Story membership alone does not justify a structural
boundary or structure for deferred behavior.

## Update a feature story in its seed

For feature stories, use this section. Correction stories stay minimal under the
contract above, keeping findings, proof, and decisions in their plans.

Follow [seed format](../../dough-story-decomposition/references/seed-format.md)
for ownership, metadata, anchors, and backlog boundaries. Expand each selected
story with the understanding above, replacing overlapping detail. Record only
its relevant open questions. Keep each story's outcome and boundary separate
when refining several; do not merge them into one delivery by implication.

Create a missing seed using that format; route unresolved parent framing or
candidate selection to [dough-story-decomposition](../../dough-story-decomposition/SKILL.md)
rather than inventing decisions. Keep refinement in the seed.

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
7. **Story obligations** — during planned execution, every reported gap, loss,
   limitation, or interim behavior uses the `## Story obligations` section and
   [entry format and dispositions](../../dough-execute-plan/references/story-obligations.md#record-format).
   Its `**Source:**` link names the selected story section. Plans without the
   section have no obligations; no migration is required. Planned execution
   requires a `**Source review:**` link under `**Source:**` to the
   [source review](../../dough-execute-plan/references/story-obligations.md#record-the-source-review)
   once acceptance retains it; acceptance adds and allocates it, and planning
   does not write it.

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

After writing an identified work item's plan, [record preparation facts](../../dough-product-backlog/references/record-preparation.md)
for the planned approach, omitting assessment on that write.

## Own executable proof

Follow [own executable proof](executable-proof.md) for executable plans.

## Refine the active plan

Edit the same plan in place. Preserve completed slices and resume-useful history,
replace obsolete planned detail rather than appending a competing breakdown,
and record only learnings that affect remaining work. Apply
[story-obligation ownership](../../dough-execute-plan/references/story-obligations.md)
when removing or renumbering slices: move receiving and interim obligations
and source-review allocations to their remaining owners,
preserve reported text and provenance, and update slice references.
Replanning never drops an obligation; the check refuses dangling references.
Apply
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
