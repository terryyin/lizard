---
name: dough-slice-planning
description: >-
  Plans one understood, bounded story, retrospective correction, or remaining
  work from an understood instruction as an executable sequence of
  Behavior/Structure slices with outside-in proof and safe stopping points. Use
  when a selected story is ready for implementation planning, an execution
  retrospective has one bounded correction to plan, or authorized remaining
  work from a sufficient instruction needs an ordinary plan. Stays within the
  triggering instruction's execution authority: finish after writing and
  reporting the plan unless that instruction explicitly also requests execution.
  Invokes slice-plan refinement when remaining plan concerns can be resolved
  within the understood outcome, then reports remaining concerns or a limited
  no-concerns finding and records readiness through the shared preparation
  procedure without granting Take or execution.
---

# Slice planning

Write one sufficient executable plan for one understood story, a bounded
retrospective correction, or remaining work from an understood instruction as
described below. Stay within the triggering human or parent-agent
instruction's explicit execution authority. Do not implement product code or
invoke execution unless that instruction explicitly also requests execution
after planning.

## Require understood planning input

Require one user or stakeholder outcome, its value, evaluable key examples, and
boundaries from later work. Name the missing field and stop; do not invent a
story, seed, or completed plan slices. Use
[dough-story-refinement](../dough-story-refinement/SKILL.md) when a selected
story's goal, scope, or examples are unresolved. Use
[dough-story-decomposition](../dough-story-decomposition/SKILL.md) when the
parent problem, candidate selection, or story ordering is unresolved. Never
turn a decomposition seed directly into an execution plan. An understood
instruction may be the source without a story. When its mission was
[admitted](../dough-execute-plan/references/admit-accepted-work.md), plan that
admitted story instead: the plan attaches to it, never to another story.

For a correction handed off by
[dough-execution-retrospective](../dough-execution-retrospective/SKILL.md#reconcile-findings-with-current-truth),
use its evidenced current findings, one bounded correction outcome, affected
concepts, preserved product promises and constraints, and evaluable proof as the
planning input. Cite the original story and reviewed commits for provenance;
do not invent a feature promise or require the correction to fit the old
story's implementation footprint. The retrospective owns current-truth checks,
constraint disputes, and whether to amend an unfinished plan or create a
follow-up. Missing correction scope or proof stops this planning path.

For a new follow-up, write its minimal story and then its plan as
[planning scope and lifecycle](../dough-story-refinement/references/planning.md#choose-the-planning-level)
defines; record preparation on that story. An amended plan or a plan-homed
correction keeps its existing home and identity.

## Resolve execution context

Before writing, identify from the user's instructions and this project's guidance:

- the selected story and its seed, when one exists, the retrospective
  correction input, its source execution, and the seed hosting its story, or
  the understood remaining-work instruction and any retained execution identity;
- the executable-plan root, filename layout, format additions, status
  vocabulary, and lifecycle;
- any supplied slice target and hard limit, including their permitted
  exceptions and overrun escalation;
- required verification, refactoring, commit, and review gates;
- relevant code, tests, stack rules, and Accepted ADRs;
- this project's established North Star location, when one exists; and
- any phase or quick-task conventions that own the plan.

Resolve these from this project, not this skill's location. First reuse a plan
that is active under this project's status vocabulary and identifies that
source. Honor the retrospective's unfinished-plan
amendment destination. Otherwise, inspect the established plan entries in the known
root: use the number after the highest allocated entry, preserving its numeric
padding and path layout rather than filling an old gap. Immediately before
writing, recheck the candidate path. If it is occupied, leave it unchanged,
advance to the next number, and check again. Do not add allocation or locking
tooling.

If the canonical plan root is unavailable, name that missing context and stop
before writing; do not ask for a plan number or invent a location. A missing
numeric limit alone is not missing context: apply the linked sizing guidance
without inventing a timing policy. Do not create a new plan under a deprecated
or merely inferred location.

Treat a verification gate as a local requirement only when the user's
instructions or this project's guidance require it for local work, and cite
that requirement in the plan. Hosted CI configuration shows which checks exist
and how to run them. Hosted CI still runs them after publication, where their
failures stay owned, but its configuration does not by itself make each check a
local gate for every change. Choose local proof for the affected behavior under
[own executable proof](../dough-story-refinement/references/executable-proof.md),
and state in the plan the reason for any broader local check, such as a changed
fixture that distributed consumers load. Execution applies the same distinction
when it [accepts proof](../dough-execute-plan/references/wrap-up.md#accept-proof).

## Write the plan

Before writing to the plan, establish or reuse the required workspace under
[preparation workspace](../dough-story-refinement/references/preparation-workspace.md),
then, for a queued story,
[announce the preparation assignment](../dough-story-refinement/references/preparation-assignment.md#announce-the-preparation-assignment);
inspecting the story, code, or tests to prepare the plan needs neither on its
own. Record the source, goal, included scope, material exclusions,
assumptions, and key examples without enlarging that source. Read and apply:

- [architectural thinking](references/architectural-thinking.md) to carry
  PFE findings, relevant accepted decisions, and only warranted short-term
  direction into the plan;
- [slice decomposition](../dough-story-decomposition/references/problem-decomposition.md#decompose-slices),
  including its cumulative design assessment, sizing, and escalation rules; and
- [executable-plan decisions](../dough-story-refinement/references/planning.md#write-an-executable-plan),
  including executable proof ownership.

Inspect only the code and tests needed to find the stable outside-in proof entry
point, behavior to extend or preserve, genuine dependencies, decisive premises,
and any Structure justified under the linked slice decomposition rules.

A decisive premise is a factual claim about this project's current state that a
slice's approach, sizing, or proof depends on: existing code and tests, host or
environment state, fixture content, workload data, or a named proof or
measurement command. Premises inherited from the story, such as "works as
today", count the same as those you write.

Derive premises from the key examples: trace each one from trigger to
observable result through existing code, and every step the plan relies on as
already behaving is a decisive premise, written down or not. That includes
anything that can collide with an example (an overlay, a competing rule) and
every transformation between a fixture's inputs and the operation that
evaluates them. A helper, step, hook or test that exists, or a grep hit, is
presence, not settling. A claim that a change fixes a reported symptom is a
premise: reproduce the symptom before dependent work, and a remedy spec that
already passes leaves the symptom unexplained instead of counting as the fix.
When an existing fixture can run the journey cheaply and safely, run it instead
of reading call sites.

Before recording `ready`, establish each decisive premise with the smallest
safe observation: reading, searching, listing, a read-only host query, or one
unpaid, side-effect-free local run of the named command. Observe the thing the claim is about, not only where you expect it: a
claim that something has no test, or that a named proof exercises a behavior,
is observed by searching for the existing tests and callers of what changes,
wherever they live. When the plan moves or relocates a function, that search
includes every caller, including scripts and step definitions, not only tests
that import it. If a script, command, or route reaches the moved function,
keep searching for a feature whose steps run that script. When such a feature
exists, name the feature and record the observation that its steps run the
script and the script calls the function. Naming only the script does not
settle the claim while a feature runs it, and a test that imports or calls
the moved unit does not settle it either. Naming an entry that does not reach
the function does not settle it. For uncertain infrastructure or storage
behavior, the observation is one isolated representative proof against the
relevant engine and version, unless matching evidence exists. Record each
premise, the operation that consumes its result, the literal observation that
reaches it, and its result in the plan. A false premise changes the plan before
broad implementation. Do not inspect claims the approach does not depend on, and
keep experiments off shared and production systems.

When only a paid, credentialed, owner-held, or state-changing observation can
settle a premise, observe its cheap parts now and make the remainder an early
probe slice whose failure stops dependent slices and changes the plan. Such a
plan can be `ready`; the probe's observation keeps its existing authority
requirements.

During construction, apply those decomposition, cumulative design, and sizing checks: correct
obvious defects such as an independent second outcome before reporting, and
preserve proof ownership and any supplied sizing constraints on every resulting
slice.

After the plan file exists for a work item with a recorded identity, apply
[record preparation facts](../dough-product-backlog/references/record-preparation.md)
for the planned approach (omit assessment on that write).

## Resolve fixable plan concerns

When remaining concerns about slice boundaries, cumulative design, proof
ownership, or sizing can be resolved within the understood outcome and scope,
invoke [dough-slice-plan-refinement](../dough-slice-plan-refinement/SKILL.md)
on the written plan before the final report and readiness assessment. This
refinement is part of slice planning and needs no additional instruction;
honor an explicit instruction to leave refinement to a later step. A plan
without such concerns needs no refinement pass.

Keep unresolved source questions and human-owned decisions outside this
handoff. Report a missing input, disputed constraint, or concern that refinement
cannot resolve within scope under the next section; do not repeat refinement
without new evidence or widen the outcome.
Refinement keeps the same plan, preparation workspace, and assignment. It
grants neither execution nor publication authority.

## Report concern evidence and assess readiness

After constructing the plan, report, then record readiness through the shared
procedure:

- In one line, whether refinement ran, was not needed (with the reason), or
  was left to a later step as instructed.
- Each remaining slice-specific concern with the affected slice, the reason
  (for example an integration assumption or repeated special-case design), and
  its consequence (for example uncertain sizing or duplicated domain rules),
  including concerns spanning successive slices. When this review identified
  none, say so narrowly.
- Interim behavior that slice decomposition permits is an accepted trade-off,
  not a remaining concern, once the plan names it and its replacing slice.
  Relabeling a blocking concern, such as a proof gap, does not settle it.
- Then apply
  [assess readiness at preparation completion](../dough-product-backlog/references/record-preparation.md#assess-readiness-at-preparation-completion):
  every remaining concern becomes a `not-ready` reason, and neither refinement
  nor an accepted trade-off establishes readiness alone. Do not prescribe the
  next workflow action, Take the item, or start execution from this finding.

The recipient chooses the next action under the triggering instruction's
authority and project policy. The recorded assessment is agent judgment bound to
content digests, not authorization to execute. It does not grant, expand, or
replace the triggering instruction's execution authority.

## Stay within the triggering instruction

After writing and reporting the plan, the next action remains within the
triggering human or parent-agent instruction:

- Planning-only request: report the plan path, ordered slices,
  considered-but-excluded additions, the refinement decision and concern
  report, and the recorded readiness assessment, then stop. Do
  not implement and do not invoke execution.
- Parent-agent delegation that asks only for slice planning: return the plan,
  the refinement decision and concern report, and the recorded readiness
  assessment to the parent. The parent's broader implementation task
  is not an explicit execution request to this planner.
- Explicit plan-and-execute request: after reporting, the authorized workflow
  may continue into execution without asking again for the same authorization,
  subject to this project's gates and any unresolved concerns that still block
  progress. Prefer the project's established execution path (for example
  [dough-execute-plan](../dough-execute-plan/SKILL.md)) when that path applies.

Apply [preparation workspace](../dough-story-refinement/references/preparation-workspace.md)'s
keep or discard decision, then close or retain the workspace, when this
session ends.

After the matching case above, end with:

`## SLICE PLAN WRITTEN`
