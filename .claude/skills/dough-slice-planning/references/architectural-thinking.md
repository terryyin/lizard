# Architectural thinking during slice planning

Use architectural thinking while constructing the plan; do not delegate it to
a separate architecture role or add an approval stage. Start with
[dough-pfe](../../dough-pfe/SKILL.md) as the authoritative way to find and
assess existing solutions. Carry its evidenced reuse, change, modularization,
or gap decision into the slices instead of repeating its search procedure here.

## Use established direction first

Read the story, the PFE result, the product structure this work touches, its
domain meaning, and this project's Accepted ADRs. When this project has a
North Star, read it before writing the plan.

Cite every North Star topic that bears on work this plan will execute. A
topic bears on that work when the slices follow it, would depart from it, or
do work the topic names. In the plan, name its location and heading and state
how the slices follow it. To depart, revise the topic first. Leave every
other topic uncited. Do not create or update a topic, an architecture report,
a reference registry, or an approval step merely because planning occurred.

Revise a cited topic only when current evidence warrants a different or
clearer direction. Check the affected stories, domain meaning, and Accepted
ADRs first. Keep uncertainty and a hypothetical later need out of the topic.

When no accepted decision or topic covers a choice that sets shared
responsibility or a product boundary, changes domain meaning, or is difficult
to reverse, record it under [Record warranted new direction
narrowly](#record-warranted-new-direction-narrowly).

### Reconsider recorded direction during execution

When execution returns contrary evidence through the existing execution
handoff, the coordinator applies this same planner judgment rather than asking
the executing role to choose or rewrite direction. Recheck the cited topic,
evidence, affected stories, domain meaning, relevant product structure, and
Accepted ADRs. Select a direction supported by that complete context, then
update the topic only if the selection changes it and align the same active
plan through [active-plan
refinement](../../dough-story-refinement/references/planning.md#refine-the-active-plan).
Resume the affected execution path only after the topic and remaining plan are
consistent. This reconsideration does not close the active story or discard
completed proof.

If the evidence conflicts with an Accepted ADR, do not revise a North Star
topic to work around it. Leave the dependent path stopped and follow the
project's existing human-owned ADR exception or supersession process. Continue
independent work only where its direction remains supported.

## Record warranted new direction narrowly

Add that uncovered choice as one short topic at the project's established
North Star location. Use a heading and a brief paragraph that state the
direction, supporting evidence, and affected work. Cite it as above.

If the project has no North Star location, the planner may choose one shared
`NORTH-STAR.md` under this project's established planning root and state that
location in the plan. Do this only when the new topic is warranted; do not
require a configuration setting or create the file for an ordinary plan.

A North Star topic cannot override an Accepted ADR and holds direction only
until implemented ([where enduring knowledge lives](#where-enduring-knowledge-lives)).
Humans retain decisions about domain meaning and ADR acceptance, exceptions, and
replacement.
When indispensable domain or ADR context is missing or conflicts with the proposed
direction, stop the dependent planning path with the evidence and decision
needed. Continue independent planning work where safe, but do not present the
affected path as executable.

## Keep current work sufficient

Include any Structure slice required to make the selected solution coherent for
the current story, even when that work crosses components or process boundaries.
Place it immediately before the Behavior it enables and prove preserved external
behavior. Do not add structure, topics, or flexibility solely for hypothetical
later stories.

Map persistence, implementation, APIs, UI, and tests directly to the domain's
concepts and meanings with the minimum necessary translation. Keep wire and
command adapters thin over strongly typed handlers, and share or generate one
typed contract across internal boundaries rather than duplicating payload
shapes by hand.

### Runtime failure handling

Follow this project's failure-reporting and recovery conventions. When
designing a failure path, prefer these options in order:

1. **Prevent** the failure by making invalid states unrepresentable.
2. **Propagate** the failure so it stays visible in tests and production.
3. **Enrich** an unclear failure with context and preserve the original cause.
4. **Catch** only for a deliberate outcome, such as recovery, retry,
   compensation, or containment of explicitly best-effort work.

### Decide cohesive reorganization for already-oversized expected edits

When expected edits touch a file already over this project's
[file-size check](../../dough-post-change-refactor/references/refactor-checks.md#file-size),
including a comment-only change, settle that file's cohesive reorganization
before implementation. For each such expected edit, record in the plan:

- the file and its current size;
- cohesive responsibilities and seams for the reorganization;
- affected callers and preserved proof; and
- which slice owns the reorganization.

Assign necessary reorganization within the Behavior it enables or in an
immediately preceding Structure slice. Do not invent a repository-wide size
inventory or a parallel size policy; inspect only the files this work expects
to edit. Keep the existing numeric check and cohesive-seam requirement: a short
diff, a prior exception, or naming the file in the plan does not waive them.

When the reorganization cannot fit the current outcome under
[slice sizing](../../dough-story-decomposition/references/problem-decomposition.md#size-and-escalate-slices),
use that escalation and replanning path rather than compressing evidence or
cutting arbitrarily. When it conflicts with an Accepted ADR, needs unapproved
cross-subsystem work, or otherwise requires human judgment, stop the dependent
planning path with the conflict and the exact decision needed; continue only
independent work whose direction remains supported. A precisely scoped
developer decision recorded for that file, work, and conditions may authorize
the affected path; it does not become a standing waiver.

## Where enduring knowledge lives

Automated tests document enduring behavior, code documents enduring
architecture, and configuration documents operational setup. Enduring
architectural preferences live only in human-controlled ADRs, with any
area-specific ADR extensions in `docs/` also added only with human permission.
Do not create or update other enduring documents without explicit human
permission.

Unimplemented design or architectural direction for upcoming work may live in a
North Star topic. Once implemented, delete the topic completely and leave no
copy.

## Retire temporary direction during ordinary wrap-up

During ordinary story wrap-up, inspect the North Star topics that the completed
work cited, added, or revised. Treat a topic as a retirement candidate only when
the completed work has substantially realized it or remaining work no longer
needs it. Retire a selected candidate only after checking the current stories
and plans that its direction affects. If another active story still needs the
topic, keep the topic and its references.

When retiring a topic, delete its heading and text and remove or repair affected
references consistently. Do not preserve an archive, registry, monitor, or
completion record for the removed direction.
