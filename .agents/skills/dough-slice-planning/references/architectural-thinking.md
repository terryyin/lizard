# Architectural thinking during slice planning

Use architectural thinking while constructing the plan; do not delegate it to
a separate architecture role or add an approval stage. Start with
[dough-pfe](../../dough-pfe/SKILL.md) as the authoritative way to find and
assess existing solutions. Carry its evidenced reuse, change, modularization,
or gap decision into the slices instead of repeating its search procedure here.

## Use established direction first

Read the story, the PFE result, relevant existing product structure, domain
meaning, and this project's Accepted ADRs. Also inspect the project's
established North Star location when it has one. Judge whether the selected
solution depends on a consequential architectural choice and which current or
upcoming stories that choice would constrain. A choice is consequential when,
for example, it establishes shared responsibility or a product boundary,
changes domain meaning, or commits affected work to a difficult-to-reverse
structure.

When established structure and accepted decisions already support the work,
write an ordinary sufficient plan. Do not create or update a North Star topic,
architecture report, reference registry, or extra approval step merely because
planning occurred.

When an existing short-term topic governs a consequential choice, cite its
location and topic in the plan and state how the selected solution follows it.
Revise the topic only when current supporting evidence warrants a different or
clearer direction. Consider the consequences for affected stories, domain
meaning, and relevant Accepted ADRs before revising it; do not turn uncertainty
or a hypothetical later need into a constraint.

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

If consequential, evidence-supported direction is needed and no existing topic
covers it, add one short topic at the project's established North Star location.
Use a heading and a brief paragraph that state the direction, supporting
evidence, and affected work. Refer to that topic from the plan.

If the project has no North Star location, the planner may choose one shared
`NORTH-STAR.md` under this project's established planning root and state that
location in the plan. Do this only when the new topic is warranted; do not
require a configuration setting or create the file for an ordinary plan.

A North Star topic cannot override an Accepted ADR or become the sole home of
an indispensable architectural assumption. Humans retain decisions about
domain meaning and ADR acceptance, exceptions, and replacement. When
indispensable domain or ADR context is missing or conflicts with the proposed
direction, stop the dependent planning path with the evidence and decision
needed. Continue independent planning work where safe, but do not present the
affected path as executable.

## Keep current work sufficient

Include any Structure slice required to make the selected solution coherent for
the current story, even when that work crosses components or process boundaries.
Place it immediately before the Behavior it enables and prove preserved external
behavior. Do not add structure, topics, or flexibility solely for hypothetical
later stories.

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

The North Star cannot be the sole durable home of indispensable architectural
context. Before deleting a topic that carries such context, require that context
in this project's applicable durable, human-owned decision home, such as its ADR
process, and resolve ownership through that existing decision process. Until
that is resolved, keep the topic and its references, report the blocking gap,
and do not claim closure. Do not turn retirement into a new review or approval
ceremony.
