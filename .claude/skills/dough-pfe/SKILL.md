---
name: dough-pfe
description: >-
  Finds and assesses existing solutions across the whole product before an
  implementation responsibility is added or relocated. Use for PFE, Proudly
  Found Elsewhere, or when deciding whether to reuse, change, modularize, or
  replace an existing product solution.
---

# Find and use a suitable existing solution

Given an implementation responsibility, find a domain-correct way to use what
this product already knows before adding another representation of the same
solution.

## Establish the responsibility

Use the selected story or plan, current product behavior, and this project's
domain language and applicable decisions to state the responsibility, its
purpose, and genuine constraints. Resolve this project's navigation and product
boundaries. If the responsibility or the domain meaning needed to assess it is
missing, stop the affected work and ask the developer for that specific context.

Project decisions remain authoritative. This skill does not grant permission to
override an architectural decision, expand the assignment, or alter unrelated
behavior.

## Search across the product

Search broadly enough to find conceptual solutions, not just matching names.
Follow domain concepts, behavior, callers, tests, documentation, data flow, and
ownership across the whole product. Include other components, services, tools,
and process boundaries when the responsibility can meaningfully cross them.

Judge a candidate by its domain meaning: the responsibility it owns, the purpose
it serves, its rules and invariants, and its lifecycle. Different structure or
names do not rule out a fit; similar-looking code does not establish one. Stop
searching once the evidence is sufficient to make or rule out a suitable choice.

## Choose a domain-coherent outcome

Use the smallest justified outcome:

- reuse a suitable solution directly;
- change the existing solution when the responsibility belongs there;
- modularize it to expose a suitable part, including the necessary current
  structural work while preserving its original purpose and behavior; or
- make the gap explicit and add a new solution when no candidate has the same
  domain meaning.

Explain the candidate evidence and why the choice preserves one coherent
representation of the responsibility. Do not force unrelated responsibilities
together to remove superficial duplication, and do not add machinery for
hypothetical future reuse.

If domain meaning remains unresolved, or the choice has consequential product
or architectural effects that are not already authorized, stop that path. Give
the developer the competing interpretations, decisive evidence, and the choice
needed to continue.
