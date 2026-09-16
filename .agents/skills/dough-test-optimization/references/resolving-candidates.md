# Resolve recorded candidates

Run only for `--resolve`. Read the project's candidate record, or candidate
entries in the relevant optimization plan. If neither exists, report no
candidates; do not invent a record. Do not profile or optimize in this mode.

For each candidate, inspect its related test family and surviving proof across
boundaries, including referenced replacement tests. Separate unavoidable cost
of the protected behavior from repeated setup, live external calls, and redundant
proof. Consider whether a cheaper design preserves both fault detection and
user-value clarity before calling the cost inherent.

Resolve into one of these outcomes:

- **Profile-only exclusion:** use the project's existing mechanism at the
  narrowest justified scope when the cost is inherent. Explicit `--resolve`
  authorizes an evident exclusion only when that mechanism leaves normal test
  and CI execution intact and no project rule requires separate approval.
  Without such a mechanism, record the necessary-cost finding and ask only if
  an exclusion decision is needed; do not invent a tag or disable the test.
- **Replacement plan:** when cheaper proof preserves the protection, add the
  replacement/removal and verification work to one plan for this pass, reusing
  an applicable active plan. Follow
  [record the optimization plan](../SKILL.md#record-the-optimization-plan),
  including its resolve-only baseline handling, then return here. Planning here
  does not authorize implementation.
- **Human decision:** cite the concrete product, architectural, or confidence
  trade-off and keep the candidate pending. Reuse an already supplied decision;
  do not request it again.

Remove a candidate entry only after its exclusion or necessary-cost disposition
is recorded, or its replacement plan contains the obligation. Preserve pending
decisions and the record's established empty-section convention. Do not create
a resolved-history archive or claim a planned replacement was implemented.

Report each disposition and rationale, changed exclusions, remaining decisions,
and the plan location when applicable. Emit `## CANDIDATES RESOLVED` only when no
candidate decision remains pending. Stop without starting an optimization run.
