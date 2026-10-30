# Resolve recorded candidates

Run only for `--resolve`. Read this project's
[candidate record](candidate-record.md) first. Follow that reference for path,
sections, entry shape, empty `None.` sections, missing-file reporting without
creating the file, and stopping without edits when the record is unreadable or
mismatched. Do not invent a record. Do not profile or optimize in this mode.

When the file is missing, report that no findings have been recorded and stop
without creating it. When every finding section is `None.`, report an empty
record and stop. Preserve recorded decisions already under accepted costs,
profile-only exclusions, and transferred obligations; move an accepted cost
back to pending only with new evidence, as that reference requires.

For each pending candidate, inspect its related test family and surviving
proof across boundaries, including referenced replacement tests. Separate
unavoidable cost of the protected behavior from repeated setup, live external
calls, and redundant proof. Consider whether a cheaper design preserves both
fault detection and user-value clarity before calling the cost inherent.

Resolve into one of these outcomes, keeping the entry's kebab-case slug
unchanged when it moves:

- **Accepted necessary cost:** when the cost is inherent to unique protected
  behavior and no project exclusion mechanism applies (or none should be
  applied), move the entry to `## Accepted necessary costs` under that
  reference's acceptance rule. Leave `## Profile-only exclusions` unchanged
  unless an exclusion was actually applied.
- **Profile-only exclusion:** use the project's existing mechanism at the
  narrowest justified scope when the cost is inherent. Explicit `--resolve`
  authorizes an evident exclusion only when that mechanism leaves normal test
  and CI execution intact and no project rule requires separate approval.
  When the mechanism was actually applied, move the entry to
  `## Profile-only exclusions` under that reference. Without such a
  mechanism, record the necessary-cost finding under
  `## Accepted necessary costs` and ask only if an exclusion decision is
  needed; do not invent a tag or disable the test, and do not write under
  `## Profile-only exclusions`.
- **Replacement plan:** when cheaper proof preserves the protection, add the
  replacement/removal and verification work to one plan for this pass, reusing
  an applicable active plan. Follow
  [record the optimization plan](../SKILL.md#record-the-optimization-plan),
  including its resolve-only baseline handling, then return here. Move the
  entry to `## Transferred obligations` under that reference with a link to
  that replacement plan. Planning here does not authorize implementation;
  never claim the replacement was implemented.
- **Human decision:** cite the concrete product, architectural, or confidence
  trade-off and keep the candidate under `## Pending candidates` with that
  decision recorded. Reuse an already supplied decision; do not request it
  again.

After each disposition, leave empty finding sections per that reference. Do
not create a resolved-history archive or claim a planned replacement was
implemented.

Report each disposition and rationale, changed exclusions, remaining
decisions, and the plan location when applicable. Emit
`## CANDIDATES RESOLVED` only when no candidate decision remains pending.
Stop without starting an optimization run.
