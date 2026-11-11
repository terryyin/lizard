# Resolve recorded candidates

Run only for `--resolve`. Read this project's
[candidate record](candidate-record.md) first, in either version. Follow that
reference for path, format, missing-file reporting without creating the file,
and stopping without edits when the record is unreadable or mismatched. Do
not invent a record. Do not profile or optimize in this mode.

When the file is missing, report that no candidates have been recorded and
stop without creating it. When no candidate awaits judgment, because
`## Live candidates` or a four-section record's `## Pending candidates` is
`None.`, report that and stop. Preserve a four-section record's other
recorded decisions, as that reference requires.

For each candidate, inspect its related test family and surviving proof
across boundaries, including referenced replacement tests. Separate
unavoidable cost of the protected behavior from repeated setup, live external
calls, and redundant proof. Use the
[optimization approaches](optimization-tactics.md) to assess whether a cheaper
design preserves fault detection and user-value clarity before calling the cost
inherent.

Resolve each candidate into one of these outcomes:

- **Fixed:** current code or retained CI proof shows the cost is gone, for
  example because later work removed it. The entry leaves the record.
- **Accepted:** the cost meets the
  [acceptance rule](candidate-record.md#keep-accepted-cost-beside-its-test).
  Record the acceptance beside the owning test as that rule says; the entry
  leaves the record.
- **Moved to a named story:** cheaper proof preserves the protection. Add the
  replacement or removal and its verification work to one plan for this
  pass, reusing an applicable active plan. Follow
  [record the optimization plan](../SKILL.md#record-the-optimization-plan),
  including its resolve-only baseline handling, then return here. The entry
  leaves the record; that plan's story holds the work. Planning here does
  not authorize implementation; never claim the replacement was implemented.
- **Still live:** the candidate needs a developer decision or named evidence.
  Cite the concrete product, architectural, or confidence trade-off, or name
  the evidence, under **Decision or evidence needed**, and keep its slug.
  Reuse an already supplied decision; do not request it again.

Write the result in the live format, as that reference says, with no
resolved-history archive.

Report each outcome and rationale, remaining decisions, and the plan location
when applicable. Emit `## CANDIDATES RESOLVED` only when no candidate remains
live. Stop without starting an optimization run.
