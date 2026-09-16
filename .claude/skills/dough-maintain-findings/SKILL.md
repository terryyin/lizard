---
name: dough-maintain-findings
description: >-
  Adopts supplied, approved DD-NNN to ODF-NNN finding-name mappings in this
  project's DearDough.md while preserving evidence and former-code traceability.
  Use only on explicit skill invocation or an explicit request to adopt or rename
  finding codes; encountering findings or mapping recommendations does not
  authorize maintenance. Other finding cleanup and status changes are out of scope.
---

# Maintain finding names

Adopt approved finding names only when the user explicitly invokes this skill
or requests that maintenance. Do not run adoption as a side effect of a
retrospective, reconciliation recommendation, or other work. This increment
supports supplied `DD-NNN` → `ODF-NNN` mappings only; do not allocate codes,
reconcile names, merge findings, change statuses, or remove log content.

## Recover the request

Resolve the target project, approved mappings, and their concrete finding
meaning from the request and established session context. An explicit request
to adopt supplied recommendations is sufficient approval; do not ask again
when the user has already accepted them. An invocation alone does not approve
unaccepted recommendations. If mappings or approval are missing, identify the
missing input and make no rename.

Use this project's canonical `DearDough.md`: an explicitly established location
takes precedence over `<project-root>/DearDough.md`. Do not infer the project
from the installed skill directory, search other projects, or require a naming
catalog. If the target or canonical location is ambiguous, or the log is missing
or unreadable, explain the limitation and make no write. Do not create a log.

## Check each mapping

Read the log and evaluate all approved mappings before editing. Match identity
and concrete meaning, not just a similar title or symptom. A source code must
identify one finding in this project, and the supplied recommendation's meaning
must agree with that finding. Do not infer approval or meaning for another
project's same-numbered code.

- **Ready:** One matching `DD-NNN` heading exists and the destination `ODF-NNN`
  is unassigned to any other finding.
- **Already adopted:** The matching `ODF-NNN` heading and its former-code alias
  identify this same mapping, with no competing source identity. Leave it
  unchanged; do not add another alias or finding.
- **Blocked:** The source identity is missing or ambiguous, meaning is missing
  or mismatched, the destination belongs to another finding, or the requested
  mappings conflict with each other. Cite the conflicting codes and relevant
  log or recommendation text. Leave those findings unchanged and the identity
  decision with the user; do not merge, overwrite, or reinterpret the mapping.

Check aliases as well as headings for competing source identities. Repeated
identical mappings are one request; conflicting destinations for one source or
multiple sources for one destination block the involved mappings. An existing
destination heading without enough evidence to establish prior adoption is a
limitation, not permission to add an alias by assumption.

Apply independently unambiguous approved mappings even when another mapping
is blocked. If malformed content prevents safely isolating the affected
findings, leave the log unchanged and explain that broader limitation.

## Apply the smallest edit

For each ready mapping, replace only the finding heading's code. Keep its title
and existing body unchanged, and insert one short note immediately below the
heading: `Former local code: DD-NNN.` Reuse an equivalent existing alias without
duplicating it. Preserve occurrence rows, observations, inferences, resolution
and follow-up notes, unrelated findings, ordering, and formatting. Do not update
historical code mentions throughout the evidence; the alias supplies traceability.

For example, adopting the approved mixed-commit-provenance mapping changes
`## DD-001 — Mixed commit provenance` to
`## ODF-001 — Mixed commit provenance`, with `Former local code: DD-001.` below
it. The evidence remains intact. Repeating that accepted mapping makes no edit.

Before saving, check that the changes contain only the ready heading-code
replacements and necessary aliases. If the log changed since reading, recheck
the mappings against its current content rather than overwriting newer work.
Write only the canonical log; do not edit guidance, catalogs, or other files.
Verify the saved result and preservation of the existing content. Report a
failed or unverified write as such, never as successful adoption.

## Report

Give the canonical log path and the mappings applied, already adopted, and
blocked, with a concise reason for each block. State when the log is unchanged.
For a partial result, distinguish the saved mappings from those left for the
user to resolve. Name adoption does not mean the underlying finding is resolved.
