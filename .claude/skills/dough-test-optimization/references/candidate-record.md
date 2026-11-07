# Test optimization candidate record

Own every recorded finding in this project's
`.planning/TEST-OPTIMIZATION-CANDIDATES.md`. Head it exactly
`# Test Optimization Candidates`. The record tracks findings and their
dispositions. Raw profiles stay outside it.

## Sections (required order)

1. `## Pending candidates` — proposed changes not yet implemented or decided,
   including those awaiting a named human decision.
2. `## Accepted necessary costs` — cost judged inherent to unique protected
   behavior. Acceptance is a decision record only: it filters, skips, or
   excludes nothing. Normal verification and CI keep running the test. An
   accepted cost is not an exclusion; later passes keep it unless new evidence
   supports moving it back to pending.
3. `## Profile-only exclusions` — exclusions actually applied through this
   project's existing mechanism. Name that mechanism and its scope. Do not
   invent a mechanism here.
4. `## Transferred obligations` — candidates whose work now lives in a named
   replacement story or plan. Keep the entry as a pointer until that work
   closes; never claim the replacement was implemented.

An empty finding section holds exactly `None.` A record whose every finding
section is `None.` is an explicit empty record.

## Entry shape

Each finding is a `###` entry. Its heading is a short kebab-case slug, unique
in the record and unchanged when the entry moves between sections. Use these
labeled fields:

- **Locations / scope:**
- **Observed cost:** (include measurement conditions)
- **Unique protection:**
- **Alternatives attempted:**
- **Disposition / rationale:**
- **Date:**
- **Human decision needed:** (omit when none)
- **Evidence / follow-up:**

## Read and write rules

- **Missing file:** no pass has recorded findings in this project. A pass that
  records a finding creates the file from this format; it needs no
  pre-existing file. `--resolve` reports the absence without creating it.
- **Unreadable or mismatched:** stop the write, report the problem, and never
  overwrite or guess at the file.
- **Later passes:** read this record first. Preserve recorded decisions.
  Move an accepted cost back to pending only with new evidence. Earlier records
  may have a leading pass summary; remove it when updating the findings while
  preserving their decisions and evidence.
- **Lifecycle:** pending → accepted necessary cost, profile-only exclusion, or
  transferred obligation (or stay pending with its human decision). Before a
  spent plan is deleted, move any finding that exists only in that plan into
  this record. When the receiver of a transferred obligation closes with its
  work done, remove that entry; return any unfinished part to pending. Do not
  delete this record as spent history.

## Compact template

```markdown
# Test Optimization Candidates

## Pending candidates

### example-slug

- **Locations / scope:** …
- **Observed cost:** …
- **Unique protection:** …
- **Alternatives attempted:** …
- **Disposition / rationale:** pending — …
- **Date:** YYYY-MM-DD
- **Human decision needed:** … (omit when none)
- **Evidence / follow-up:** …

## Accepted necessary costs

None.

## Profile-only exclusions

None.

## Transferred obligations

None.
```
