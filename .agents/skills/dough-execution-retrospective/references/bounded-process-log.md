# Bound process-log recording

Use this after [Select reviews](../SKILL.md#select-reviews) has enabled process
review and the recording rules have produced a supported edit. Do not load it
to skip process review, inspect a skipped or unresolved log, or size-check a
no-op.

## Count physical lines

Count every physical text line in the existing file and in the complete
candidate, including blank lines, headings, descriptions, occurrence rows, human
notes, and any other metadata. A final line that is not newline-terminated
counts once. An empty or missing file is 0 existing lines. Terminating an
unterminated last line does not add a line; new content after that terminator
does. Do not use `wc -l` as the definition; it misses an unterminated last line.

## Candidate-write flow

1. Construct the complete ordinary candidate, including blanks, metadata, and
   the intended last line, before touching the file. Keep ordinary maintenance:
   do not delete, reorder, migrate, or merge existing content when there is no
   size pressure. Do not fabricate condensed findings.
2. Measure the existing file with the same count.
3. Measure the complete ordinary candidate. A successful write may be at most
   1,000 lines. Exactly 1,000 is allowed; 1,001 is not.
4. If the existing file already exceeds 1,000 lines, or the ordinary candidate
   would exceed 1,000 lines, do not write that overflowing candidate. Form a
   bounded candidate using [Retain higher-value learning](#retain-higher-value-learning)
   only when that section's conditions hold. If a safe bounded candidate cannot
   be formed, leave the original file unchanged and report the limitation:
   existing-violation when the existing file already exceeds 1,000, or a
   ceiling refusal for an overflowing candidate. Do not claim repair.
5. If the existing file is already at least 500 lines and this write is
   accepted, include a 500-line threshold warning in the recording result.
   Below 500, do not emit that warning, even when this write will cross 500.
6. Write the accepted candidate once. Preserve the existing file on failed
   validation or write; do not truncate first and reconstruct afterward.

## Retain higher-value learning

Use this only inside the candidate-write flow when the ordinary candidate would
exceed 1,000 lines, or the existing file already exceeds 1,000 and a supported
write is still required. Occurrence-detail removal and whole-issue removal are
choices within this rule, not a separate storage system or archive. Do not add
automatic commits, remote storage, backup directories, scoring engines, or a
new archive file.

Compare the new information with existing material using supported impact,
likely recurrence, current actionability, and evidence quality. Do not assign
numeric scores. A severe one-off can outrank a frequent minor inconvenience.
Age, file position, recurrence count alone, and text length are not priority
rules.

Prefer removing redundant detail or lower-value occurrences before removing a
whole finding when that preserves more learning. A whole low-priority issue,
including one at the top of the file, may be removed. Do not strip decisive
evidence from a retained issue to meet size. Do not fabricate a condensed
replacement for removed material.

Retention is allowed only when all of these hold:

- the new information is higher priority than the material that would be
  removed
- enough lower-priority material can be replaced for a complete candidate of
  at most 1,000 lines
- a recoverable exact copy of the affected content exists through this project's
  history or an explicitly established recovery method
- the recovery reference includes any affected uncommitted content; `HEAD`
  alone is insufficient if uncommitted edits would be removed
- identity of retained issues stays interpretable
- matching and deduplication remain evidence-based after removal

If the new information is not higher priority, cannot fit even after justified
replacements, recovery is unavailable or ambiguous, identity cannot be resolved
safely, or the write would fail: leave the original file unchanged and return the
finding with the limitation. Continue independently supported reviews.

Before removal, verify recovery: the stated reference actually contains the
bytes that would be removed. An already oversized log must be reduced to at most
1,000 lines in the same accepted candidate; otherwise report the existing
violation without claiming repair.

Keep compact retention metadata in the same log; it counts toward the 1,000-line
ceiling. Write or update one `## Retention` section that is not an issue
heading:

```markdown
## Retention

- Highest allocated local number: <N>
- Recovery: <usable reference that contains the removed bytes>
- Occurrence history is partial
```

`N` is the high-water from
[Record supported process findings](../SKILL.md#record-supported-process-findings):
the highest local number ever allocated in this log, including removed
issues. Never lower it.

## Recording result

For an accepted ordinary write, report the canonical path, created or updated
issue IDs with occurrence rows, the measured final size, and the 500-line
warning when step 5 applies.

For an accepted bounded write, report the same, plus the removals, priority
rationale, and recovery reference.

For a refused ceiling, existing-violation, failed validation, failed write, or
refused retention, report `not recorded` or `unchanged` with the reason.
Never describe those cases as successful writes.
