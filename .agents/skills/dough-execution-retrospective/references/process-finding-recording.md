# Record supported process findings

Record compact observed facts, supported judgments, and open questions that remain
understandable after chat history is lost, with evidence locators where available.
Preserve findings and occurrences beyond story closure, subject to bounded
retention below or a separately authorized disposition.

Use the explicit user/project `DearDough.md` location or `<project-root>/DearDough.md` for enabled
process findings, resolved in the checkout chosen under
[write only in an owned checkout](../SKILL.md#write-only-in-an-owned-checkout).
Missing/conflicting root/location stops recording only: return findings and
continue independent reviews without inventing or searching elsewhere.

Reuse logged execution identity, else canonical plan/story plus first related implementation commit,
or a stable execution-record reference if no commit exists. Later commits/reviews/dates create no
identity. Missing/conflicting identity permits findings but no countable row or invented tracking.

For a new log, assign `DD-001` upward in supported-finding order using:

```markdown
# DearDough Process Findings

## DD-001 — <descriptive issue title>
<concise concrete description>

### Occurrences
- Execution: <stable execution identity>
  - Timestamp: <ISO 8601 occurrence time with timezone | unknown>
  - Tool: <Codex, Cursor, Claude Code, or another identified tool>
  - Model: <model identifier, when available>
  - Open Dough release: <version | unknown | unreleased | modified>
  - Evidence: <decisive compact references or locators>
  - Observed effect: <what the record shows>
  - Inference: <qualified cause, cost, or uncertainty, only when needed>
```

Use compact references and separate observation/inference. Rows count retained occurrences,
not all-time recurrence. Record supported one-offs, practices, potential general issues, and
retrospective observations with qualified generality. For each new occurrence:

- **Timestamp:** actual event time, ISO 8601 with timezone, from execution evidence or live clock;
  otherwise `unknown`, with available dates/ranges in Evidence. Never substitute review/import/
  nearby-commit times or invent date precision. Preserve timestamps; fill unknown only with event
  evidence. Older timestamp-free rows remain valid without replay rewrites.
- **Tool/model:** identify the executing tool, not reviewer; omit Model when execution evidence
  supplies none, without guessing or separate lookup. Unidentified tool means no countable row.
  Backfill older rows only with supporting evidence.
- **Release:** execution-time guidance provenance, otherwise `unknown`; not product version or
  today's checkout/installation `VERSION` unless tied to this work. Mark unreleased/modified
  guidance with available revision/base release, e.g. `modified; revision <rev>; base <version>`.
  Never mislabel it a clean release, guess, or backfill older releases; identical rereview stays unchanged.

Existing headings/descriptions/rows must safely identify issues, executions, and next ID.
Use only this log's IDs; preserve DD/adopted ODF codes, mint only `DD-NNN`. Keep notes, evidence,
release rows, and unrelated content; make the smallest supported edit. Migration, normalization,
reordering, deletion, or merging requires bounded retention below.

Match the same concrete problem/practice by decisive evidence, not wording/symptoms; reuse its code.
Recover history only for consequential identity/match questions, reusing established removed IDs.
Missing history that prevents safe identity resolution stops allocation. Otherwise an unmatched/
uncertain finding gets the next unused `DD-NNN`, with matching uncertainty stated when relevant.
Never renumber to fill gaps or restore a pruned occurrence on identical rereview.

Next DD number is one above the greater of all current DD/adopted-ODF heading numbers and
retention metadata's highest allocated local number. Never reuse removed IDs or collide across
prefixes (`ODF-001` reserves 1). Update existing high-water metadata on every allocation, even
without pruning. Uncertain historical gaps cannot be filled; report inability to allocate safely.

One execution means one row: identical/pruned rereview makes no edit; decisive new evidence or
corrected qualified conclusions enrich it, preserving prior notes. New executions add rows, symptoms do not.

For supported writes only, build the complete ordinary candidate, then follow
[bounded process-log recording](bounded-process-log.md) for measurement, retention,
warnings, writing/refusal, and recovery. Ambiguity, malformed content, unsafe retention, or write
failure leaves the log byte-identical; report limitations/findings and continue.

Report path, created IDs/rows or `unchanged`/`not recorded` with reason, and required size/retention
warnings or refusal. Claim no unsuccessful write as success. The final banner requires an evidenced
overlooked request, decision, warning, failed verification, or Jidoka stop still needing user action.
