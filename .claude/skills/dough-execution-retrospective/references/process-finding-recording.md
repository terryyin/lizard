# Record supported process findings

Record compact observed facts, supported judgments, and open questions that remain
understandable after chat history is lost, with evidence locators where available.
Preserve findings and occurrences beyond story closure, subject to bounded
retention below or a separately authorized disposition.

## Classify the finding

Give each supported process finding one category. Use the first category its
evidence supports, and include the basis in its concise description.

1. **Engineering principle conflict or challenge.** Name the principle, cite
   the specific decision in the applicable Accepted ADR, and identify the
   observed choice that conflicts with it or the evidence that calls it into
   question. Follow [ADR awareness](../../dough-adr-awareness/SKILL.md) for
   supersession and human-approved exceptions.
2. **Concrete defect.** Cite the established guidance, accepted requirement,
   test or other evidence of the agreed behavior. State what should have
   happened and what actually happened, with evidence of the mismatch.
3. **Other.** Use this for everything else, including useful practices,
   friction without an established expectation, and observations whose cause
   is unclear. Describe what happened and its effect. The feedback cycle
   assesses possible model capability limits and the case for intervention.

Record the category once per finding. Reuse it on recurrence while the
evidence still supports it. An identical reread requires no edit.

## Record the evidence

Use the explicit user/project `DearDough.md` location or `<project-root>/DearDough.md` for enabled
process findings, resolved in the checkout chosen under
[write only in an owned checkout](../SKILL.md#write-only-in-an-owned-checkout).
Missing/conflicting root/location stops recording only: return findings and
continue independent reviews without inventing or searching elsewhere.

Reuse logged execution identity, else canonical plan/story plus first related implementation commit,
or a stable execution-record reference if no commit exists. Later commits/reviews/dates create no
identity. Missing/conflicting identity permits findings but no countable row or invented tracking.

When the log is missing, use this project's Git history to establish first use or recreation
before allocating. Run `git log --name-status --follow -- <log path>`, follow renames to former
paths, and read each earlier version with `git show <revision>:<path>`. Recovered DD/adopted-ODF
headings, aliases, and `Highest allocated local number` count as current content for matching and
numbering below. Cite recovered findings as `<revision>:<path> <ID>` so older findings sharing a
number stay distinguishable by revision.

- Complete history without an earlier log is first use.
- Recreation carries the recovered floor in the `## Retention` metadata of
  [bounded process-log recording](bounded-process-log.md#retain-higher-value-learning), with
  `Recovery: <revision>:<path>` of the version read.
- Shallow (`git rev-parse --is-shallow-repository` prints `true`) or otherwise incomplete
  history without an earlier log stops allocation: leave the log absent and name the missing
  evidence, such as full history of the log path and its former paths.

A new log assigns `DD-001` upward in supported-finding order, or continues above a recovered
floor, using:

```markdown
# DearDough Process Findings

## DD-001 — <descriptive issue title>
<category> — <concise description and its basis>

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
Recover history for a missing log or consequential identity/match questions, reusing established
removed IDs; a verified recurrence keeps its recovered heading code and any former alias.
Missing history that prevents safe identity resolution stops allocation. Otherwise an unmatched/
uncertain finding gets the next unused `DD-NNN`, with matching uncertainty stated when relevant.
Never renumber to fill gaps or restore a pruned occurrence on identical rereview.

Next DD number is one above the greatest of all current or recovered DD/adopted-ODF heading
numbers, their aliases, and retention metadata's highest allocated local number. Never reuse
removed IDs or collide across prefixes (`ODF-001` reserves 1). Update existing high-water metadata
on every allocation, even without pruning. Uncertain historical gaps cannot be filled; report
inability to allocate safely.

One execution means one row: identical/pruned rereview makes no edit; decisive new evidence or
corrected qualified conclusions enrich it, preserving prior notes. New executions add rows, symptoms do not.

For supported writes only, build the complete ordinary candidate, then follow
[bounded process-log recording](bounded-process-log.md) for measurement, retention,
writing/refusal, and recovery. Ambiguity, malformed content, unsafe retention, or write
failure leaves the log byte-identical; report limitations/findings and continue.

Retain and return the path, created IDs/rows or `unchanged`/`not recorded` with reason,
and required size/retention refusal. Claim no unsuccessful write as success.
Apply the [review response](../SKILL.md#report) to these recording results: a settled
record requires no visible recap; an unresolved refusal remains actionable.
