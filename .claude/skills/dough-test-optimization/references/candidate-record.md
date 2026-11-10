# Test optimization candidate record

Keep this project's live test-optimization candidates in
`.planning/TEST-OPTIMIZATION-CANDIDATES.md`, headed exactly
`# Test Optimization Candidates`. Raw profiles and pass summaries stay
outside it.

## What the record holds

The record holds only live candidates. A live candidate is a material cost
that still needs deeper developer analysis **and** protects valuable behavior
that you cannot safely simplify, downgrade, or remove. A cost that fails
either condition does not enter the record; resolve a clear opportunity
during the optimization itself.

A candidate leaves the record when it is fixed,
[accepted beside its owning test](#keep-accepted-cost-beside-its-test), or
moved to a named story. Delete the entry and keep no resolved archive: its
evidence stays recoverable from the record's Git history and from the work
that resolved it, and each live decision keeps one home.

## Keep accepted cost beside its test

Accept a cost when it is inherent to unique protected behavior and no cheaper
design preserves that protection. Record the acceptance beside the test,
scenario, or shared fixture that owns the cost, at the narrowest scope that
owns it: the one case or scenario, even when CI times only its whole file or
feature, or the shared fixture or step when each of its users pays that same
cost. [Runner output](evidence-and-leads.md#read-runner-output-by-bounded-extraction)
can time a case that the summary does not. Use this project's native form: a
tag or annotation with its reason beside it, or a short source comment.
Explain why the cost is warranted: the work it pays for, the behavior only it
protects, and the cheaper proof ruled out. Kept in source, the indicator
moves with its test.

An accepted-cost indicator changes no selection, execution, timeout, or
verdict, because normal verification and CI must keep proving the behavior it
protects.

When you rank leads, read the indicators in the leads' tests. An accepted
cost still counts in every timing and total, but not in a lead's removable
cost while its reason holds. It covers only the cost it explains: its file's
other cases, and avoidable setup around it, such as hooks, fixtures, and
waits, stay open to improvement.

Revisit an accepted cost when it grows materially beyond the work its
indicator names, when its justification changes, or when new evidence
identifies cheaper proof of the same behavior. Judge it as a candidate again,
then update or remove its indicator with the result.

Existing execution tags and profile-only markers keep their own meaning. Read
each by what this project defines it to do: a tag that selects tests or
names a host service they need still governs selection and execution, and a
profile-only marker governs what a requested profiling run measures. Only an
accepted-cost indicator records an accepted cost.

## Live format

Under the title, write one `## Live candidates` section. It holds `###`
entries, or exactly `None.` when no candidate is live. Head each entry with a
short kebab-case slug, unique in the record and unchanged while the candidate
stays live. Give each entry these five labelled fields:

- **Affected test or fixture:** the test, scenario, or shared fixture that
  carries the cost, by repository path, with `::<case>` or `:<line>` for one
  case.
- **Observed cost and conditions:** the cost and how it was observed: its
  measure, such as a CI case or file sum, a CI wall time, or a focused local
  time; the attempt or revision and date; and the runner conditions, such as
  workers and host load.
- **Unique protection:** the valuable behavior only this proof protects.
- **Alternatives tried:** each cheaper design tried or ruled out, with its
  result.
- **Decision or evidence needed:** the developer decision, or the named
  evidence, that would let the candidate leave the record.

```markdown
# Test Optimization Candidates

## Live candidates

### example-slug

- **Affected test or fixture:** `path/to/test_file::test_case`
- **Observed cost and conditions:** …
- **Unique protection:** …
- **Alternatives tried:** …
- **Decision or evidence needed:** …
```

## Read either version

Read the live record, or the four-section record this format replaces. A
four-section record has the same title, optionally a leading
`## Latest pass` summary, then these sections in order, each `None.` or
`###` entries:

1. `## Pending candidates` — candidates not yet decided, including those
   awaiting a named human decision.
2. `## Accepted necessary costs` — costs judged inherent to unique protected
   behavior.
3. `## Profile-only exclusions` — exclusions applied through this project's
   existing mechanism. The section may open with a note naming that
   mechanism and its scope before its entries.
4. `## Transferred obligations` — candidates whose work moved to a named
   story or plan.

Its entries are labelled **Locations / scope**, **Observed cost**, **Unique
protection**, **Alternatives attempted**, **Disposition / rationale**,
**Date**, optional **Human decision needed**, and **Evidence / follow-up**.
Judge each pending entry as live or resolve it. The other sections' recorded
decisions stand: reopen an accepted cost only with new evidence, and leave a
transferred obligation to its named story or plan.

## Read and write rules

- **Missing file:** no pass has recorded a live candidate. A pass that
  records one creates the file in the live format; it needs no pre-existing
  file. `--resolve` reports the absence without creating it.
- **Unreadable or mismatched:** a file in neither version stops the write.
  Report the problem and never overwrite or guess at the file.
- **Later passes:** read this record first and preserve its recorded
  decisions. Write the live format.
- **First write converts:** a pass that writes a four-section record
  rewrites it in the live format. Pending entries judged live become live
  entries. Each recorded acceptance that current code and evidence still
  substantiate moves beside its test with its rationale; judge any other as
  a candidate. Fixed entries, transferred obligations, and profile-only
  entries leave the record; the markers stay in source with their own
  meaning.
- **Cleanup:** before a spent plan is deleted, write each candidate that
  exists only in that plan and is still live into this record. When the
  story or plan that received a moved candidate or transferred obligation
  closes, write any unfinished part that is still live back into this
  record. Do not delete this record as spent history.
