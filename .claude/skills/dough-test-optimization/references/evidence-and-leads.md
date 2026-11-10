# Read evidence and rank leads

Use this reference during discovery, and again when you measure an experiment
and report its gain.

## Read the retained CI proof

From this project's checkout, run the [lead reader](../scripts/ci-proof-leads.mjs):

```text
node <this skill's directory>/scripts/ci-proof-leads.mjs [--revision <rev>] [--scope <path>]...
```

It asks this project's configured CI, its GitHub Actions workflow or CI
adapter, for the attempt to read, checks the summary's identity, and prints
one bounded table. Pass each test location of the requested scope with
`--scope`; without it, the scope is this project's Intent locations. The
revision defaults to `HEAD`. Read only that table, never a raw summary. Exit 1
means nothing ranked, and the table says why.

Name the evidence before you use it:

- **Attempt.** The table names the repository, the requested revision, and
  the attempt read with its revision and outcome. The reader prefers a
  complete, successful attempt at the requested revision.
- **Selection.** When the Selection line says the test-selection command is
  not recorded, read the CI configuration at the attempt's revision. For a CI
  adapter, start with the adapter command on the Source line and follow it to
  the CI job it reports. Read that job's test command, filters, markers, and
  splits that ran each entry. Name that selection with the attempt. A
  selection you cannot confirm is a stated gap, and no comparison crosses it.
- **Compatibility.** An earlier attempt, at an ancestor of the requested
  revision, is a lead only within the parts that still apply. Set aside leads
  whose tests, shared setup, or code under test appear among the changed paths
  until the current code confirms them.
- **Failed or incomplete attempts.** Use them to diagnose problems: which
  parts failed, went missing, or ran out of time. Their timings are no
  baseline.
- **Gaps.** Missing, ambiguous, and untimed parts stay gaps; use the rest at
  its actual scope. Follow each gap's remedy. `--summary <file>` reads a
  summary this project retains elsewhere, such as one saved from the run's
  page. It must be the attempt CI chooses for the requested revision; to use
  another attempt's summary, pass `--revision` at that attempt's revision.

When no retained proof can be read, state that gap with the remedy the table
names, and continue only with evidence that does not depend on it. When the
developer asks for this project's CI to retain proof, follow the
[CI proof summary](ci-proof-summary.md) recipe.

## Read runner output by bounded extraction

When a lead needs timing the summary lacks, such as a file breakdown an
adapter answer omits, or hook, scenario, or worker timing, use this project's
retained full summary or runner output for the same attempt: a runner report,
a job log, or the CI job timings. Extract it with a small throwaway script,
kept outside this project, that prints only aggregates such as sums, shares,
and a few top rows. Never read a raw report or log into context.

Runner reports and job evidence separate what file totals mix:

- shared setup, such as per-test hooks and fixtures, from the test bodies;
- parallel work, per worker, shard, or job;
- queue time before a job or suite starts;
- the work that controls elapsed time: the job or shard that finishes last,
  and the tail after its other workers go idle.

When shared setup shows across many files, it is a lead of its own beside the
top files. When the evidence still runs out, for example once a report's
retention has passed, state the gap and stop the investigation that depends
on it. The remaining leads rest on what is still retained, such as the
summary's file totals.

## State the saving worth keeping

Before experimenting, state the smallest saving worth keeping for the scope:
a figure, the measure it is in, and why. Use a target the developer supplied.
Otherwise derive it from the evidence:

- the scope's total in that measure;
- how much the measure moves between comparable attempts or runs, such as the
  same unchanged files in the reader's tables naming distinct attempts. To
  reach an earlier attempt, request the revision before the previous table's
  attempt (`--revision <attempt revision>~1`). Check that the Attempt lines
  differ; tables naming the same attempt count as one observation;
- what would noticeably shorten the developer's wait.

A saving that the measure's own variation could hide needs repeated
comparable evidence to show, and a saving no developer would notice does not
pay for its change.

Choose the shortlist, the evidence source, and the investigation depth
yourself. Do not ask the developer for percentages, batch sizes, or modes.

## Rank removable causes

Within each unit (test cases, test files, or legacy test results), the reader
ranks items by summed CI time and lists the folders and the tail beyond them;
time and shares never cross units. That is cost, not removable cost. Rank leads by the cost an
experiment could plausibly remove: repeated setup, process starts, waits, and
duplicate proof remove much; a test whose time is the behavior it proves
removes little. Read the
[accepted-cost indicators](candidate-record.md#keep-accepted-cost-beside-its-test)
in a lead's tests before estimating what it could remove.

- Examine individually material files, families, and fixtures in detail.
- Group the many small costs by shared cause, such as per-case setup, process
  starts, waits, or a worker tail, so a cost that no single file shows can
  still lead.
- A mean per case can hide uneven costs. Before judging a lead's time
  inherent, read its cases for fixed waits, timeouts, and sleeps, or time
  those cases unmodified as discovery.

## Profile afresh only on request

A fresh profiling run is a run of the requested scope to discover or rank
costs. Start one only on the developer's explicit request, which can be
ordinary language, such as asking to time the whole suite. Run this project's
own command for that scope in its ordinary local mode, applying only its
existing profile-only markers, and report the limits of a filtered run. A
failed or incomplete run supplies diagnostics, not a baseline. Keep raw
profiles out of commits. Timing unmodified tests of a chosen lead during
discovery, or a chosen lead's family before and after its experiment, is not
a profiling run.

When a gap blocks a decision that could pay off, recommend such a run with
its expected cost, such as its duration and what it occupies, estimated from
the retained evidence, and leave the choice to the developer.

## Measure like for like

These measures are distinct; name which one each figure is:

- a CI case or file sum;
- a CI job or workflow wall time;
- the developer's wait, including any suite admission or queue.

Summed test durations are neither wall time nor CPU time. A claim about local
elapsed time requires comparable local evidence. A faster focused family is a
focused gain; a shorter wait for the developer needs comparable local
evidence for the whole scope. Record each local timing's revision, literal
command, runner mode, workers, filters, load, cache conditions, wall time,
executed cases, and available test and setup timings. Compare like selections
and runner conditions, including replacement tests' cost when proof moved
across boundaries and shared setup where measured. Startup, caches, machine
load, and admission or queue waits can dominate a timing: interleave or repeat
doubtful comparisons, or report them inconclusive. A timing claim stays within
its evidence; an unsupported comparison stays open rather than borrowing a
different measure.
