// Collects one run attempt's CI proof fragments into its summary. The
// declaration is the expectation: fragments present never add a part. A
// fragment from another attempt or revision, or for an undeclared part, is
// excluded; a declared part with no fragment is missing, and one with more
// than one is ambiguous. Neither is counted. Node built-ins only, like the
// rest of the collector (ci-proof-summary.mjs).
import {
  COUNT_FIELDS,
  SUMMARY_SCHEMA_VERSION,
  fragmentProblems,
  zeroCounts,
} from "./ci-proof-contract.mjs";

// Builds the run attempt's summary from the fragments read. Returns the
// summary and a note for every fragment excluded from it.
export function summarize({ fragments, expect, run }) {
  const notes = [];
  const byPart = new Map(expect.map(({ entry }) => [entry, new Map()]));
  const declared = new Map(expect.map(({ entry, parts }) => [entry, parts]));
  for (const { source, value } of fragments) {
    const problems = fragmentProblems(value);
    const exclude = (reason) => notes.push(`excluded ${source}: ${reason}`);
    if (problems.length > 0) exclude(problems.join("; "));
    else if (value.attempt !== run.attempt) {
      exclude(`attempt ${value.attempt}, not ${run.attempt}`);
    } else if (value.revision !== run.revision) {
      exclude(`revision ${value.revision}, not ${run.revision}`);
    } else if (!declared.has(value.entry)) {
      exclude(`entry ${value.entry} is not declared`);
    } else if (value.of !== declared.get(value.entry)) {
      exclude(
        `of ${value.of}, but ${value.entry} declares ${declared.get(value.entry)} parts`,
      );
    } else {
      const parts = byPart.get(value.entry);
      parts.set(value.part, [...(parts.get(value.part) ?? []), value]);
    }
  }
  const entries = expect.map(({ entry, parts: declaredParts }) => {
    const found = byPart.get(entry);
    const totals = zeroCounts();
    const units = new Set();
    const parts = [...Array(declaredParts).keys()].map((index) => {
      const part = index + 1;
      const candidates = found.get(part) ?? [];
      if (candidates.length === 0) return { part, status: "missing" };
      if (candidates.length > 1) return { part, status: "ambiguous" };
      const [only] = candidates;
      units.add(only.unit);
      const counts = Object.fromEntries(
        COUNT_FIELDS.map((field) => [field, only[field]]),
      );
      for (const field of COUNT_FIELDS) totals[field] += counts[field];
      return { part, status: "present", ...counts };
    });
    if (units.size > 1) {
      throw new Error(
        `entry ${entry} has parts counted in ${[...units].join(", ")}`,
      );
    }
    return { path: entry, unit: [...units][0] ?? null, ...totals, parts };
  });
  return {
    summary: {
      schemaVersion: SUMMARY_SCHEMA_VERSION,
      repository: run.repository,
      runId: run.runId,
      runAttempt: run.attempt,
      revision: run.revision,
      entries,
    },
    notes,
  };
}

// One line per entry for the CI log, naming every gap.
export function describeSummary(summary) {
  return summary.entries.map((entry) => {
    const of = entry.parts.length;
    const gaps = entry.parts
      .filter((part) => part.status !== "present")
      .map((part) => `part ${part.part} of ${of} ${part.status}`);
    const counts = `${entry.passed} passed, ${entry.failed} failed, ${entry.other} other ${entry.unit ?? "(no unit)"}`;
    return [`${entry.path}: ${counts}`, ...gaps].join("; ");
  });
}
