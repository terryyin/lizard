// Collects one attempt's CI proof fragments into its summary: a GitHub
// Actions run attempt's or a CI adapter's attempt's (ci-proof-contract.mjs).
// The declaration is the expectation: fragments present never add a part. A
// fragment from another attempt, of either identity, or another revision, or
// for an undeclared part, is excluded; a declared part with no fragment is
// missing, and one with more than one is ambiguous. Neither is counted. A
// present part's file breakdown, when its fragment has one, adds to its
// entry's per-path figures; a part without one leaves them partial. Node
// built-ins only, like the rest of the collector (ci-proof-summary.mjs).
import {
  COUNT_FIELDS,
  SUMMARY_SCHEMA_VERSION,
  attemptOf,
  fragmentProblems,
  summaryAttempt,
  zeroCounts,
} from "./ci-proof-contract.mjs";
import {
  addedFigures,
  breakdownOf,
  emptyFigures,
} from "./ci-proof-figures.mjs";

// An entry's file breakdown summed over its broken-down present FRAGMENTS,
// or nothing when none was broken down.
function entryBreakdown(fragments) {
  const brokenDown = fragments.filter((value) => value.files !== undefined);
  if (brokenDown.length === 0) return {};
  const byPath = new Map();
  const add = (path, figures) =>
    byPath.set(path, addedFigures(byPath.get(path) ?? emptyFigures(), figures));
  for (const value of brokenDown) {
    for (const { path, ...figures } of value.files) add(path, figures);
    if (value.unassigned !== undefined) add(undefined, value.unassigned);
  }
  return breakdownOf(byPath);
}

// Builds the summary of RUN, the attempt collected, from the fragments read.
// Returns the summary and a note for every fragment excluded from it.
export function summarize({ fragments, expect, run }) {
  const notes = [];
  const byPart = new Map(expect.map(({ entry }) => [entry, new Map()]));
  const declared = new Map(expect.map(({ entry, parts }) => [entry, parts]));
  for (const { source, value } of fragments) {
    const problems = fragmentProblems(value);
    const exclude = (reason) => notes.push(`excluded ${source}: ${reason}`);
    if (problems.length > 0) exclude(problems.join("; "));
    else if (attemptOf(value) !== attemptOf(run)) {
      exclude(`${attemptOf(value)}, not ${attemptOf(run)}`);
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
    let durationMs;
    let untimed;
    const counted = [];
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
      const present = { part, status: "present", ...counts };
      if (only.durationMs !== undefined) {
        present.durationMs = only.durationMs;
        durationMs = (durationMs ?? 0) + only.durationMs;
      }
      if (only.untimed !== undefined) {
        present.untimed = only.untimed;
        untimed = (untimed ?? 0) + only.untimed;
      }
      if (only.files !== undefined) present.breakdown = true;
      counted.push(only);
      return present;
    });
    if (units.size > 1) {
      throw new Error(
        `entry ${entry} has parts counted in ${[...units].join(", ")}`,
      );
    }
    return {
      path: entry,
      unit: [...units][0] ?? null,
      ...totals,
      ...(durationMs === undefined ? {} : { durationMs }),
      ...(untimed === undefined ? {} : { untimed }),
      parts,
      ...entryBreakdown(counted),
    };
  });
  return {
    summary: {
      schemaVersion: SUMMARY_SCHEMA_VERSION,
      repository: run.repository,
      ...summaryAttempt(run),
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
