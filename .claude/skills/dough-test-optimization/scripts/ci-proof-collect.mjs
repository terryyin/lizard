// Collects one attempt's CI proof fragments into its summary: a GitHub
// Actions run attempt's or a CI adapter's attempt's (ci-proof-contract.mjs).
// The declaration is the expectation: fragments present never add a part. A
// declaration names an entry, its part count and the unit its
// parts are counted in (`case` or `file`): only that unit's fragments fill
// it, and its evidence keeps that unit even when no fragment arrived.
// A fragment from another attempt, of either
// identity, or another revision, or for an undeclared entry, unit or part
// count, is excluded with a note; a declared part with no fragment is
// missing, and one with more than one is ambiguous. Neither is counted, and
// nothing is added across units. A present part's file breakdown, when its
// fragment has one, adds to its evidence's per-path figures; a part without
// one leaves them partial. Node built-ins only, like the rest of the
// collector (ci-proof-summary.mjs).
import {
  COUNT_FIELDS,
  DECLARED_UNITS,
  SUMMARY_SCHEMA_VERSION,
  attemptOf,
  fragmentProblems,
  summaryAttempt,
  unitNoun,
  zeroCounts,
} from "./ci-proof-contract.mjs";
import {
  addedFigures,
  breakdownOf,
  emptyFigures,
} from "./ci-proof-figures.mjs";

// An evidence's file breakdown summed over its broken-down present
// FRAGMENTS, or nothing when none was broken down.
function evidenceBreakdown(fragments) {
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

// The evidence of one declaration from the fragments found for its parts.
function evidenceOf({ parts: declared, unit, found }) {
  const candidatesOf = (part) => found.get(part) ?? [];
  const totals = zeroCounts();
  let durationMs;
  let untimed;
  const kept = [];
  const parts = [...Array(declared).keys()].map((index) => {
    const part = index + 1;
    const candidates = candidatesOf(part);
    if (candidates.length === 0) return { part, status: "missing" };
    if (candidates.length > 1) {
      return { part, status: "ambiguous" };
    }
    const [only] = candidates;
    const counts = Object.fromEntries(
      COUNT_FIELDS.map((field) => [field, only[field]]),
    );
    for (const field of COUNT_FIELDS) totals[field] += counts[field];
    const shown = { part, status: "present", ...counts };
    if (only.durationMs !== undefined) {
      shown.durationMs = only.durationMs;
      durationMs = (durationMs ?? 0) + only.durationMs;
    }
    if (only.untimed !== undefined) {
      shown.untimed = only.untimed;
      untimed = (untimed ?? 0) + only.untimed;
    }
    if (only.files !== undefined) shown.breakdown = true;
    kept.push(only);
    return shown;
  });
  return {
    unit,
    ...totals,
    ...(durationMs === undefined ? {} : { durationMs }),
    ...(untimed === undefined ? {} : { untimed }),
    parts,
    ...evidenceBreakdown(kept),
  };
}

// Each expected entry and unit pair supplies one part count.
function declarationsOf(expect) {
  const seen = new Set();
  return expect.map(({ entry, parts, unit }) => {
    if (!DECLARED_UNITS.includes(unit)) {
      throw new Error(
        `entry ${entry} requires a unit of ${DECLARED_UNITS.join(" or ")}`,
      );
    }
    const key = JSON.stringify([entry, unit]);
    if (seen.has(key)) {
      throw new Error(
        `entry ${entry} is declared twice for ${unitNoun(unit, 2)}`,
      );
    }
    seen.add(key);
    return { entry, parts, unit, found: new Map() };
  });
}

// Builds the summary of RUN, the attempt collected, from the fragments read
// and EXPECT, the declared entries, their parts and units. Returns the
// summary and a note for every fragment excluded from it or not counted.
export function summarize({ fragments, expect, run }) {
  const notes = [];
  const declarations = declarationsOf(expect);
  const declarationOf = (value) =>
    declarations.find(
      (declaration) =>
        declaration.entry === value.entry && declaration.unit === value.unit,
    );
  for (const { source, value } of fragments) {
    const problems = fragmentProblems(value);
    const exclude = (reason) => notes.push(`excluded ${source}: ${reason}`);
    if (problems.length > 0) {
      exclude(problems.join("; "));
      continue;
    }
    const declaration = declarationOf(value);
    if (attemptOf(value) !== attemptOf(run)) {
      exclude(`${attemptOf(value)}, not ${attemptOf(run)}`);
    } else if (value.revision !== run.revision) {
      exclude(`revision ${value.revision}, not ${run.revision}`);
    } else if (declaration === undefined) {
      exclude(
        declarations.some(({ entry }) => entry === value.entry)
          ? `entry ${value.entry} declares no parts for ${unitNoun(value.unit, 2)}`
          : `entry ${value.entry} is not declared`,
      );
    } else if (value.of !== declaration.parts) {
      exclude(
        `of ${value.of}, but ${value.entry} declares ${declaration.parts} parts for ${unitNoun(declaration.unit, 2)}`,
      );
    } else {
      const { found } = declaration;
      found.set(value.part, [...(found.get(value.part) ?? []), value]);
    }
  }
  const entries = [];
  for (const declaration of declarations) {
    const evidence = evidenceOf(declaration);
    const entry = entries.find(({ path }) => path === declaration.entry);
    if (entry === undefined) {
      entries.push({ path: declaration.entry, evidence: [evidence] });
    } else {
      entry.evidence.push(evidence);
    }
  }
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

// One line per entry's evidence for the CI log, naming its unit and every
// gap.
export function describeSummary(summary) {
  return summary.entries.flatMap((entry) =>
    entry.evidence.map((evidence) => {
      const of = evidence.parts.length;
      const gaps = evidence.parts
        .filter((part) => part.status !== "present")
        .map((part) => `part ${part.part} of ${of} ${part.status}`);
      const counts = `${evidence.passed} passed, ${evidence.failed} failed, ${evidence.other} other ${unitNoun(evidence.unit, 2)}`;
      return [`${entry.path}: ${counts}`, ...gaps].join("; ");
    }),
  );
}
