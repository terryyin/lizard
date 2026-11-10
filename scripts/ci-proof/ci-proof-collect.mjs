// Collects one attempt's CI proof fragments into its summary: a GitHub
// Actions run attempt's or a CI adapter's attempt's (ci-proof-contract.mjs).
// The declaration is the expectation: fragments present never add a part. A
// declaration names an entry, its part count and, optionally, the unit its
// parts are counted in (`case` or `file`): only that unit's fragments fill
// it, and its evidence keeps that unit even when no fragment arrived. A
// declaration without a unit takes the entry's fragments of any unit no
// declaration of the entry names, as before units were declared: its
// evidence is counted in the one unit they report (legacy, labelled null,
// when none was present). When they report more than one unit, it cannot
// tell which unit its parts are counted in: every present part is
// ambiguous, and a note says why. A fragment from another attempt, of either
// identity, or another revision, or for an undeclared entry, unit or part
// count, is excluded with a note; a declared part with no fragment is
// missing, and one with more than one is ambiguous. Neither is counted, and
// nothing is added across units. A present part's file breakdown, when its
// fragment has one, adds to its evidence's per-path figures; a part without
// one leaves them partial. Node built-ins only, like the rest of the
// collector (ci-proof-summary.mjs).
import {
  COUNT_FIELDS,
  LEGACY_UNIT,
  SUMMARY_SCHEMA_VERSION,
  attemptOf,
  fragmentProblems,
  summaryAttempt,
  unitNoun,
  unitOf,
  zeroCounts,
} from "./ci-proof-contract.mjs";
import {
  addedFigures,
  breakdownOf,
  emptyFigures,
} from "./ci-proof-figures.mjs";

// What a unit's results are called, as Intent calls them; a legacy unit
// also names the label it was spelled with.
const unitWords = ({ unit, label }) =>
  unit !== LEGACY_UNIT || label === null || label === undefined
    ? unitNoun(unit, 2)
    : `${unitNoun(unit, 2)} (${label})`;

// " for test files" after a declaration of a unit; nothing without one.
const forUnit = ({ unit }) =>
  unit === undefined ? "" : ` for ${unitWords({ unit })}`;

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

// The units the PRESENT fragments report, each once.
function unitsReported(present) {
  const units = new Map();
  for (const value of present) {
    const unit = unitOf(value);
    units.set(JSON.stringify(unit), unit);
  }
  return [...units.values()];
}

// The evidence of one DECLARATION from the fragments FOUND for its parts,
// noting in NOTES why a declaration without a unit counts none.
function evidenceOf({ entry, parts: declared, unit, found }, notes) {
  const candidatesOf = (part) => found.get(part) ?? [];
  const present = [...found.values()]
    .filter((candidates) => candidates.length === 1)
    .map(([only]) => only);
  const reported = unit === undefined ? unitsReported(present) : [];
  const conflicting = reported.length > 1;
  if (conflicting) {
    notes.push(
      `entry ${entry} declares its ${declared} parts without a unit, but its fragments report ${reported.map(unitWords).join(" and ")}, so none is counted`,
    );
  }
  const counted =
    unit !== undefined
      ? { unit }
      : reported.length === 1
        ? reported[0]
        : { unit: LEGACY_UNIT, label: null };
  const totals = zeroCounts();
  let durationMs;
  let untimed;
  const kept = [];
  const parts = [...Array(declared).keys()].map((index) => {
    const part = index + 1;
    const candidates = candidatesOf(part);
    if (candidates.length === 0) return { part, status: "missing" };
    if (candidates.length > 1 || conflicting) {
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
    ...counted,
    ...totals,
    ...(durationMs === undefined ? {} : { durationMs }),
    ...(untimed === undefined ? {} : { untimed }),
    parts,
    ...evidenceBreakdown(kept),
  };
}

// EXPECT's declarations, each once: `{entry, parts, unit?}`.
function declarationsOf(expect) {
  const seen = new Set();
  return expect.map(({ entry, parts, unit }) => {
    const key = JSON.stringify([entry, unit ?? null]);
    if (seen.has(key)) {
      throw new Error(`entry ${entry} is declared twice${forUnit({ unit })}`);
    }
    seen.add(key);
    return { entry, parts, unit, found: new Map() };
  });
}

// Builds the summary of RUN, the attempt collected, from the fragments read
// and EXPECT, the declared entries, their parts and any unit. Returns the
// summary and a note for every fragment excluded from it or not counted.
export function summarize({ fragments, expect, run }) {
  const notes = [];
  const declarations = declarationsOf(expect);
  const declarationOf = (value) =>
    declarations.find(
      (declaration) =>
        declaration.entry === value.entry && declaration.unit === value.unit,
    ) ??
    declarations.find(
      (declaration) =>
        declaration.entry === value.entry && declaration.unit === undefined,
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
          ? `entry ${value.entry} declares no parts for ${unitWords(value)}`
          : `entry ${value.entry} is not declared`,
      );
    } else if (value.of !== declaration.parts) {
      exclude(
        `of ${value.of}, but ${value.entry} declares ${declaration.parts} parts${forUnit(declaration)}`,
      );
    } else {
      const { found } = declaration;
      found.set(value.part, [...(found.get(value.part) ?? []), value]);
    }
  }
  const entries = [];
  for (const declaration of declarations) {
    const evidence = evidenceOf(declaration, notes);
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
      const counts = `${evidence.passed} passed, ${evidence.failed} failed, ${evidence.other} other ${unitWords(evidence)}`;
      return [`${entry.path}: ${counts}`, ...gaps].join("; ");
    }),
  );
}
