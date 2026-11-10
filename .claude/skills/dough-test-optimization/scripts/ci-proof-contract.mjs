// The CI proof contract: the versioned fragment and summary schemas, their
// attempt identity, their artifact names, and their validation. Readers of a
// fragment or summary validate it with fragmentProblems or summaryProblems;
// a summary reader then reads a valid summary of either readable version in
// the current one with currentSummaryOf. The units results are counted in
// and one unit's results are ci-proof-evidence.mjs's ("Units"); results'
// figures and their file breakdown are ci-proof-figures.mjs's. Node
// built-ins only, like the rest of the collector (ci-proof-summary.mjs).
//
// A summary entry, one declared Intent location, keeps its results
// partitioned by unit as `evidence`, at most one per unit, each with its own
// counts, timing, parts and file breakdown: no figure is added across units,
// and how many paths a breakdown names never counts as test files. A version
// 1 summary, from before units, is read with each entry's results as its one
// legacy evidence (currentSummaryOf).
import {
  LEGACY_UNIT,
  resultsProblems,
  unitProblems,
} from "./ci-proof-evidence.mjs";
import {
  breakdownProblems,
  figuresProblems,
  isObject,
  isText,
} from "./ci-proof-figures.mjs";

export { COUNT_FIELDS, zeroCounts } from "./ci-proof-figures.mjs";
export {
  DECLARED_UNITS,
  LEGACY_UNIT,
  PART_STATUSES,
  UNITS,
  unitNoun,
  unitOf,
} from "./ci-proof-evidence.mjs";

export const FRAGMENT_SCHEMA_VERSION = 2;
export const SUMMARY_SCHEMA_VERSION = 2;
// The summary versions a reader reads: version 1, from before units, and the
// current one.
export const READABLE_SUMMARY_VERSIONS = [1, SUMMARY_SCHEMA_VERSION];

// The most bytes of a CI proof summary Intent reads, from either proof
// source; a larger one is written whole and named, never truncated, and
// not shown.
export const SUMMARY_READ_LIMIT_BYTES = 1024 * 1024;

// `dashboard/tests` -> `dashboard-tests`, for fragment file names.
export function entrySlug(entry) {
  return entry
    .replace(/[^A-Za-z0-9]+/g, "-")
    .replace(/^-|-$/g, "")
    .toLowerCase();
}

// The artifact name of a GitHub Actions run attempt's summary.
export function summaryArtifactName(attempt) {
  return `ci-proof-summary-attempt-${attempt}`;
}

// An attempt is named by exactly one of two identities: a GitHub Actions run
// attempt, by the positive integer `attempt` (with `runId` in a run or
// summary), or a CI adapter's attempt, by its text `attemptId`. A fragment
// names the attempt's `attempt` or `attemptId`; a summary its `runId` and
// `runAttempt` or its `attemptId`.
const isAdapterAttempt = (value) => value.attemptId !== undefined;

// The attempt that VALUE, a fragment or run, names, as text.
export const attemptOf = (value) =>
  isAdapterAttempt(value)
    ? `attemptId ${value.attemptId}`
    : `attempt ${value.attempt}`;

// A fragment's attempt fields for IDENTITY, a run or fragment identity.
const fragmentAttempt = (identity) =>
  isAdapterAttempt(identity)
    ? { attemptId: identity.attemptId }
    : { attempt: identity.attempt };

// A summary's attempt fields for RUN.
export const summaryAttempt = (run) =>
  isAdapterAttempt(run)
    ? { attemptId: run.attemptId }
    : { runId: run.runId, runAttempt: run.attempt };

// One entry's fragment for one part of one attempt's revision, its results
// counted in UNIT: `{unit: "case"}`, `{unit: "file"}`, or legacy results
// `{unit: "legacy", label}` with the label their unit was spelled with.
// EXTRAS may carry optional `durationMs` (summed test execution time for
// this part) and, for a fragment broken down by file, `untimed`, `files` and
// `unassigned` (ci-proof-figures.mjs, "File breakdown").
export function fragment(identity, entry, unit, counts, extras = {}) {
  return {
    schemaVersion: FRAGMENT_SCHEMA_VERSION,
    entry,
    part: identity.part,
    of: identity.of,
    ...fragmentAttempt(identity),
    revision: identity.revision,
    ...unit,
    ...counts,
    ...extras,
  };
}

export const isPositive = (value) => Number.isSafeInteger(value) && value >= 1;

// Why VALUE does not name exactly one attempt identity, where GITHUB are the
// fields of its GitHub Actions identity; empty when it names one.
export function attemptProblems(value, github) {
  const named = github.filter((field) => value[field] !== undefined);
  if (isAdapterAttempt(value)) {
    if (named.length > 0) {
      return [`names both ${named.join(" and ")} and attemptId`];
    }
    return isText(value.attemptId) ? [] : ["attemptId is not a string"];
  }
  if (named.length === 0) {
    return [`names neither ${github.join(" and ")} nor attemptId`];
  }
  return github
    .filter((field) => !isPositive(value[field]))
    .map((field) => `${field} is not a positive integer`);
}

// Returns why VALUE is not a valid fragment; empty when it is one.
export function fragmentProblems(value) {
  if (!isObject(value)) return ["fragment is not a JSON object"];
  const problems = [];
  if (value.schemaVersion !== FRAGMENT_SCHEMA_VERSION) {
    problems.push(`schemaVersion is not ${FRAGMENT_SCHEMA_VERSION}`);
  }
  for (const field of ["entry", "revision"]) {
    if (!isText(value[field])) problems.push(`${field} is not a string`);
  }
  problems.push(...unitProblems(value, "", false));
  for (const field of ["part", "of"]) {
    if (!isPositive(value[field])) {
      problems.push(`${field} is not a positive integer`);
    }
  }
  problems.push(...attemptProblems(value, ["attempt"]));
  if (isPositive(value.part) && isPositive(value.of) && value.part > value.of) {
    problems.push("part is greater than of");
  }
  const figures = figuresProblems(value, "");
  return [
    ...problems,
    ...figures,
    ...(figures.length > 0 || !isText(value.entry)
      ? []
      : breakdownProblems(value, value.entry, "", value)),
  ];
}

// A version 1 entry: its path, its unit as its runner spelled it (null when
// no part was present to say), and its results.
function legacyEntryProblems(entry, index) {
  const where = `entries[${index}].`;
  if (!isObject(entry)) return [`entries[${index}] is not an object`];
  const problems = [];
  if (!isText(entry.path)) problems.push(`${where}path is not a string`);
  if (entry.unit !== null && !isText(entry.unit)) {
    problems.push(`${where}unit is neither a string nor null`);
  }
  return [
    ...problems,
    ...resultsProblems(
      entry,
      where,
      problems.length > 0 ? undefined : entry.path,
    ),
  ];
}

// A current entry: its path and its evidence, one unit's results each.
function entryProblems(entry, index) {
  const where = `entries[${index}].`;
  if (!isObject(entry)) return [`entries[${index}] is not an object`];
  const problems = isText(entry.path) ? [] : [`${where}path is not a string`];
  if (!Array.isArray(entry.evidence) || entry.evidence.length === 0) {
    return [...problems, `${where}evidence is not a non-empty array`];
  }
  const units = new Set();
  entry.evidence.forEach((evidence, at) => {
    const here = `${where}evidence[${at}].`;
    if (!isObject(evidence)) {
      problems.push(`${where}evidence[${at}] is not an object`);
      return;
    }
    const unit = unitProblems(evidence, here, true);
    if (unit.length === 0 && units.has(evidence.unit)) {
      unit.push(`${here}unit ${evidence.unit} is repeated`);
    }
    units.add(evidence.unit);
    problems.push(
      ...unit,
      ...resultsProblems(
        evidence,
        here,
        unit.length > 0 ? undefined : entry.path,
      ),
    );
  });
  return problems;
}

// Returns why VALUE is not a valid summary of a readable version; empty when
// it is one.
export function summaryProblems(value) {
  if (!isObject(value)) return ["summary is not a JSON object"];
  const problems = [];
  if (!READABLE_SUMMARY_VERSIONS.includes(value.schemaVersion)) {
    problems.push(
      `schemaVersion is not ${READABLE_SUMMARY_VERSIONS.join(" or ")}`,
    );
  }
  for (const field of ["repository", "revision"]) {
    if (!isText(value[field])) problems.push(`${field} is not a string`);
  }
  problems.push(...attemptProblems(value, ["runId", "runAttempt"]));
  if (!Array.isArray(value.entries)) {
    return [...problems, "entries is not an array"];
  }
  const problemsOf =
    value.schemaVersion === 1 ? legacyEntryProblems : entryProblems;
  value.entries.forEach((entry, index) =>
    problems.push(...problemsOf(entry, index)),
  );
  return problems;
}

// SUMMARY, valid in a readable version, in the current version: a version 1
// entry's results become its one legacy evidence, labelled with the unit the
// entry spelled, never read as test cases or test files.
export function currentSummaryOf(summary) {
  if (summary.schemaVersion === SUMMARY_SCHEMA_VERSION) return summary;
  return {
    ...summary,
    schemaVersion: SUMMARY_SCHEMA_VERSION,
    entries: summary.entries.map(({ path, unit, ...results }) => ({
      path,
      evidence: [{ unit: LEGACY_UNIT, label: unit, ...results }],
    })),
  };
}
