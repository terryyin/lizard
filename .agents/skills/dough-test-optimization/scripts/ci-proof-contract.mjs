// The CI proof contract: the versioned fragment and summary schemas, their
// attempt identity, their artifact names, and their validation. Readers of a
// fragment or summary validate it with fragmentProblems or summaryProblems.
// Results' figures and their file breakdown are ci-proof-figures.mjs's. Node
// built-ins only, like the rest of the collector (ci-proof-summary.mjs).
import {
  addedFigures,
  breakdownProblems,
  emptyFigures,
  figuresProblems,
  isObject,
  isText,
} from "./ci-proof-figures.mjs";

export { COUNT_FIELDS, zeroCounts } from "./ci-proof-figures.mjs";

export const FRAGMENT_SCHEMA_VERSION = 1;
export const SUMMARY_SCHEMA_VERSION = 1;
export const PART_STATUSES = ["present", "missing", "ambiguous"];

// The most bytes of a GitHub Actions run attempt's summary Intent reads; a
// larger one is written whole and named, never truncated, and not shown.
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

// One entry's fragment for one part of one attempt's revision. EXTRAS may
// carry optional `durationMs` (summed test execution time for this part)
// and, for a fragment broken down by file, `untimed`, `files` and
// `unassigned` (ci-proof-figures.mjs, "File breakdown").
export function fragment(identity, entry, unit, counts, extras = {}) {
  return {
    schemaVersion: FRAGMENT_SCHEMA_VERSION,
    entry,
    part: identity.part,
    of: identity.of,
    ...fragmentAttempt(identity),
    revision: identity.revision,
    unit,
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
  for (const field of ["entry", "revision", "unit"]) {
    if (!isText(value[field])) problems.push(`${field} is not a string`);
  }
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

function partProblems(part, index, where) {
  const at = `${where}parts[${index}].`;
  if (!isObject(part)) return [`${where}parts[${index}] is not an object`];
  const problems = [];
  if (part.part !== index + 1) problems.push(`${at}part is not ${index + 1}`);
  if (!PART_STATUSES.includes(part.status)) {
    problems.push(`${at}status is not ${PART_STATUSES.join(", ")}`);
  } else if (part.status === "present") {
    problems.push(...figuresProblems(part, at));
    if (part.breakdown !== undefined && part.breakdown !== true) {
      problems.push(`${at}breakdown is not true`);
    }
  }
  return problems;
}

// The counts and untimed of ENTRY's parts that were broken down by file.
function brokenDownFigures(entry) {
  const parts = entry.parts.filter(
    (part) => part.status === "present" && part.breakdown === true,
  );
  const total = parts.reduce(addedFigures, emptyFigures());
  return parts.every((part) => part.untimed !== undefined)
    ? total
    : { ...total, untimed: undefined };
}

function entryProblems(entry, index) {
  const where = `entries[${index}].`;
  if (!isObject(entry)) return [`entries[${index}] is not an object`];
  const problems = [];
  if (!isText(entry.path)) problems.push(`${where}path is not a string`);
  if (entry.unit !== null && !isText(entry.unit)) {
    problems.push(`${where}unit is neither a string nor null`);
  }
  problems.push(...figuresProblems(entry, where));
  if (!Array.isArray(entry.parts) || entry.parts.length === 0) {
    return [...problems, `${where}parts is not a non-empty array`];
  }
  entry.parts.forEach((part, partIndex) =>
    problems.push(...partProblems(part, partIndex, where)),
  );
  if (problems.length > 0 || !isText(entry.path)) return problems;
  const brokenDown = entry.parts.some((part) => part.breakdown === true);
  if (brokenDown !== (entry.files !== undefined)) {
    return [
      brokenDown
        ? `${where}files is absent though a part was broken down by file`
        : `${where}files is given though no part was broken down by file`,
    ];
  }
  return breakdownProblems(entry, entry.path, where, brokenDownFigures(entry));
}

// Returns why VALUE is not a valid summary; empty when it is one.
export function summaryProblems(value) {
  if (!isObject(value)) return ["summary is not a JSON object"];
  const problems = [];
  if (value.schemaVersion !== SUMMARY_SCHEMA_VERSION) {
    problems.push(`schemaVersion is not ${SUMMARY_SCHEMA_VERSION}`);
  }
  for (const field of ["repository", "revision"]) {
    if (!isText(value[field])) problems.push(`${field} is not a string`);
  }
  problems.push(...attemptProblems(value, ["runId", "runAttempt"]));
  if (!Array.isArray(value.entries)) {
    return [...problems, "entries is not an array"];
  }
  value.entries.forEach((entry, index) =>
    problems.push(...entryProblems(entry, index)),
  );
  return problems;
}
