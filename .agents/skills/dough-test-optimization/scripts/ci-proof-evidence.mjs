// One unit's results in CI proof (ci-proof-contract.mjs): the unit a
// fragment's results or a summary entry's `evidence` are counted in, and why
// one unit's results do not account for their figures, parts and file
// breakdown. Results' figures and their file breakdown are
// ci-proof-figures.mjs's. Node built-ins only, like the rest of the
// collector (ci-proof-summary.mjs).
//
// Units. Every result is counted in one unit: `case`, one test case outcome
// a runner reported; `file`, one aggregate outcome for running one file,
// however many cases that file holds.
import {
  addedFigures,
  breakdownProblems,
  emptyFigures,
  figuresProblems,
  isObject,
  isText,
} from "./ci-proof-figures.mjs";

export const PART_STATUSES = ["present", "missing", "ambiguous"];

export const DECLARED_UNITS = ["case", "file"];

// What results are called in each unit, as Intent names them.
const UNIT_NOUNS = {
  case: ["test case", "test cases"],
  file: ["test file", "test files"],
};

// What COUNT results in UNIT are called: "1 test case", "3 test files".
export const unitNoun = (unit, count) => UNIT_NOUNS[unit][count === 1 ? 0 : 1];

// Why VALUE does not use an allowed unit, or its label is invalid.
export function unitProblems(value, where) {
  if (!DECLARED_UNITS.includes(value.unit)) {
    return [`${where}unit is not ${DECLARED_UNITS.join(", ")}`];
  }
  return value.label === undefined
    ? []
    : [`${where}label is given for ${value.unit} results`];
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

// The counts and untimed of RESULTS' parts that were broken down by file.
function brokenDownFigures(results) {
  const parts = results.parts.filter(
    (part) => part.status === "present" && part.breakdown === true,
  );
  const total = parts.reduce(addedFigures, emptyFigures());
  return parts.every((part) => part.untimed !== undefined)
    ? total
    : { ...total, untimed: undefined };
}

// Why RESULTS, one unit's evidence at entry PATH, do not account for their
// figures, parts and file breakdown. The breakdown is checked once PATH is
// known and
// nothing else is wrong.
export function resultsProblems(results, where, path) {
  const problems = figuresProblems(results, where);
  if (!Array.isArray(results.parts) || results.parts.length === 0) {
    return [...problems, `${where}parts is not a non-empty array`];
  }
  results.parts.forEach((part, partIndex) =>
    problems.push(...partProblems(part, partIndex, where)),
  );
  if (problems.length > 0 || !isText(path)) return problems;
  const brokenDown = results.parts.some((part) => part.breakdown === true);
  if (brokenDown !== (results.files !== undefined)) {
    return [
      brokenDown
        ? `${where}files is absent though a part was broken down by file`
        : `${where}files is given though no part was broken down by file`,
    ];
  }
  return breakdownProblems(results, path, where, brokenDownFigures(results));
}
