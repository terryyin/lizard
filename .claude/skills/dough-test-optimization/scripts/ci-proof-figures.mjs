// The figures of CI proof results (ci-proof-contract.mjs): counts, optional
// summed test execution time `durationMs` and `untimed`, how they add up,
// and their file breakdown, all within one unit's results. Node built-ins
// only, like the rest of the collector (ci-proof-summary.mjs).
//
// File breakdown (optional; additive since schema version 1). A fragment, a
// present summary part and a summary entry's evidence (a version 1 entry)
// may carry `untimed`: how many of their counted results recorded no
// duration, so a sum over the rest is known to be partial; without it, only
// `durationMs` presence says whether time was recorded. A fragment broken
// down by file also carries `files`, one aggregate per repository path
// inside its entry (`{path, passed, failed, other, durationMs?, untimed?}`,
// `untimed` absent when 0), and `unassigned`, the same figures without a
// path, for counted results its report could not place in a file of the
// entry. Together they account for every count of the fragment. A file
// breakdown groups results by path; it never changes their unit, and how
// many paths it names is no count of test files. A summary part whose
// fragment was broken down says `breakdown: true`; its evidence carries the
// files and unassigned figures summed over those parts, which together
// account for exactly those parts' counts. A path is relative,
// `/`-separated, and never names `.` or `..`; machine-absolute paths are
// never published.

export const COUNT_FIELDS = ["passed", "failed", "other"];

export const zeroCounts = () => ({ passed: 0, failed: 0, other: 0 });

// Figures as they accumulate: counts and untimed, durationMs once any is.
export const emptyFigures = () => ({ ...zeroCounts(), untimed: 0 });

// TOTAL with VALUE's counts, optional durationMs and untimed added.
export function addedFigures(total, value) {
  const sum = Object.fromEntries(
    COUNT_FIELDS.map((field) => [field, total[field] + value[field]]),
  );
  const durationMs =
    value.durationMs === undefined
      ? total.durationMs
      : (total.durationMs ?? 0) + value.durationMs;
  return {
    ...sum,
    ...(durationMs === undefined ? {} : { durationMs }),
    untimed: total.untimed + (value.untimed ?? 0),
  };
}

// FIGURES as published: untimed only when nonzero.
const publishedFigures = ({ untimed, ...figures }) =>
  untimed > 0 ? { ...figures, untimed } : figures;

// BY_PATH, a map from each repository path, or undefined for the
// unassigned, to its figures, as a breakdown: `files` in path order, and
// `unassigned` when anything was unassigned.
export function breakdownOf(byPath) {
  const unassigned = byPath.get(undefined);
  return {
    files: [...byPath.keys()]
      .filter((path) => path !== undefined)
      .sort()
      .map((path) => ({ path, ...publishedFigures(byPath.get(path)) })),
    ...(unassigned === undefined
      ? {}
      : { unassigned: publishedFigures(unassigned) }),
  };
}

// Records one counted result, FIELD, with its optional durationMs
// (undefined when it recorded none), under PATH in BREAKDOWN, a map from
// each repository path, or undefined for the unassigned, to its figures.
export function recordResult(breakdown, path, field, durationMs) {
  breakdown.set(
    path,
    addedFigures(breakdown.get(path) ?? emptyFigures(), {
      ...zeroCounts(),
      [field]: 1,
      ...(durationMs === undefined ? { untimed: 1 } : { durationMs }),
    }),
  );
}

// A fragment's breakdown extras from BREAKDOWN (`recordResult`): `untimed`
// over all of it, then its files and unassigned (`breakdownOf`).
export function breakdownExtras(breakdown) {
  const untimed = [...breakdown.values()].reduce(
    (sum, figures) => sum + figures.untimed,
    0,
  );
  return { untimed, ...breakdownOf(breakdown) };
}

// BREAKDOWN with every label that is not a relative path inside ENTRY folded
// into the unassigned figures.
export function unassigningUnusable(breakdown, entry) {
  const kept = new Map();
  for (const [label, figures] of breakdown) {
    const path = isPathIn(label, entry) ? label : undefined;
    kept.set(path, addedFigures(kept.get(path) ?? emptyFigures(), figures));
  }
  return kept;
}

export const isObject = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);
export const isText = (value) => typeof value === "string" && value !== "";
const isCount = (value) => Number.isSafeInteger(value) && value >= 0;
// Optional durationMs: a non-negative finite number when present.
const isDurationMs = (value) =>
  typeof value === "number" && Number.isFinite(value) && value >= 0;

// Whether TEXT is a relative repository path inside ENTRY.
export const isPathIn = (text, entry) =>
  isText(text) &&
  text.startsWith(`${entry}/`) &&
  !text.includes("\\") &&
  text.split("/").every((part) => part !== "" && part !== "." && part !== "..");

// Problems of a figures object (counts, optional durationMs and untimed).
export function figuresProblems(value, where) {
  const problems = COUNT_FIELDS.filter((field) => !isCount(value[field])).map(
    (field) => `${where}${field} is not a count`,
  );
  if (value.durationMs !== undefined && !isDurationMs(value.durationMs)) {
    problems.push(`${where}durationMs is not a non-negative number`);
  }
  if (value.untimed !== undefined && !isCount(value.untimed)) {
    problems.push(`${where}untimed is not a count`);
  }
  return problems;
}

// Why VALUE's `files` and `unassigned` are not a breakdown of ENTRY's
// EXPECTED counts and untimed (untimed compared only when EXPECTED carries
// it); empty when they are, or when VALUE carries no breakdown.
export function breakdownProblems(value, entry, where, expected) {
  if (value.files === undefined) {
    return value.unassigned === undefined
      ? []
      : [`${where}unassigned is given without files`];
  }
  if (!Array.isArray(value.files)) return [`${where}files is not an array`];
  const problems = [];
  const seen = new Set();
  value.files.forEach((file, index) => {
    const at = `${where}files[${index}].`;
    if (!isObject(file)) {
      problems.push(`${where}files[${index}] is not an object`);
      return;
    }
    if (!isPathIn(file.path, entry)) {
      problems.push(`${at}path is not a relative path inside ${entry}`);
    } else if (seen.has(file.path)) {
      problems.push(`${at}path ${file.path} is repeated`);
    }
    seen.add(file.path);
    problems.push(...figuresProblems(file, at));
  });
  if (value.unassigned !== undefined) {
    if (!isObject(value.unassigned)) {
      problems.push(`${where}unassigned is not an object`);
    } else {
      problems.push(
        ...figuresProblems(value.unassigned, `${where}unassigned.`),
      );
    }
  }
  if (problems.length > 0) return problems;
  const total = [
    ...value.files,
    ...(value.unassigned === undefined ? [] : [value.unassigned]),
  ].reduce(addedFigures, emptyFigures());
  for (const field of COUNT_FIELDS) {
    if (total[field] !== expected[field]) {
      problems.push(
        `${where}files and unassigned count ${total[field]} ${field}, not ${expected[field]}`,
      );
    }
  }
  if (expected.untimed !== undefined && total.untimed !== expected.untimed) {
    problems.push(
      `${where}files and unassigned count ${total.untimed} untimed, not ${expected.untimed}`,
    );
  }
  return problems;
}
