// The gaps that keep a scope's figures partial (./ci-proof-scope.mjs), as a
// lead reading words them: an entry's missing and ambiguous parts, failed
// and untimed results, results without a file breakdown, and results its
// reports could not place in a file. Each names its results in their unit,
// test cases, test files or legacy test results (./ci-proof-evidence.mjs),
// and, beside another unit's results at the same entry, which unit's parts
// it means.
import { unitNoun } from "./ci-proof-contract.mjs";

/**
 * Where a gap is: the entry PATH, the scope paths INSIDE it a reading holds
 * (absent for the whole entry), whether its unit is NAMED, as beside another
 * unit's results, and that UNIT.
 * @typedef {{
 *   readonly path: string;
 *   readonly inside?: readonly string[];
 *   readonly named: boolean;
 *   readonly unit: "case" | "file" | "legacy";
 * }} GapAt
 */

const partList = (numbers, of) =>
  `part${numbers.length > 1 ? "s" : ""} ${numbers.slice(0, 8).join(", ")}${numbers.length > 8 ? ", …" : ""} of ${of}`;

// "tests:", or for scope paths inside it "tests/a: inside tests, whose".
const whereOf = ({ path, inside }) =>
  inside ? `${inside.join(", ")}: inside ${path}, whose` : `${path}:`;

// " of its test files" beside another unit's results at the entry.
const ofUnit = ({ named, unit }) =>
  named ? ` of its ${unitNoun(unit, 2)}` : "";

const counted = (count, unit) => `${count} ${unitNoun(unit, count)}`;

/**
 * The gap of AT's results without a file breakdown.
 * @param {GapAt} at
 */
export function noBreakdown(at) {
  if (at.inside)
    return `${whereOf(at)} ${at.named ? unitNoun(at.unit, 2) : "results"} have no file breakdown, so they cannot be measured`;
  return at.named
    ? `${at.path}: no file breakdown${ofUnit(at)}; they rank as one entry total`
    : `${at.path}: no file breakdown; its results rank as one entry total`;
}

/**
 * The gap of AT's PARTS, of OF, reported without a file breakdown.
 * @param {GapAt} at
 * @param {readonly number[]} parts
 * @param {number} of
 */
export const unbrokenParts = (at, parts, of) =>
  `${whereOf(at)} ${partList(parts, of)}${ofUnit(at)} reported no file breakdown; ${at.inside ? "some of its results may be there" : "they rank as one block"}`;

/**
 * The gap of AT's COUNT results its reports could not place in a file.
 * @param {GapAt} at
 * @param {number} count
 */
export const unassigned = (at, count) =>
  `${whereOf(at)} ${counted(count, at.unit)} ${count === 1 ? "was" : "were"} not placed in a file; ${at.inside ? "some of its results may be among them" : "they rank as one block"}`;

/**
 * The gaps of one unit's own reports at an entry, as its scope UNIT states
 * them, and of the in-scope results' summed FIGURES.
 * @param {GapAt} at
 * @param {{ of: number; missing: readonly number[]; ambiguous: readonly number[] }} unit
 * @param {{ results: number; failed: number; untimed: number }} figures
 */
export function reportGaps(at, { of, missing, ambiguous }, figures) {
  const { path } = at;
  const gaps = [];
  for (const [status, numbers] of [
    ["missing", missing],
    ["ambiguous", ambiguous],
  ])
    if (numbers.length > 0)
      gaps.push(
        `${path}: ${partList(numbers, of)}${ofUnit(at)} ${status}${status === "ambiguous" ? " (reported more than once)" : ""}; their results are not counted`,
      );
  const { results, failed, untimed } = figures;
  if (failed > 0)
    gaps.push(`${path}: ${counted(failed, at.unit)} failed in this attempt`);
  if (untimed > 0)
    gaps.push(
      untimed === results
        ? `${path}: no duration recorded for its ${counted(results, at.unit)}; they cannot rank`
        : `${path}: ${untimed} of ${counted(results, at.unit)} recorded no duration; its time sums are partial`,
    );
  return gaps;
}
