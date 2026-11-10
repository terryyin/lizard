// How a lead reading's table (./ci-proof-leads-table.mjs) writes its text:
// figures in fixed formats, results named in their unit, shortened paths and
// reasons, aligned columns, capped lists, and the whole within
// LEADS_TABLE_LIMIT_BYTES.
import { unitNoun } from "./ci-proof-contract.mjs";

const LEADS_TABLE_LIMIT_BYTES = 16 * 1024;

const PATH_CHARACTERS = 96;
const UNIT_CHARACTERS = 24;

const counts = new Intl.NumberFormat("en-US");
const tenths = new Intl.NumberFormat("en-US", {
  minimumFractionDigits: 1,
  maximumFractionDigits: 1,
});
const hundredths = new Intl.NumberFormat("en-US", {
  minimumFractionDigits: 2,
  maximumFractionDigits: 2,
});

/** @param {number} value */
export const count = (value) => counts.format(value);

/** @param {number} ms */
export const seconds = (ms) => tenths.format(ms / 1000);

/**
 * Seconds per case of MS over CASES timed cases; "-" with none.
 * @param {number} ms
 * @param {number} cases
 */
export const meanSeconds = (ms, cases) =>
  cases > 0 ? hundredths.format(ms / 1000 / cases) : "-";

/**
 * MS as a percentage of TOTAL.
 * @param {number} ms
 * @param {number} total
 */
export const share = (ms, total) =>
  total > 0 ? `${tenths.format((100 * ms) / total)}%` : "-";

/**
 * TEXT cut to at most MAX characters, at its end.
 * @param {string} text
 * @param {number} max
 */
export const clip = (text, max) =>
  text.length <= max ? text : `${text.slice(0, max - 1)}…`;

/**
 * One unit's results as a column or heading names them: "test cases",
 * "test files", or legacy test results with the label their unit was
 * spelled with, as provenance: "legacy (checks)".
 * @param {import("./ci-proof-scope.mjs").ScopeUnit} unit
 */
export const unitText = ({ unit, label }) =>
  clip(
    unit === "legacy" ? `legacy (${label ?? "no label"})` : unitNoun(unit, 2),
    UNIT_CHARACTERS,
  );

/**
 * PATH cut to at most MAX characters in its middle, keeping both ends.
 * @param {string} path
 * @param {number} [max]
 */
export const elide = (path, max = PATH_CHARACTERS) =>
  path.length <= max
    ? path
    : `${path.slice(0, 32)}…${path.slice(path.length - (max - 33))}`;

/**
 * ROWS under HEADER as aligned columns; NUMERIC names the columns to align
 * right. A last column of text is left as it is.
 * @param {readonly string[]} header
 * @param {readonly (readonly string[])[]} rows
 * @param {readonly number[]} numeric
 */
export function columns(header, rows, numeric) {
  const widths = header.map((title, index) =>
    Math.max(title.length, ...rows.map((row) => row[index].length)),
  );
  const cellOf = (cell, index) => {
    if (numeric.includes(index)) return cell.padStart(widths[index]);
    return index === header.length - 1 ? cell : cell.padEnd(widths[index]);
  };
  const line = (row) => `  ${row.map(cellOf).join("  ")}`.trimEnd();
  return [line(header), ...rows.map(line)];
}

/**
 * The first LIMIT of LINES, then how many MORE there are.
 * @param {readonly string[]} lines
 * @param {number} limit
 * @param {string} more
 */
export const capped = (lines, limit, more) =>
  lines.length <= limit
    ? lines
    : [
        ...lines.slice(0, limit),
        `  … and ${count(lines.length - limit)} more ${more}`,
      ];

/**
 * A section line: its NAME, then its TEXT.
 * @param {string} name
 * @param {string} text
 */
export const label = (name, text) => `${name.padEnd(14)} ${text}`;

/**
 * LINES as text within LEADS_TABLE_LIMIT_BYTES, cut at a line, and saying
 * so, when they would not fit.
 * @param {readonly string[]} lines
 */
export function bounded(lines) {
  const kept = [];
  const note = "… output cut at its byte limit";
  const reserved = Buffer.byteLength(note) + 1;
  let bytes = 0;
  for (const line of lines) {
    const size = Buffer.byteLength(line) + 1;
    if (bytes + size + reserved > LEADS_TABLE_LIMIT_BYTES) {
      kept.push(note);
      break;
    }
    kept.push(line);
    bytes += size;
  }
  return `${kept.join("\n")}\n`;
}
