// The part of one attempt's CI proof summary (./ci-proof-contract.mjs) that a
// requested scope holds, at the granularity the summary supports, for a lead
// reading to rank (./ci-proof-ranking.mjs). A summary is read in the current
// version (./ci-proof-check.mjs): each entry holds its results in each unit
// as `evidence` (./ci-proof-evidence.mjs, "Units"), test cases, test files or
// legacy test results, and each unit's results stay that unit's here. Of each
// unit's results the scope holds each in-scope test file of its breakdown
// and, as one block each, results without a breakdown, parts reported
// without one, and results its reports could not place in a file. Its gaps
// keep partial figures visible (./ci-proof-scope-gaps.mjs).
import { unitOf } from "./ci-proof-contract.mjs";
import * as gapsOf from "./ci-proof-scope-gaps.mjs";

/**
 * One unit's results, as a ranking groups them: `case`, `file`, or `legacy`
 * with the label its unit was spelled with, provenance only.
 * @typedef {{
 *   readonly unit: "case" | "file" | "legacy";
 *   readonly label?: string | null;
 * }} ScopeUnit
 */

/**
 * One rankable item of the scope: its path, its entry, what it is, the unit
 * of its results, its folder when it is a file, and its counted results, how
 * many failed, how many recorded no duration, and the summed time of the
 * rest, absent when none recorded one.
 * @typedef {ScopeUnit & {
 *   readonly path: string;
 *   readonly entry: string;
 *   readonly kind: "file" | "entry" | "parts" | "unassigned";
 *   readonly folder?: string;
 *   readonly parts?: readonly number[];
 *   readonly results: number;
 *   readonly failed: number;
 *   readonly untimed: number;
 *   readonly durationMs?: number;
 * }} ScopeItem
 */

/**
 * One in-scope entry as the attempt reported it: its path and, for each unit
 * it holds results in, how many parts that unit declares and the numbers of
 * those present, missing and ambiguous.
 * @typedef {{
 *   readonly path: string;
 *   readonly units: readonly (ScopeUnit & {
 *     readonly of: number;
 *     readonly present: number;
 *     readonly missing: readonly number[];
 *     readonly ambiguous: readonly number[];
 *   })[];
 * }} ScopeEntry
 */

// Whether PATH is SCOPE or inside it.
const isWithin = (path, scope) =>
  path === scope || path.startsWith(`${scope}/`);

// FIGURES' results, failed and untimed, and the time those timed took. A
// figure without a duration recorded none for any of its results.
function measured(figures) {
  const results = figures.passed + figures.failed + figures.other;
  const { failed } = figures;
  if (figures.durationMs === undefined)
    return { results, failed, untimed: results };
  const untimed = Math.min(results, figures.untimed ?? 0);
  return { results, failed, untimed, durationMs: figures.durationMs };
}

// Figures ONE and OTHER of one unit, summed.
function plus(one, other) {
  const durations = [one.durationMs, other.durationMs].filter(
    (value) => value !== undefined,
  );
  return {
    results: one.results + other.results,
    failed: one.failed + other.failed,
    untimed: one.untimed + other.untimed,
    ...(durations.length === 0
      ? {}
      : { durationMs: durations.reduce((sum, value) => sum + value, 0) }),
  };
}

const none = { results: 0, failed: 0, untimed: 0 };
const measuredParts = (parts) => parts.map(measured).reduce(plus, none);

function scopeUnit(evidence) {
  const numbers = (status) =>
    evidence.parts
      .filter((part) => part.status === status)
      .map(({ part }) => part);
  return {
    ...unitOf(evidence),
    of: evidence.parts.length,
    present: numbers("present").length,
    missing: numbers("missing"),
    ambiguous: numbers("ambiguous"),
  };
}

// The items and gaps of EVIDENCE, one unit's results at the entry AT names,
// whole or, when AT names scope paths inside it, only for its files under
// them.
function unitScope(evidence, at) {
  const { inside } = at;
  const item = (path, kind, figures, extra = {}) => ({
    path,
    entry: at.path,
    kind,
    ...unitOf(evidence),
    ...extra,
    ...figures,
  });
  const present = evidence.parts.filter((part) => part.status === "present");
  if (evidence.files === undefined) {
    const total = measuredParts(present);
    const items =
      inside || total.results === 0 ? [] : [item(at.path, "entry", total)];
    return { items, gaps: [gapsOf.noBreakdown(at)] };
  }
  const items = evidence.files
    .filter(
      (file) => !inside || inside.some((scope) => isWithin(file.path, scope)),
    )
    .map((file) =>
      item(file.path, "file", measured(file), {
        folder: file.path.slice(0, file.path.lastIndexOf("/")),
      }),
    );
  const gaps = [];
  const unbroken = present.filter((part) => part.breakdown !== true);
  const unbrokenTotal = measuredParts(unbroken);
  if (unbrokenTotal.results > 0) {
    const parts = unbroken.map(({ part }) => part);
    if (!inside) items.push(item(at.path, "parts", unbrokenTotal, { parts }));
    gaps.push(gapsOf.unbrokenParts(at, parts, evidence.parts.length));
  }
  const unassigned = evidence.unassigned && measured(evidence.unassigned);
  if (unassigned && unassigned.results > 0) {
    if (!inside) items.push(item(at.path, "unassigned", unassigned));
    gaps.push(gapsOf.unassigned(at, unassigned.results));
  }
  return { items, gaps };
}

/**
 * The part of SUMMARY, a valid summary in the current version, that SCOPE
 * holds: repository paths of entries, folders or files, or every entry when
 * SCOPE is undefined. `noBreakdown` names each in-scope entry once that
 * holds results in some unit without a file breakdown.
 * @param {{ readonly entries: readonly any[] }} summary
 * @param {readonly string[] | undefined} scope
 * @returns {{
 *   entries: ScopeEntry[];
 *   items: ScopeItem[];
 *   gaps: string[];
 *   noBreakdown: string[];
 * }}
 */
export function scopeOf(summary, scope) {
  const paths = scope ?? summary.entries.map((entry) => entry.path);
  const entries = [];
  const items = [];
  const gaps = [];
  const noBreakdown = [];
  for (const entry of summary.entries) {
    const whole = paths.some((path) => isWithin(entry.path, path));
    const inside = paths.filter(
      (path) => path !== entry.path && isWithin(path, entry.path),
    );
    if (!whole && inside.length === 0) continue;
    const units = entry.evidence.map(scopeUnit);
    entries.push({ path: entry.path, units });
    if (entry.evidence.some((evidence) => evidence.files === undefined))
      noBreakdown.push(entry.path);
    entry.evidence.forEach((evidence, index) => {
      /** @type {import("./ci-proof-scope-gaps.mjs").GapAt} */
      const at = {
        path: entry.path,
        ...(whole ? {} : { inside }),
        named: entry.evidence.length > 1,
        unit: evidence.unit,
      };
      const held = unitScope(evidence, at);
      items.push(...held.items);
      gaps.push(
        ...gapsOf.reportGaps(at, units[index], held.items.reduce(plus, none)),
        ...held.gaps,
      );
    });
  }
  for (const path of paths) {
    const covering = summary.entries.filter(
      (entry) => isWithin(entry.path, path) || isWithin(path, entry.path),
    );
    if (covering.length === 0)
      gaps.push(`${path}: no entry of this summary covers it`);
    else if (
      covering.every((entry) =>
        entry.evidence.every((evidence) => evidence.files !== undefined),
      ) &&
      !items.some((item) => isWithin(item.path, path))
    )
      gaps.push(`${path}: the summary holds no results under it`);
  }
  return { entries, items, gaps, noBreakdown };
}
