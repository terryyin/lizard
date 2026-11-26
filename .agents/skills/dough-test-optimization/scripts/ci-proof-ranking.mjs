// Leads from the in-scope items of one attempt's CI proof summary
// (./ci-proof-scope.mjs), ranked within each unit by summed CI time, since
// time, counts and shares never cross units. Test cases precede test files. Each
// ranked unit has its leads, then the folders holding the most of its rest,
// then its remaining tail; every timed second of a ranked unit is in exactly
// one of those. The units with timed items share LEAD_LIMIT leads and
// FOLDER_LIMIT folders. Items that recorded no duration count in the totals only.
import { DECLARED_UNITS } from "./ci-proof-contract.mjs";

export const LEAD_LIMIT = 20;
const FOLDER_LIMIT = 10;

/** @typedef {import("./ci-proof-scope.mjs").ScopeItem} ScopeItem */
/** @typedef {import("./ci-proof-scope.mjs").ScopeUnit} ScopeUnit */

/**
 * Summed figures of some items of one unit: how many items, how many of
 * them are test file paths of a breakdown, their results, how many results
 * recorded a duration and the time they took, and how many recorded none.
 * @typedef {{
 *   readonly items: number;
 *   readonly paths: number;
 *   readonly results: number;
 *   readonly timedResults: number;
 *   readonly durationMs: number;
 *   readonly untimed: number;
 * }} Totals
 */

/**
 * One unit's ranking: the summed time of its timed items, its leads, the
 * folders shown beyond them of `folderCount`, and its tail, absent when
 * nothing remains.
 * @typedef {ScopeUnit & {
 *   readonly totalMs: number;
 *   readonly timedItems: number;
 *   readonly leads: readonly ScopeItem[];
 *   readonly folders: readonly (Totals & { folder: string })[];
 *   readonly folderCount: number;
 *   readonly tail?: Totals;
 * }} UnitRanking
 */

/**
 * Every unit's totals, with how many entries hold its results, and the
 * rankings of the units with timed items, in the same order.
 * @typedef {{
 *   readonly units: readonly (ScopeUnit & Totals & { entries: number })[];
 *   readonly rankings: readonly UnitRanking[];
 * }} Ranking
 */

const byPath = (one, other) => (one < other ? -1 : one > other ? 1 : 0);

// Larger summed time first; equal times by name, so a ranking is stable.
const byTime = (name) => (one, other) =>
  other.durationMs - one.durationMs || byPath(name(one), name(other));

/**
 * @param {readonly ScopeItem[]} items
 * @returns {Totals}
 */
function totalsOf(items) {
  let durationMs = 0;
  let timedResults = 0;
  for (const item of items)
    if (item.durationMs !== undefined) {
      durationMs += item.durationMs;
      timedResults += item.results - item.untimed;
    }
  const sum = (field) => items.reduce((total, item) => total + item[field], 0);
  return {
    items: items.length,
    paths: items.filter((item) => item.kind === "file").length,
    results: sum("results"),
    timedResults,
    durationMs,
    untimed: sum("untimed"),
  };
}

// ITEMS grouped by KEY, in first-seen order.
function grouped(items, key) {
  const groups = new Map();
  for (const item of items) {
    const name = key(item);
    groups.set(name, [...(groups.get(name) ?? []), item]);
  }
  return [...groups.values()];
}

/**
 * One unit's ITEMS ranked, with up to LEADLIMIT leads and FOLDERLIMIT
 * folders.
 * @param {readonly ScopeItem[]} items
 * @param {number} leadLimit
 * @param {number} folderLimit
 * @returns {UnitRanking}
 */
function unitRanking(items, leadLimit, folderLimit) {
  const timed = items
    .filter((item) => item.durationMs !== undefined)
    .sort(byTime((item) => item.path));
  const leads = timed.slice(0, leadLimit);
  const rest = timed.slice(leadLimit);
  const folders = grouped(
    rest.filter((item) => item.folder !== undefined),
    (item) => item.folder,
  )
    .map((group) => ({ folder: group[0].folder, ...totalsOf(group) }))
    .sort(byTime((folder) => folder.folder));
  const shown = folders.slice(0, folderLimit);
  const shownFolders = new Set(shown.map((at) => at.folder));
  const tail = rest.filter((item) => !shownFolders.has(item.folder));
  return {
    unit: items[0].unit,
    totalMs: totalsOf(timed).durationMs,
    timedItems: timed.length,
    leads,
    folders: shown,
    folderCount: folders.length,
    ...(tail.length > 0 ? { tail: totalsOf(tail) } : {}),
  };
}

/**
 * The scope's ITEMS ranked within each unit, with every unit's totals.
 * @param {readonly ScopeItem[]} items
 * @returns {Ranking}
 */
export function rankedLeads(items) {
  const groups = DECLARED_UNITS.map((unit) =>
    items.filter((item) => item.unit === unit),
  ).filter((group) => group.length > 0);
  const units = groups.map((group) => ({
    unit: group[0].unit,
    entries: new Set(group.map((item) => item.entry)).size,
    ...totalsOf(group),
  }));
  const ranked = groups.filter((group) =>
    group.some((item) => item.durationMs !== undefined),
  );
  const leadLimit = Math.ceil(LEAD_LIMIT / Math.max(1, ranked.length));
  const folderLimit = Math.ceil(FOLDER_LIMIT / Math.max(1, ranked.length));
  return {
    units,
    rankings: ranked.map((group) => unitRanking(group, leadLimit, folderLimit)),
  };
}
