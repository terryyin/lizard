// The part of a lead reading's table (./ci-proof-leads-table.mjs) that shows
// a ranking (./ci-proof-ranking.mjs): every unit's totals, then for each
// ranked unit its leads, the folders beyond them and its tail. Results,
// time, shares and means never cross units: each ranked unit's section names
// its unit and its results.
import { RANKED_UNITS } from "./ci-proof-ranking.mjs";
import {
  capped,
  columns,
  count,
  elide,
  label,
  meanSeconds,
  seconds,
  share,
  unitText,
} from "./ci-proof-table-text.mjs";

const UNIT_LINES = 6;

// What a ranked unit's results column is headed.
const RESULTS = { case: "cases", file: "files", legacy: "results" };

// An item's path as a lead shows it: a block of an entry says what it is,
// and results without a duration are counted beside it.
function itemPath(item) {
  const untimed = item.untimed > 0 ? ` (+${count(item.untimed)} untimed)` : "";
  const parts = (item.parts ?? []).slice(0, 6).join(", ");
  const kinds = {
    file: "",
    entry: " (entry total, no file breakdown)",
    parts: ` (parts ${parts}${item.parts?.length > 6 ? ", …" : ""} without file breakdown)`,
    unassigned: " (results not placed in a file)",
  };
  return `${elide(item.path)}${kinds[item.kind]}${untimed}`;
}

function totalsLines({ units, rankings }) {
  const rows = units.map((unit) => [
    unitText(unit),
    count(unit.entries),
    count(unit.paths),
    count(unit.results),
    seconds(unit.durationMs),
    count(unit.untimed),
  ]);
  const unranked = units.length - rankings.length;
  return [
    label(
      "Scope totals",
      "per unit; results, time, shares and means never cross units",
    ),
    ...capped(
      columns(
        ["unit", "entries", "paths", "results", "time s", "untimed"],
        rows,
        [1, 2, 3, 4, 5],
      ),
      UNIT_LINES + 1,
      "units",
    ),
    ...(unranked > 0
      ? [
          label(
            "Unranked",
            `${count(unranked)} units without timed items or beyond the first ${RANKED_UNITS}; narrow --scope to rank them`,
          ),
        ]
      : []),
  ];
}

function leadLines({ leads, timedItems, totalMs }, name, results) {
  const rows = leads.map((item, index) => [
    String(index + 1),
    seconds(item.durationMs),
    share(item.durationMs, totalMs),
    count(item.results),
    meanSeconds(item.durationMs, item.results - item.untimed),
    itemPath(item),
  ]);
  const ranked = "ranked by summed CI time";
  return [
    label(
      "Leads",
      `${name}: ${
        leads.length < timedItems
          ? `${count(leads.length)} of ${count(timedItems)} timed items, ${ranked}`
          : `all ${count(leads.length)} timed items, ${ranked}`
      }`,
    ),
    ...columns(
      ["#", "time s", "share", results, "mean s", "path"],
      rows,
      [0, 1, 2, 3, 4],
    ),
  ];
}

function restLines({ folders, folderCount, tail, totalMs }, name, results) {
  const folderRows = folders.map((folder) => [
    seconds(folder.durationMs),
    share(folder.durationMs, totalMs),
    count(folder.paths),
    count(folder.results),
    meanSeconds(folder.durationMs, folder.timedResults),
    elide(folder.folder),
  ]);
  return [
    label(
      "Folders",
      `${name}: ${
        folders.length === 0
          ? "none beyond the leads"
          : `${count(folders.length)} of ${count(folderCount)} folders beyond the leads, ranked by summed CI time`
      }`,
    ),
    ...(folderRows.length === 0
      ? []
      : columns(
          ["time s", "share", "paths", results, "mean s", "folder"],
          folderRows,
          [0, 1, 2, 3, 4],
        )),
    label(
      "Tail",
      `${name}: ${tail ? "the rest beyond the leads and those folders" : "none"}`,
    ),
    ...(tail
      ? columns(
          ["items", results, "time s", "share", "mean s"],
          [
            [
              count(tail.items),
              count(tail.results),
              seconds(tail.durationMs),
              share(tail.durationMs, totalMs),
              meanSeconds(tail.durationMs, tail.timedResults),
            ],
          ],
          [0, 1, 2, 3, 4],
        )
      : []),
  ];
}

// One ranked unit's section: its leads, folders and tail, shares of its own
// summed time.
function unitRankingLines(ranking) {
  const name = unitText(ranking);
  const results = RESULTS[ranking.unit];
  return [
    ...leadLines(ranking, name, results),
    ...restLines(ranking, name, results),
  ];
}

/**
 * RANKING's lines in a lead reading's table.
 * @param {import("./ci-proof-ranking.mjs").Ranking} ranking
 * @returns {string[]}
 */
export const rankingLines = (ranking) => [
  ...totalsLines(ranking),
  ...(ranking.rankings.length === 0
    ? [label("Leads", "none: no in-scope result recorded a duration")]
    : ranking.rankings.flatMap(unitRankingLines)),
];
