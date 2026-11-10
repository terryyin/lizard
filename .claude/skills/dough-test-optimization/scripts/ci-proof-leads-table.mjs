// The bounded text a lead reading prints (./ci-proof-leads.mjs), never the
// raw summary: the requested revision, the attempt read and how it relates
// to that revision, the recorded selection, gaps, per-unit totals, the
// ranked leads, the folders beyond them and the remaining tail
// (./ci-proof-ranking-table.mjs). Every list is capped and every path or
// reason shortened (./ci-proof-table-text.mjs), so the text stays within its
// byte limit whatever the summary holds.
import { rankingLines } from "./ci-proof-ranking-table.mjs";
import {
  bounded,
  capped,
  clip,
  columns,
  count,
  elide,
  label,
  unitText,
} from "./ci-proof-table-text.mjs";

const REASON_CHARACTERS = 220;
const REMEDY_CHARACTERS = 420;
const GAP_LINES = 12;
const SELECTION_LINES = 10;
const CHANGED_LINES = 10;

/** @typedef {import("./ci-proof-ranking.mjs").Ranking} Ranking */

/**
 * A gap the reading says how to close: its reason and its remedy.
 * @typedef {{ readonly reason: string; readonly remedy: string }} RemediedGap
 */

/**
 * What one lead reading found. `attempt` names the attempt read, its
 * revision and how it ended; `compatibility` relates it to the requested
 * revision; `selection` is what the summary records of the test selection,
 * each entry with each unit it holds;
 * each gap is its reason, with its remedy when the reading names one;
 * `ranking` is absent, with `verdict` saying why, when nothing ranked.
 * @typedef {{
 *   readonly repository: string;
 *   readonly requested: { readonly spec: string; readonly revision: string };
 *   readonly source: string;
 *   readonly attempt?: {
 *     readonly identity: string;
 *     readonly revision: string;
 *     readonly outcome: string;
 *   };
 *   readonly compatibility?: {
 *     readonly text: string;
 *     readonly changed: readonly { status: string; path: string }[];
 *   };
 *   readonly selection?: {
 *     readonly text: string;
 *     readonly entries: readonly {
 *       path: string;
 *       declared: string;
 *       units: readonly (import("./ci-proof-scope.mjs").ScopeUnit & {
 *         parts: string;
 *       })[];
 *     }[];
 *   };
 *   readonly gaps: readonly (string | RemediedGap)[];
 *   readonly ranking?: Ranking;
 *   readonly verdict?: string;
 * }} LeadsReport
 */

// A gap's lines: its reason, then its remedy, when it has one, on a line of
// its own so a long reason never hides it.
function gapLines(gap) {
  if (typeof gap === "string") return [`  - ${clip(gap, REASON_CHARACTERS)}`];
  return [
    `  - ${clip(gap.reason, REASON_CHARACTERS)}`,
    `    remedy: ${clip(gap.remedy, REMEDY_CHARACTERS)}`,
  ];
}

function attemptLines({ attempt, compatibility }) {
  if (!attempt) return [];
  const changed = (compatibility?.changed ?? []).map(
    ({ status, path }) => `  ${status.padEnd(2)} ${elide(path)}`,
  );
  return [
    label(
      "Attempt",
      `${attempt.identity} (${attempt.outcome}) at ${attempt.revision}`,
    ),
    ...(compatibility
      ? [
          label("Compatibility", clip(compatibility.text, REASON_CHARACTERS)),
          ...capped(changed, CHANGED_LINES, "changed paths"),
        ]
      : []),
  ];
}

function selectionLines({ selection }) {
  if (!selection) return [];
  // One row for each unit an entry holds, the entry named on its first.
  const rows = selection.entries.flatMap((entry) =>
    entry.units.map((unit, index) => [
      index === 0 ? elide(entry.path) : "",
      index === 0 ? clip(entry.declared, 40) : "",
      unitText(unit),
      unit.parts,
    ]),
  );
  return [
    label("Selection", clip(selection.text, REASON_CHARACTERS)),
    ...capped(
      columns(["entry", "declared", "unit", "parts"], rows, []),
      SELECTION_LINES + 1,
      "selection rows",
    ),
  ];
}

/**
 * REPORT as the bounded text a lead reading prints.
 * @param {LeadsReport} report
 * @returns {string}
 */
export function leadsTable(report) {
  const { requested, ranking, gaps } = report;
  const reasons = gaps.slice(0, GAP_LINES).flatMap(gapLines);
  return bounded([
    `CI proof leads for ${clip(report.repository, 100)}`,
    label("Requested", `${clip(requested.spec, 60)} = ${requested.revision}`),
    label("Source", clip(report.source, REASON_CHARACTERS)),
    ...attemptLines(report),
    ...selectionLines(report),
    label("Gaps", gaps.length > 0 ? count(gaps.length) : "none"),
    ...reasons,
    ...(gaps.length > GAP_LINES
      ? [`  … and ${count(gaps.length - GAP_LINES)} more gaps`]
      : []),
    ...(ranking
      ? rankingLines(ranking)
      : [
          label(
            "Leads",
            `none: ${clip(report.verdict ?? "nothing ranked", REASON_CHARACTERS)}`,
          ),
        ]),
  ]);
}
