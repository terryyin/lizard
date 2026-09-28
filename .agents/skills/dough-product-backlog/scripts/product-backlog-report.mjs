// What the tool says about a change it has just made: which work item moved,
// which list holds it now, and what the operation deliberately left alone.
// Every report is written from a change already published, so it describes
// the file as it now stands rather than what the caller asked for. This is
// the counterpart of the usage text, which describes the tool itself.

import { directionHeading } from "./product-backlog-direction.mjs";
import { queueHeading, takenHeading } from "./product-backlog-document.mjs";

export function reportAdd(identity, file) {
  return `Added "${identity}" to "## ${queueHeading}" in ${file}.`;
}

export function reportPlace(outcome, file) {
  const { identity } = outcome.entry;
  const from = outcome.returned
    ? `Returned "${identity}" from "## ${takenHeading}" to`
    : `Placed "${identity}" in`;
  return `${from} "## ${queueHeading}" in ${file}, at the requested position.`;
}

export function reportTake(outcome, file) {
  const { identity } = outcome.entry;
  if (outcome.result === "taken") {
    return `Took "${identity}" into "## ${takenHeading}" in ${file}.`;
  }
  const ending =
    outcome.result === "linked"
      ? "; its plan link was added and its place kept."
      : ", unchanged.";
  return `"${identity}" is already in "## ${takenHeading}" in ${file}${ending}`;
}

export function reportComplete(outcome, file) {
  const { identity, list, href } = outcome.entry;
  const released = outcome.released.map(
    (path) => ` Released agent profile ${path} beside the backlog.`,
  );
  return (
    `Removed "${identity}" from "## ${list}" in ${file}. ` +
    `Its canonical home ${href} was not changed.${released.join("")}`
  );
}

export function reportRefresh(outcome, file) {
  const { identity, list } = outcome.entry;
  if (outcome.changed.length === 0) {
    return `"${identity}" already reads as requested in ${file}, unchanged.`;
  }
  return (
    `Refreshed the ${outcome.changed.join(" and ")} of "${identity}" in ` +
    `"## ${list}" in ${file}; its identity and its place are unchanged.`
  );
}

// What the direction now says is the caller's own text, so the report says
// what became of the section rather than quoting it back at them.
export function reportDirection(outcome, file) {
  const said = {
    set: `Set the "## ${directionHeading}" in ${file}`,
    replaced: `Replaced the "## ${directionHeading}" in ${file}`,
    cleared: `Cleared the "## ${directionHeading}" from ${file}`,
    unchanged: `The "## ${directionHeading}" in ${file} already reads as requested`,
  }[outcome.result];
  return `${said}. Every entry in both lists is unchanged.`;
}

// A merge says what both branches turned out to have changed, including the
// removals of work neither branch was about, because that is what a caller
// checks the published result against before accepting it.
export function reportMerge(outcome, file) {
  const listing = `${outcome.entries} ${outcome.entries === 1 ? "entry" : "entries"}`;
  if (outcome.changes.length === 0) {
    return (
      `Merged both branch versions into ${file}; neither changed the ` +
      `ancestor, so all ${listing} carry across unchanged.`
    );
  }
  return [
    `Merged both branch versions into ${file}, now listing ${listing}:`,
    ...outcome.changes.map((change) => `  ${change}`),
  ].join("\n");
}

export function reportRecordState(outcome, file) {
  const { identity, key, refinement, approach, assessment } = outcome.state;
  const approachText =
    approach.kind === "planned" ? `planned (${approach.plan})` : approach.kind;
  const verb = outcome.result === "recorded" ? "Recorded" : "Replaced";
  const assessmentText =
    assessment?.status === "ready" || assessment?.status === "not-ready"
      ? ` Assessment ${assessment.status}${
          assessment.status === "not-ready"
            ? ` (${assessment.reasons.join("; ")})`
            : ""
        }.`
      : "";
  const backlogText =
    outcome.linked?.result === "linked"
      ? `Linked its ${takenHeading} entry to the plan ${outcome.target} in ` +
        `${file}; other stories and entries were not changed.`
      : `Other stories and the backlog were not changed.`;
  return (
    `${verb} preparation for "${identity}" in ${key}: ` +
    `refinement ${refinement}, approach ${approachText}.` +
    `${assessmentText} ${backlogText}`
  );
}

export function reportAdopt(outcome, file) {
  if (outcome.written.length === 0 && outcome.relabelled.length === 0) {
    return (
      `All ${outcome.entries} active entries already record their identity; ` +
      `${file} is unchanged.`
    );
  }
  const report =
    outcome.written.length === 0
      ? [
          `All ${outcome.entries} active entries already record their ` +
            `identity in their canonical homes.`,
        ]
      : [
          `Recorded ${outcome.written.length} identities for ${outcome.entries} active entries:`,
          ...outcome.written.map(
            (home) => `  ${home.identity} in ${home.relative}`,
          ),
        ];
  if (outcome.relabelled.length > 0) {
    report.push(
      `Entries now naming their identity in ${file}:`,
      ...outcome.relabelled.map(
        (entry) => `  ${entry.identity} — ${entry.title}`,
      ),
    );
  }
  return report.join("\n");
}
