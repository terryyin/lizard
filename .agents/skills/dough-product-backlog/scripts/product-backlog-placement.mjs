// Which entries a list holds, which entry an operation was asked to change,
// where an entry line sits inside one of the backlog's two lists, and how a
// line is written or taken out there without disturbing the text around it.
// Adding an entry, moving one between lists, refreshing one, and removing one
// ask the same questions, so they ask them here.

import { queueHeading, takenHeading } from "./product-backlog-document.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";

export function entriesIn(document, name) {
  return document.entries.filter((entry) => entry.list === name);
}

// Names the one active entry an operation was asked to change. An absent
// identity is a refusal rather than an empty result, so no operation invents
// work the backlog does not carry or silently does nothing; `hint` says what
// the caller of that particular operation can do about it.
export function findEntry(document, identity, hint) {
  requireField(identity, "identity");
  const entry = document.entries.find((each) => each.identity === identity);
  if (!entry) {
    throw new BacklogError(
      `Identity "${identity}" is in neither "## ${takenHeading}" nor ` +
        `"## ${queueHeading}". ${hint}`,
    );
  }
  return entry;
}

// The line a new entry takes at the end of a section, including a section that
// holds no entries yet.
export function appendIndex(document, section) {
  const entries = entriesIn(document, section.name);
  if (entries.length > 0) {
    return entries[entries.length - 1].index + 1;
  }
  let index = section.start;
  if (document.lines[index] === "") {
    index += 1;
  }
  return index;
}

// Where an entry belongs in "## Backlog list" for one explicit destination
// relationship, asked of a document that does not hold that entry. Writing a
// new entry and moving one that is already listed ask exactly this question, so
// both order the queue by the same rule: a destination is a relationship to
// another identity or to an end of the list, never the line number an entry
// happens to occupy.
export function queueIndexFor(document, placement) {
  const queued = entriesIn(document, queueHeading);

  if (placement.position === "first") {
    return queued.length > 0
      ? queued[0].index
      : appendIndex(document, document.queue);
  }
  if (placement.position === "last") {
    return appendIndex(document, document.queue);
  }

  const wanted = placement.after ?? placement.before;
  const anchor = queued.find((entry) => entry.identity === wanted);
  if (!anchor) {
    const elsewhere = document.entries.find(
      (entry) => entry.identity === wanted,
    );
    throw new BacklogError(
      elsewhere
        ? `Anchor identity "${wanted}" is in "## ${elsewhere.list}"; this ` +
            `operation only places entries in "## ${queueHeading}".`
        : `Anchor identity "${wanted}" is not in "## ${queueHeading}".`,
    );
  }
  return placement.after ? anchor.index + 1 : anchor.index;
}

// Writes the line at `index`, keeping the blank line that separates the last
// entry of a section from whatever follows it.
export function insertEntryLine(document, index, line) {
  document.lines.splice(index, 0, line);
  const following = document.lines[index + 1];
  if (
    following !== undefined &&
    following !== "" &&
    !following.startsWith("- ")
  ) {
    document.lines.splice(index + 1, 0, "");
  }
}

// Rewrites the entry at `from` as `line` at `to`, an index read while the
// entry was still in place; removing it first shifts everything after it.
export function moveEntryLine(document, from, to, line) {
  document.lines.splice(from, 1);
  insertEntryLine(document, to > from ? to - 1 : to, line);
}

// Takes the entry line at `index` out, collapsing only the blank line that the
// removed entry itself was holding apart from what follows its list. Every
// other line, including the text around both lists, is left as it was.
export function removeEntryLine(document, index) {
  document.lines.splice(index, 1);
  if (document.lines[index - 1] === "" && document.lines[index] === "") {
    document.lines.splice(index, 1);
  }
}
