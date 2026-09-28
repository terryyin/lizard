// The backlog's near-future direction: one value, recorded exactly as the
// caller supplies it. This tool never writes, summarises, or reflows strategy
// text of its own — someone has already chosen the direction, and this records
// their choice, creating the section, replacing what it says, or clearing it.
// Reading the direction as one value lives here too, beside writing it, so
// anything that later has to compare two directions asks this module rather
// than reading the lines for itself.

import {
  parseBacklog,
  renderBacklog,
  sectionsNamed,
} from "./product-backlog-document.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";

export const directionHeading = "Near-future direction";

// Line endings belong to the file, not to the request: the text a caller
// supplies and the text they say they read are compared, and written, in the
// backlog's own convention. That is the only change ever made to either.
function normalize(text) {
  return text.replace(/\r\n/g, "\n");
}

// Where the section sits, and nothing about what it says. An absent section is
// located at "## Taken", which is where the established backlog keeps the
// direction, so creating one and replacing one write in the same place.
function sectionOf(document) {
  const matches = sectionsNamed(document, directionHeading);
  if (matches.length > 1) {
    throw new BacklogError(
      `Expected at most one "## ${directionHeading}" section; found ` +
        `${matches.length}. Repair it by hand before running this operation.`,
    );
  }
  const found = matches[0];
  if (!found) {
    const before = document.taken.heading;
    return { present: false, heading: before, end: before };
  }
  return { present: true, heading: found.heading, end: found.end };
}

// The direction as one value: the text the section holds, or "" when the
// backlog carries no direction. A section left with no text says no more than
// an absent one, so both read as no direction and are stated the same way.
export function directionOf(document) {
  const section = sectionOf(document);
  const body = document.lines.slice(section.heading + 1, section.end);
  while (body.length > 0 && body[0] === "") {
    body.shift();
  }
  while (body.length > 0 && body[body.length - 1] === "") {
    body.pop();
  }
  return body.join("\n");
}

// The lines a direction is written as: its heading, the text exactly as it was
// supplied, and the blank lines that hold both apart from what surrounds them
// — or no lines at all when there is no direction. Recording a direction and
// writing out a whole merged backlog compose the section here, so `directionOf`
// always reads back what either of them wrote.
export function directionLines(text) {
  return text === ""
    ? []
    : [`## ${directionHeading}`, "", ...text.split("\n"), ""];
}

// What this request asks the direction to become, and the direction the caller
// read it against. Both are stated the same way, and both are required: an
// empty --text or --expect cannot be told apart from a variable the caller
// never set, so "no direction" is always its own option rather than empty
// text, and a request that leaves either side unsaid is refused.
const wantedDirection = {
  option: "text",
  absent: "clear",
  promise: "states what the direction should say",
  hint: "Removing the direction is its own request: supply --clear.",
};

const readDirection = {
  option: "expect",
  absent: "expect-none",
  promise: "states the direction it was written against",
  hint:
    "Expecting no direction at all is its own request: supply " +
    "--expect-none.",
};

function statedText({ option, absent, promise, hint }, text, none) {
  if (none === (text !== undefined)) {
    throw new BacklogError(
      `Supply exactly one of --${option} <text> or --${absent}, so that a ` +
        `direction update always ${promise}.`,
    );
  }
  if (none) {
    return "";
  }
  requireField(text, option, ` ${hint}`);
  return normalize(text);
}

function quoted(text) {
  return text === "" ? "  (no direction)" : text.replace(/^/gm, "  ");
}

// A request is applied to the direction it was written against or to nothing
// at all, so a direction someone else has since changed is never overwritten
// unknowingly.
function requireExpected(current, expected) {
  if (current === expected) {
    return;
  }
  throw new BacklogError(
    `The direction this request was written against is not the direction the ` +
      `backlog carries, so nothing was written.\nExpected:\n${quoted(expected)}` +
      `\nFound:\n${quoted(current)}\nRead the direction the backlog carries ` +
      `now and write the request against that.`,
  );
}

// Which of the three outcomes this request asked for, read from the document
// it was applied to rather than from the words of the request.
function outcomeOf(present, current, wanted) {
  if (wanted === "") {
    return present ? "cleared" : "unchanged";
  }
  if (current === wanted) {
    return "unchanged";
  }
  return current === "" ? "set" : "replaced";
}

// Applies the direction update to one backlog document and returns the backlog
// to publish alongside which of the outcomes it turned out to be.
export function setDirection(source, request) {
  const document = parseBacklog(source);
  const wanted = statedText(wantedDirection, request.text, request.clearing);
  const current = directionOf(document);
  requireExpected(
    current,
    statedText(readDirection, request.expected, request.expectingNone),
  );

  const section = sectionOf(document);
  const lines = [...document.lines];
  lines.splice(
    section.heading,
    section.end - section.heading,
    ...directionLines(wanted),
  );

  // Reading the candidate back is what keeps the supplied text the recorded
  // text: anything that would not read back as supplied — a blank line at
  // either end, a "## " heading of its own — is refused rather than tidied.
  // The same read re-validates both lists and every entry around it.
  const candidate = renderBacklog({ ...document, lines });
  if (directionOf(parseBacklog(candidate)) !== wanted) {
    throw new BacklogError(
      `The supplied direction text would not read back as it was supplied, ` +
        `so nothing was written. The text is recorded exactly as given and is ` +
        `never reflowed or tidied here: supply it without a blank line at ` +
        `either end and without a "## " heading of its own.`,
    );
  }

  return {
    source: candidate,
    result: outcomeOf(section.present, current, wanted),
  };
}
