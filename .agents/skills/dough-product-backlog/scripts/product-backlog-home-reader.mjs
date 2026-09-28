// Interprets a work item's canonical home from document text: one anchored
// story section inside a seed, or one whole document such as an active plan.
// Reading region and recorded identity lives here so CLI and browser share one
// meaning without importing filesystem or store operations.
//
// This owns how a home is read from text, not where that text was loaded from
// and not which identity a work item is allocated.

import { composeIdentity, splitHref } from "./product-backlog-identity.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { splitSource } from "./product-backlog-source.mjs";

const identityPattern = /^\*\*Identity:\*\* +(?<identity>\S.*?) *$/;
const frontMatterIdPattern = /^id: +(?<id>\S+) *$/;
const fence = "---";

// The one spelling of a recorded identity line in home text. Writers and the
// reader share this form so a line written under a heading still matches.
export function identityLine(identity) {
  return `**Identity:** ${identity}`;
}

// The whole document, or just the story section the anchor names.
function regionFor(lines, relative, anchor) {
  if (anchor === "") {
    const heading = lines.findIndex((line) => line.startsWith("# "));
    if (heading === -1) {
      throw new BacklogError(
        `${relative} has no "# " title to record an identity under.`,
      );
    }
    return { start: 0, end: lines.length, heading };
  }

  const start = lines.findIndex(
    (line) => line.trim() === `<a id="${anchor}"></a>`,
  );
  if (start === -1) {
    throw new BacklogError(
      `${relative} has no story anchored at "${anchor}"; the backlog links ` +
        `to a section that is not there.`,
    );
  }
  const region = anchoredSection(lines, start);
  if (region.heading === -1) {
    throw new BacklogError(
      `${relative} has no "### " story heading under the anchor "${anchor}".`,
    );
  }
  return region;
}

// A line that opens an anchored section and so ends the one before it.
function isAnchorLine(line) {
  return line.startsWith('<a id="');
}

// One anchored section from its anchor line to the next anchor line or "## "
// heading, with its first "### " heading, or -1 when it holds none.
function anchoredSection(lines, start) {
  let end = lines.length;
  for (let index = start + 1; index < lines.length; index += 1) {
    if (isAnchorLine(lines[index]) || lines[index].startsWith("## ")) {
      end = index;
      break;
    }
  }
  const heading = lines.findIndex(
    (line, index) => index > start && index < end && line.startsWith("### "),
  );
  return { start, end, heading };
}

// The home's document lines without the other stories' sections: every other
// anchored section holding a "### " story heading is left out, and everything
// else — front matter, title, seed-level prose, and this story's own section —
// stays as shared context. Blank lines left at the end are dropped, so closing
// the last sibling reads the same whether or not its separating blank line
// went with it. A whole-document home leaves nothing out.
export function storyContextLines(home) {
  const { lines } = home.document;
  if (home.anchor === "") {
    return lines;
  }
  const kept = [];
  let index = 0;
  while (index < lines.length) {
    const other =
      index !== home.region.start && isAnchorLine(lines[index])
        ? anchoredSection(lines, index)
        : undefined;
    if (other !== undefined && other.heading !== -1) {
      index = other.end;
      continue;
    }
    kept.push(lines[index]);
    index += 1;
  }
  while (kept.length > 0 && kept[kept.length - 1].trim() === "") {
    kept.pop();
  }
  return kept;
}

// The immutable document ID a seed already carries. Adoption reuses it rather
// than allocating a number of its own, so no shared registry is needed.
function documentIdFor(lines) {
  if (lines[0] !== fence) {
    return "";
  }
  const close = lines.findIndex((line, index) => index > 0 && line === fence);
  const end = close === -1 ? lines.length : close;
  for (let index = 1; index < end; index += 1) {
    const match = frontMatterIdPattern.exec(lines[index]);
    if (match) {
      return match.groups.id;
    }
  }
  return "";
}

function recordedIn(lines, region, relative) {
  const found = [];
  for (let index = region.start; index < region.end; index += 1) {
    const match = identityPattern.exec(lines[index]);
    if (match) {
      found.push({ identity: match.groups.identity, index });
    }
  }
  if (found.length > 1) {
    throw new BacklogError(
      `${relative} records an identity ${found.length} times at lines ` +
        `${found.map((entry) => entry.index + 1).join(" and ")}; a human ` +
        `decides which one this work item keeps.`,
    );
  }
  return found[0];
}

// Reads the canonical home a backlog link names from already-loaded source
// text and reports what it already records. Throws a BacklogError describing
// the home when it cannot be used.
export function readHome(source, href) {
  const { path: relative, anchor } = splitHref(href);
  const document = splitSource(source);
  const region = regionFor(document.lines, relative, anchor);
  return {
    href,
    relative,
    anchor,
    // Two stories in one seed are separate homes, so the anchor is part of it.
    key: anchor === "" ? relative : `${relative}#${anchor}`,
    document,
    region,
    documentId: anchor === "" ? "" : documentIdFor(document.lines),
    recorded: recordedIn(document.lines, region, relative),
  };
}

// The identity an anchored home implies before anything is recorded there:
// the seed's own document ID composed with the anchor — the same evidence a
// work item's identity is first taken from. A whole-document home, or an
// anchored home whose seed carries no "id:" of its own, implies none this
// way. Both adoption and the canonical-home check at `add` read this same
// implied identity when nothing has been recorded yet.
export function impliedIdentity(home) {
  if (home.anchor === "" || home.documentId === "") {
    return undefined;
  }
  return composeIdentity(home.documentId, home.anchor);
}

// The identity a home names on its own, without writing anything: an explicit
// `**Identity:**` record when the home carries one, or otherwise the identity
// it implies. A whole-document home that has recorded nothing names none on
// its own; there is no link-derived fallback here, because a link agreeing
// with an identity is not the same as the home naming it.
export function namedIdentity(home) {
  return home.recorded ? home.recorded.identity : impliedIdentity(home);
}
