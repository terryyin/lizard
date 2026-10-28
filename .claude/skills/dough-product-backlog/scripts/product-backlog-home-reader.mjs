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
import { storyDependenciesFence } from "./product-backlog-story-state-basis.mjs";
import { storyStateFence } from "./product-backlog-story-state-block.mjs";

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

// Evidence of an intended home, including malformed records that the strict
// reader must report. Identity wins for whole-document homes; otherwise keep
// dependency-before-preparation reporting when both records are orphaned.
function discoveryRecord(lines) {
  if (lines.some((line) => /^\*\*Identity:\*\*/.test(line))) return "Identity";
  return [
    [storyDependenciesFence, "Dependency"],
    [storyStateFence, "Preparation"],
  ].find(([fence]) =>
    lines.some((line) => line.trim().startsWith(`\`\`\`${fence}`)),
  )?.[1];
}

// Discovery distinguishes navigation anchors from story homes using the same
// section/heading meaning as readHome. Records still identify an intended home
// when its heading is malformed, including an accidental "## " heading. Look
// for that evidence up to the next anchor so discovery reports the reader's
// error rather than silently dropping a recorded story.
function discoveryAnchors(lines) {
  const anchors = [];
  for (let start = 0; start < lines.length; start += 1) {
    const match = /^<a id="([^"\n]+)"><\/a> *$/.exec(lines[start]);
    if (!match) continue;
    let end = start + 1;
    while (end < lines.length && !isAnchorLine(lines[end])) end += 1;
    const recorded = discoveryRecord(lines.slice(start + 1, end));
    if (anchoredSection(lines, start).heading !== -1 || recorded)
      anchors.push(match[1]);
  }
  return anchors;
}

// Classifies text for discovery without accepting it as a valid home. The
// caller supplies document paths; readHome owns subsequent strict validation.
export function discoveryCandidates(source) {
  const { lines } = splitSource(source);
  const anchors = discoveryAnchors(lines);
  const record = anchors.length
    ? undefined
    : discoveryRecord(source.split(/\r\n?|\n/));
  return {
    anchors,
    wholeDocument: record === "Identity",
    orphanedRecord: record === "Identity" ? undefined : record,
  };
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

// The seed ID a document's front matter carries, or "" when it carries none.
export function documentIdIn(source) {
  return documentIdFor(splitSource(source).lines);
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
