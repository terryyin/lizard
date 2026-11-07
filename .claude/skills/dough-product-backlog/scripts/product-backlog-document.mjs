// Parses and rewrites the established product backlog Markdown shape: an
// optional "## Near-future direction" section, then "## Taken", then
// "## Backlog list". Everything outside a changed entry line is preserved byte
// for byte, and the backlog lists each work item exactly once. This is the
// document model, not a general Markdown editing framework; how an identity is
// spelled belongs beside it, and so do the operations that change a backlog.

import {
  adoptionHint,
  ambiguousHome,
  identityFor,
  recordedFor,
  recordsIdentityInFull,
} from "./product-backlog-identity.mjs";
import { planNamesHome } from "./product-backlog-plan.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { joinSource, splitSource } from "./product-backlog-source.mjs";

export const takenHeading = "Taken";
export const queueHeading = "Backlog list";

const separator = " — ";
const entryPattern = /^- \[(?<title>[^[\]]+)\]\((?<href>[^)\s]+)\)(?<rest>.*)$/;
// The recorded identity and the active-plan link are each optional, so an
// entry written before its identity was adopted, and one whose link already
// spells its identity, both still read as the same work item.
const detailPattern =
  /^(?: — (?<recorded>[^\s()[\]]+))?(?: \(\[(?<label>[^[\]]+)\]\((?<target>[^)\s]+)\)\))?$/;

function readEntry(line, index, list) {
  const match = entryPattern.exec(line);
  if (!match) {
    throw new BacklogError(
      `Unsupported entry in "## ${list}" at line ${index + 1}: ${line}`,
    );
  }
  const { title, href, rest } = match.groups;
  let recorded = "";
  let plan;
  if (rest !== "") {
    const detail = detailPattern.exec(rest);
    if (!detail) {
      throw new BacklogError(
        `Unsupported entry detail in "## ${list}" at line ${index + 1}: ${line}`,
      );
    }
    recorded = detail.groups.recorded ?? "";
    if (detail.groups.target !== undefined) {
      plan = { label: detail.groups.label, target: detail.groups.target };
    }
  }
  return {
    identity: identityFor(href, recorded),
    title,
    href,
    plan,
    list,
    index,
    // Whether the entry already carries its identity in full, and the line as
    // it stands: together they tell such an entry from one that only reads as
    // the same identity through the link beside it.
    recordsIdentityInFull: recordsIdentityInFull(recorded),
    line,
  };
}

// Reads one established entry bullet, for callers that hold a written line
// rather than its separate fields.
export function parseEntryLine(line) {
  return readEntry(line, 0, queueHeading);
}

// Where each "## <name>" section sits: its own heading line, the first line
// after it, and the line it stops before, which is the next heading or the end
// of the file. The two required lists and the optional direction section are
// all located by this one rule.
export function sectionsNamed({ lines, headings }, name) {
  return headings
    .filter((heading) => heading.title === name)
    .map((heading) => {
      const following = headings[headings.indexOf(heading) + 1];
      return {
        name,
        heading: heading.index,
        start: heading.index + 1,
        end: following ? following.index : lines.length,
      };
    });
}

function sectionFor(document, name) {
  const matches = sectionsNamed(document, name);
  if (matches.length !== 1) {
    throw new BacklogError(
      `Expected exactly one "## ${name}" section; found ${matches.length}.`,
    );
  }
  return matches[0];
}

function readSection(lines, section) {
  const entries = [];
  for (let index = section.start; index < section.end; index += 1) {
    const line = lines[index];
    if (line.trim() === "") {
      continue;
    }
    if (!line.startsWith("- ")) {
      throw new BacklogError(
        `Unsupported text in "## ${section.name}" at line ${index + 1}: ${line}`,
      );
    }
    entries.push(readEntry(line, index, section.name));
  }
  return entries;
}

// Whether one entry's plan link names the document another entry lists as its
// canonical home: a plan attached to a story is part of that story's work, so
// listing the plan again as a home of its own would list the work twice. A
// link into a section of that plan names the same document.
function planOfOther(entry, other) {
  return (
    planNamesHome(entry.plan?.target, other.href) ||
    planNamesHome(other.plan?.target, entry.href)
  );
}

// The backlog lists each work item once, by identity, by canonical home, and
// by the plan a story links: the invariant a parsed document already satisfies.
function requireDistinctWork(entries) {
  for (const entry of entries) {
    const clash = entries.find(
      (other) =>
        other !== entry &&
        other.index < entry.index &&
        (other.identity === entry.identity ||
          other.href === entry.href ||
          planOfOther(entry, other)),
    );
    if (clash) {
      throw new BacklogError(
        `The backlog already lists the same work twice: lines ` +
          `${clash.index + 1} and ${entry.index + 1}. Repair it by hand ` +
          `before running this operation.`,
      );
    }
  }
}

// An entry other than `carried`, the one an operation is writing, that
// already lists what `listsIt` looks for.
function otherEntry(document, carried, listsIt) {
  return document.entries.find((other) => other !== carried && listsIt(other));
}

// The same invariant asked forwards, about a home an operation is about to
// write: no other entry may already link it. Writing a new entry and
// repointing an existing one both ask this; `carried` is the entry being
// repointed, which never clashes with itself.
export function requireUnlistedHome(document, href, carried) {
  const listed = otherEntry(document, carried, (other) => other.href === href);
  if (listed) {
    throw ambiguousHome(
      `the canonical home "${href}" is already listed in ` +
        `"## ${listed.list}" at line ${listed.index + 1} as identity ` +
        `"${listed.identity}".`,
    );
  }
  const linking = otherEntry(document, carried, (other) =>
    planNamesHome(other.plan?.target, href),
  );
  if (linking) {
    throw ambiguousHome(
      `"${href}" is already the plan of "${linking.identity}" in ` +
        `"## ${linking.list}" at line ${linking.index + 1}; that entry ` +
        `already lists this work.`,
    );
  }
}

// The same invariant asked of a plan link an operation is about to write: the
// plan must not already be listed as another entry's canonical home.
export function requireUnlistedPlan(document, target, carried) {
  const listed = otherEntry(document, carried, (other) =>
    planNamesHome(target, other.href),
  );
  if (listed) {
    throw ambiguousHome(
      `the plan "${target}" is already listed in "## ${listed.list}" at ` +
        `line ${listed.index + 1} as the canonical home of ` +
        `"${listed.identity}"; linking it again would list that work twice.`,
    );
  }
}

export function parseBacklog(source) {
  const { lines, newline, hasFinalNewline } = splitSource(source);

  const headings = [];
  lines.forEach((line, index) => {
    if (line.startsWith("## ")) {
      headings.push({ title: line.slice(3).trim(), index });
    }
  });

  // The headings come back with the document, so the optional direction
  // section can be located against them by the module that owns it.
  const located = { lines, headings };
  const taken = sectionFor(located, takenHeading);
  const queue = sectionFor(located, queueHeading);
  const entries = [...readSection(lines, taken), ...readSection(lines, queue)];
  requireDistinctWork(entries);

  return { lines, newline, hasFinalNewline, headings, taken, queue, entries };
}

export function renderBacklog(document) {
  return joinSource(document);
}

export function renderEntry({ identity, title, href, plan }) {
  requireField(identity, "identity", ` ${adoptionHint}`);
  requireField(title, "title");
  requireField(href, "link");
  if (/[[\]]/.test(title)) {
    throw new BacklogError(`Unsupported title characters: ${title}`);
  }
  if (/[\s)]/.test(href)) {
    throw new BacklogError(`Unsupported link characters: ${href}`);
  }

  const recorded = recordedFor(identity, href);
  const detail = recorded === "" ? "" : `${separator}${recorded}`;
  const active = plan ? ` ([${plan.label}](${plan.target}))` : "";
  const line = `- [${title}](${href})${detail}${active}`;

  const rendered = readEntry(line, 0, queueHeading);
  if (rendered.identity !== identity) {
    throw ambiguousHome(
      `the written entry would read as identity "${rendered.identity}".`,
    );
  }
  return line;
}
