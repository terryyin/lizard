// One version of a backlog as a merge compares it, and the merged result
// written back out in the established shape.
//
// A merge never reads the two lists as text: it reads what each version says
// about each work item, what order each list is in, and the text around them,
// and it writes the result back through the same entry composer every other
// operation uses. Everything a version says that no merge decision touches —
// the title above the lists, whatever follows them — is carried across as the
// bytes it already was.

import {
  directionHeading,
  directionLines,
  directionOf,
} from "./product-backlog-direction.mjs";
import {
  parseBacklog,
  queueHeading,
  renderBacklog,
  renderEntry,
  sectionsNamed,
  takenHeading,
} from "./product-backlog-document.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { readFile } from "./product-backlog-store.mjs";

// Where the part of the file a merge rewrites begins, established only for a
// version holding the direction and the two lists one after another. A merge
// writes that run of sections back out itself and carries only the text above
// and below it across, so a section wedged between them would not survive.
// Refusing says so, rather than quietly dropping somebody's writing.
function requireEstablishedShape(document) {
  const held = sectionsNamed(document, directionHeading)[0];
  const named = held
    ? `"## ${directionHeading}", "## ${takenHeading}", and "## ${queueHeading}"`
    : `"## ${takenHeading}" and "## ${queueHeading}"`;
  const wanted = [
    held?.heading,
    document.taken.heading,
    document.queue.heading,
  ].filter((at) => at !== undefined);
  const found = document.headings
    .map((heading) => heading.index)
    .filter((at) => at >= wanted[0])
    .slice(0, wanted.length);

  if (wanted.some((at, order) => at !== found[order])) {
    throw new BacklogError(
      `${named} do not come one after another, in that order. A merge ` +
        `rewrites that run of sections, so anything held between them would ` +
        `not survive it. Put them back in order by hand, with any other ` +
        `writing above or below them, before merging.`,
    );
  }
  return wanted[0];
}

// Reads one supplied version. A version this tool cannot read is named, so a
// human repairs the file the merge actually choked on rather than guessing
// which of the three it was.
export function readVersion(path, label) {
  const source = readFile(
    path,
    `Not found: ${path} (${label}). Supply the path to each of the three ` +
      `versions this merge reconciles.`,
  );

  let document;
  let direction;
  let opening;
  try {
    document = parseBacklog(source);
    direction = directionOf(document);
    opening = requireEstablishedShape(document);
  } catch (error) {
    if (error instanceof BacklogError) {
      throw new BacklogError(
        `${label} (${path}) is not a backlog this tool can read, so nothing ` +
          `was merged.\n${error.message}`,
      );
    }
    throw error;
  }

  return {
    label,
    path,
    direction,
    newline: document.newline,
    hasFinalNewline: document.hasFinalNewline,
    preamble: document.lines.slice(0, opening).join("\n"),
    epilogue: document.lines.slice(document.queue.end).join("\n"),
    entries: document.entries,
  };
}

// The values one version's reading of a work item is made of, and what each of
// them is called when two branches disagree about it, so a human is told which
// meaning is in dispute rather than which field name the parser uses. Every
// value a state carries is named here: one that is not would be compared by
// nothing and merged by nothing.
export const valueNames = {
  identity: "identities",
  title: "titles",
  href: "canonical links",
  list: "lists",
  planLabel: "plan links",
  planTarget: "plan links",
};

// What one version says about one work item, as values a three-way comparison
// can be made of. The line number an entry happens to occupy is not one of
// them: where an entry sits is the list's order, which is merged separately.
export function stateOf(entry) {
  if (!entry) {
    return null;
  }
  return {
    identity: entry.identity,
    title: entry.title,
    href: entry.href,
    list: entry.list,
    planLabel: entry.plan ? entry.plan.label : "",
    planTarget: entry.plan ? entry.plan.target : "",
  };
}

// The line a merged work item is written as. `renderEntry` stays the only
// thing that composes an entry line, so a merged entry is written exactly as
// an added, taken, or refreshed one is — including its refusal when a merged
// identity and a merged canonical home would no longer name each other.
function entryLine(state) {
  return renderEntry({
    identity: state.identity,
    title: state.title,
    href: state.href,
    plan:
      state.planTarget === ""
        ? undefined
        : { label: state.planLabel, target: state.planTarget },
  });
}

// The merged backlog, in the shape the established format has: the text that
// opened the file, the direction if there is one, both headings, and each
// list's entries in the merged order. Line endings and the final newline are
// the ancestor's, because they belong to the file rather than to any change.
function renderCandidate(ancestor, merged) {
  const lines = [];
  if (merged.preamble !== "") {
    lines.push(...merged.preamble.split("\n"));
  }
  lines.push(...directionLines(merged.direction));

  lines.push(`## ${takenHeading}`, "");
  const taken = merged.taken.map(entryLine);
  if (taken.length > 0) {
    lines.push(...taken, "");
  }
  lines.push(`## ${queueHeading}`, "", ...merged.queue.map(entryLine));
  if (merged.epilogue !== "") {
    lines.push("", ...merged.epilogue.split("\n"));
  }

  return renderBacklog({
    lines,
    newline: ancestor.newline,
    hasFinalNewline: ancestor.hasFinalNewline,
  });
}

// The bytes a merge publishes, written and then read straight back. Reading
// the candidate re-runs the parser's strictness and the rule that the backlog
// lists each work item once on the very bytes about to be written, and writing
// it re-runs every refusal an entry line carries. A candidate that fails
// either is a merge this tool got wrong, so it is refused whole rather than
// published in part.
export function publishableCandidate(ancestor, merged) {
  try {
    const candidate = renderCandidate(ancestor, merged);
    if (directionOf(parseBacklog(candidate)) !== merged.direction) {
      throw new BacklogError(
        `its "## ${directionHeading}" would not read back as merged.`,
      );
    }
    return candidate;
  } catch (error) {
    if (error instanceof BacklogError) {
      throw new BacklogError(
        `The merged backlog would not be a backlog this tool can read, so ` +
          `nothing was written.\n${error.message}\nRepair the supplied ` +
          `versions by hand and merge them again.`,
      );
    }
    throw error;
  }
}
