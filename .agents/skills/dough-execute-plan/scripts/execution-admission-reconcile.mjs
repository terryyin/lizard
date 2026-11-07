// Reconciling admitted content drafted in the originating checkout with
// fetched trunk, against the revision it was drafted from: the selected
// story's section of its seed (the whole seed when the seed itself is new)
// and a whole declared plan. Sibling sections on trunk survive, other local
// edits stay local, and an edit trunk also made differently stops with both
// versions intact for a human decision.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import {
  joinSource,
  splitSource,
} from "../../dough-product-backlog/scripts/product-backlog-source.mjs";
import { sectionOf, show } from "./execution-source.mjs";
import { git, revParse } from "./publication-git.mjs";

// A refusal that names its stop status, such as a reconciliation conflict.
export class AdmissionRefusal extends Error {
  constructor(status, message, fields = {}) {
    super(message);
    this.status = status;
    this.fields = fields;
  }
}

export const refused = (message, fields) =>
  new AdmissionRefusal("source-refused", message, fields);

// The originating checkout's working-tree copy, or null when it has none.
function worktreeSource(root, path) {
  try {
    return readFileSync(join(root, path), "utf8");
  } catch {
    return null;
  }
}

// Where admitted content is drafted, and the revision it was drafted from:
// the supplied originating (integration) checkout's worktree against its
// merge base with trunk, or a preserved claim candidate against its own
// parent. Recovery reconciles that candidate again, so later local drafting
// never changes an accepted admission. Without a supplied checkout nothing is
// drafted locally: the owned workspace is never read as drafts.
export async function draftsOf(request, remoteRef, candidateSha) {
  const { repository, integration } = request;
  if (candidateSha)
    return {
      base: await revParse(repository, `${candidateSha}^`),
      read: (path) => show(repository, candidateSha, path),
    };
  if (!integration) return { base: remoteRef, read: async () => null };
  return {
    base: (
      await git(integration, "merge-base", "HEAD", remoteRef)
    ).stdout.trim(),
    read: async (path) => worktreeSource(integration, path),
  };
}

// The file at the drafts' base, on fetched trunk, and as drafted. A file the
// drafts lack is drafted as trunk has it: there is nothing to carry.
export async function versionsOf(request, remoteRef, drafts, path) {
  const trunk = await show(request.repository, remoteRef, path);
  return {
    path,
    base: await show(request.repository, drafts.base, path),
    trunk,
    draft: (await drafts.read(path)) ?? trunk,
  };
}

const conflict = (path, message) =>
  new AdmissionRefusal(
    "source-conflict",
    `${path} ${message}; both versions are preserved`,
    { path },
  );

// Where the drafted section goes in a trunk seed that lacks it: before the
// section boundary that follows it in the draft, or at the end together
// with the blank lines that separate it.
function insertionIndex(lines, drafted, path) {
  const { lines: draft } = drafted.document;
  const { start, end } = drafted.region;
  if (end === draft.length) {
    let from = start;
    while (from > 0 && draft[from - 1] === "") from -= 1;
    return { at: lines.length, from };
  }
  const at = lines.indexOf(draft[end]);
  if (at === -1)
    throw conflict(path, "has no place for the new story section on trunk");
  return { at, from: start };
}

// Trunk's seed with only the selected story's section as drafted. Sibling
// sections keep trunk's text, and other local edits stay local. A section
// that trunk and the draft each changed from the merge base, or one removed
// on trunk, stops for a human decision. A seed new on both sides is owned
// whole.
export function reconcileStory({ base, trunk, draft, path }, href) {
  if (trunk === null) {
    if (base !== null) throw conflict(path, "was removed on fetched trunk");
    return draft;
  }
  const drafted = sectionOf(draft, href);
  const published = sectionOf(trunk, href);
  const original = sectionOf(base, href);
  if (published?.text === drafted.text) return trunk;
  if (published && published.text !== original?.text) {
    if (drafted.text === original?.text) return trunk;
    throw conflict(path, "has a story section also changed on fetched trunk");
  }
  if (!published && original)
    throw conflict(path, "lost the story section on fetched trunk");
  const document = splitSource(trunk);
  const lines = [...document.lines];
  const { lines: draftLines } = drafted.document;
  if (published) {
    const { start, end } = published.region;
    lines.splice(
      start,
      end - start,
      ...draftLines.slice(drafted.region.start, drafted.region.end),
    );
  } else {
    const { at, from } = insertionIndex(lines, drafted, path);
    lines.splice(at, 0, ...draftLines.slice(from, drafted.region.end));
  }
  return joinSource({ ...document, lines });
}

// A declared plan is owned whole: an edit on only one side wins, and
// different edits on both sides stop.
export function reconcilePlan({ base, trunk, draft, path }) {
  if (draft === base || draft === trunk) return trunk;
  if (trunk === base) return draft;
  throw conflict(path, "was also changed on fetched trunk");
}
