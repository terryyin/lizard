// Adds exactly one already identified entry to "## Backlog list" at an
// explicit relative position. Identities are never allocated here, and nothing
// else in the document changes.

import { existsSync } from "node:fs";
import { resolve } from "node:path";
import {
  parseBacklog,
  renderBacklog,
  renderEntry,
  requireUnlistedHome,
} from "./product-backlog-document.mjs";
import { namedIdentity, openHome } from "./product-backlog-home.mjs";
import {
  recordsIdentityInFull,
  recordsOwnIdentity,
  splitHref,
} from "./product-backlog-identity.mjs";
import {
  insertEntryLine,
  queueIndexFor,
} from "./product-backlog-placement.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

export function requireUnlistedWork(document, request) {
  const existing = document.entries.find(
    (entry) => entry.identity === request.identity,
  );
  if (existing) {
    throw new BacklogError(
      `Identity "${request.identity}" is already listed in ` +
        `"## ${existing.list}" at line ${existing.index + 1}. This ` +
        `operation never re-identifies or moves existing work.`,
    );
  }
  requireUnlistedHome(document, request.href);
}

// An entry whose link already spells its identity exactly names nothing
// beyond that link, so an anchored home has nothing to confirm. An entry that
// claims a name of its own must have that name in the canonical home the link
// points at, read the same way adoption reads it — recorded there already, or,
// for an anchored story that has recorded nothing yet, composed from the
// seed's own document ID and the story's anchor. This never asks the link path
// itself to spell the identity: a home whose record matches while its own path
// has moved on is the ordinary case of relocation, not a refusal.
//
// A whole-document home (no anchor, such as a plan-homed correction's own
// plan) need not be there yet. When it is, it may not name different work: a
// plan recording another identity, including the anchored story it was written
// for, belongs to that work item, which is listed through its own home.
export function requireNamedHome(backlogDirectory, request) {
  if (splitHref(request.href).anchor === "") {
    requireWholeDocumentHome(backlogDirectory, request);
    return;
  }
  if (!recordsOwnIdentity(request.identity, request.href)) {
    return;
  }
  const home = openHome(backlogDirectory, request.href);
  const named = namedIdentity(home);
  if (named === undefined) {
    throw new BacklogError(
      `${home.key} names no identity of its own, so nothing establishes ` +
        `that it is the canonical home of "${request.identity}". Record ` +
        `the identity there first, or add the entry under the identity the ` +
        `home already names.`,
    );
  }
  if (named !== request.identity) {
    throw differentWork(home.key, named, request.identity);
  }
}

function differentWork(key, named, identity) {
  return new BacklogError(
    `${key} names identity "${named}", not "${identity}", ` +
      `so it is the canonical home of different work. This operation ` +
      `never lets an entry claim a name its own canonical home does not ` +
      `give it.`,
  );
}

function requireWholeDocumentHome(backlogDirectory, request) {
  if (!existsSync(resolve(backlogDirectory, request.href))) {
    return;
  }
  const home = openHome(backlogDirectory, request.href);
  const named = home.recorded?.identity;
  if (named === undefined) {
    return;
  }
  if (recordsIdentityInFull(named)) {
    throw new BacklogError(
      `${home.key} records the story identity "${named}", so it is that ` +
        `story's plan, not a canonical home of its own. List the story ` +
        `through its seed section instead.`,
    );
  }
  if (named !== request.identity) {
    throw differentWork(home.key, named, request.identity);
  }
}

export function addQueueEntry(source, request) {
  const document = parseBacklog(source);
  const line = renderEntry(request);
  requireUnlistedWork(document, request);
  requireNamedHome(request.backlogDirectory, request);

  insertEntryLine(document, queueIndexFor(document, request), line);
  return renderBacklog(document);
}
