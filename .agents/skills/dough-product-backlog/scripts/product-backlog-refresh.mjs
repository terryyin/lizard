// Refreshes what one listed work item says about itself: its title, the
// canonical document its link names, and the active plan it links to. The
// item's identity is carried across unchanged, which is the whole point of
// this operation — the same work is still the same work after its story or
// plan has been renamed or moved.
//
// The canonical change has already happened: a human, or another workflow,
// renamed or relocated the document. This applies that established change to
// the backlog's reference to it. It renames no document, moves no file,
// allocates no identity, and repairs no link anywhere else in the project.
// Membership and order are not its business either: the entry keeps the list
// and the position it already held.

import {
  parseBacklog,
  renderBacklog,
  renderEntry,
  requireUnlistedHome,
} from "./product-backlog-document.mjs";
import {
  openHome,
  requireResolvedPlan,
  stillRecords,
} from "./product-backlog-home.mjs";
import { findEntry } from "./product-backlog-placement.mjs";
import { sameDocument } from "./product-backlog-plan.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

// What a caller can do when the named identity is in neither list. A refresh
// changes what an existing entry says, so it never writes an entry the
// backlog does not already carry.
const absent =
  `Nothing was refreshed. Either this is not the identity the backlog ` +
  `carries, or the work is not listed at all: read the two lists and name ` +
  `the entry to refresh, or add it.`;

// A refresh applies a change already made to a canonical document, so the
// caller says what that document is now called or where it now is.
function requireRequestedChange(request) {
  const named = ["title", "href", "plan"].filter(
    (field) => request[field] !== undefined,
  );
  if (named.length === 0) {
    throw new BacklogError(
      `Missing refreshed reference: supply at least one of --title, --link, ` +
        `or --plan. This operation records an established rename or move, ` +
        `and a request naming none of them asks for no change.`,
    );
  }
}

// The moved document's own recorded identity is the evidence tying the old
// reference to the new one. That record is what survives a rename, which the
// backlog line alone could not, so a home that does not carry it does not
// establish the move.
function requireNewHome(backlogDirectory, entry, href) {
  const home = openHome(backlogDirectory, href);
  if (!home.recorded) {
    throw new BacklogError(
      `${home.key} records no identity, so nothing establishes that it is ` +
        `now the canonical home of "${entry.identity}". Record the identity ` +
        `in the moved document first; this operation reads that record and ` +
        `never writes one.`,
    );
  }
  if (home.recorded.identity !== entry.identity) {
    throw new BacklogError(
      `${home.key} records identity "${home.recorded.identity}", not ` +
        `"${entry.identity}", so it is the canonical home of different ` +
        `work. Refreshing a reference never points an entry at another ` +
        `work item.`,
    );
  }
}

// The reference this refresh drops must not still claim the identity, or the
// same work item would be reachable through two active homes at once.
function requireReleased(backlogDirectory, previous, identity, what) {
  if (stillRecords(backlogDirectory, previous, identity)) {
    throw new BacklogError(
      `The ${what} "${previous}" still records identity "${identity}", so ` +
        `refreshing the reference would leave two documents claiming one ` +
        `work item. Finish the move first: a human decides which document ` +
        `keeps the identity, and this operation changes no canonical home.`,
    );
  }
}

// The plan link the refreshed entry carries. A refresh repoints a reference
// the entry already holds; giving work its first plan link is part of
// claiming it, and dropping one is not offered at all.
function planFor(entry, request) {
  if (request.plan === undefined) {
    return entry.plan;
  }
  if (!entry.plan) {
    throw new BacklogError(
      `"${entry.identity}" links no active plan, so there is no plan ` +
        `reference to refresh. Linking a plan is part of claiming the work: ` +
        `take --identity "${entry.identity}" --plan ${request.plan}.`,
    );
  }
  if (request.plan === entry.plan.target) {
    return entry.plan;
  }
  if (sameDocument(request.plan, request.href ?? entry.href)) {
    throw new BacklogError(
      `The plan "${request.plan}" is the canonical home this entry links, ` +
        `which needs no duplicate plan link.`,
    );
  }
  requireResolvedPlan(
    request.backlogDirectory,
    request.plan,
    `Refresh the link once the moved plan is in place.`,
  );
  return { label: entry.plan.label, target: request.plan };
}

function changedFields(entry, { title, href, plan }) {
  return [
    title === entry.title ? "" : "title",
    href === entry.href ? "" : "canonical link",
    plan?.target === entry.plan?.target ? "" : "plan link",
  ].filter(Boolean);
}

// Applies the refresh to one backlog document and returns the backlog to
// publish alongside the entry it refreshed and what about it changed.
export function refreshEntry(source, request) {
  const document = parseBacklog(source);
  const entry = findEntry(document, request.identity, absent);
  requireRequestedChange(request);

  const refreshed = {
    title: request.title ?? entry.title,
    href: request.href ?? entry.href,
    plan: planFor(entry, request),
  };
  // Written before any reference is established, so a title or link the
  // backlog cannot carry — including one the identity would not read back
  // from — is refused before the canonical homes are consulted.
  const line = renderEntry({ identity: entry.identity, ...refreshed });

  if (refreshed.href !== entry.href) {
    requireUnlistedHome(document, refreshed.href, entry);
    requireNewHome(request.backlogDirectory, entry, refreshed.href);
    requireReleased(
      request.backlogDirectory,
      entry.href,
      entry.identity,
      "canonical home",
    );
  }
  if (entry.plan && refreshed.plan.target !== entry.plan.target) {
    requireReleased(
      request.backlogDirectory,
      entry.plan.target,
      entry.identity,
      "plan",
    );
  }

  document.lines[entry.index] = line;
  const published = renderBacklog(document);
  // The candidate is read back, which re-runs the parser's strictness and its
  // one-entry-per-work-item check on the refreshed line before it is written.
  parseBacklog(published);
  return { source: published, entry, changed: changedFields(entry, refreshed) };
}
