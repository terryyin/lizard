// Places exactly one explicitly identified work item at an explicit
// destination in "## Backlog list", leaving every other entry, both lists, the
// direction, and the text around them as they were.
//
// The caller has already decided the priority; this applies that one decision
// and nothing else. It ranks nothing itself: no dependency order, no direction
// alignment, no ordering inferred from identities, and no tie-break between
// entries whose relative order the caller did not state. The entry's own line
// is carried across unchanged, so its title, canonical link, identity, and any
// recorded plan link mean exactly what they meant before the move; changing
// what an entry says is a separate decision.

import {
  parseBacklog,
  queueHeading,
  renderBacklog,
  takenHeading,
} from "./product-backlog-document.mjs";
import {
  findEntry,
  insertEntryLine,
  queueIndexFor,
  removeEntryLine,
} from "./product-backlog-placement.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

// What a caller can do when the named identity is in neither list. This
// operation moves work the backlog already carries and never writes an entry
// the backlog does not hold.
const absent =
  `Nothing was moved. Either this is not the identity the backlog carries, ` +
  `or the work is not listed at all: read the two lists and name the entry ` +
  `to place, or add it.`;

// Returning taken work to the queue is a decision the caller states, not one
// that naming a destination makes for them. Requiring it both ways also makes
// --return a precondition on where the entry actually is, so a request written
// against a stale reading of the backlog is refused instead of applied to
// whichever list happens to hold the work now.
function requireStatedReturn(entry, returning) {
  if (entry.list === takenHeading && !returning) {
    throw new BacklogError(
      `"${entry.identity}" is in "## ${takenHeading}". Returning taken work ` +
        `to "## ${queueHeading}" is an explicit backlog decision, and naming ` +
        `a destination does not imply it: supply --return to return it, or ` +
        `leave it taken.`,
    );
  }
  if (entry.list === queueHeading && returning) {
    throw new BacklogError(
      `"${entry.identity}" is already in "## ${queueHeading}", not ` +
        `"## ${takenHeading}", so there is nothing to return. Reprioritize ` +
        `it without --return.`,
    );
  }
}

// An entry placed relative to itself names no destination at all, and it would
// otherwise read as a missing anchor once the entry is lifted out.
function requireOtherAnchor(entry, placement) {
  const wanted = placement.after ?? placement.before;
  if (wanted === entry.identity) {
    throw new BacklogError(
      `Anchor identity "${wanted}" is the entry being placed, so the ` +
        `request names no destination. Place it relative to another entry, ` +
        `or with --position first or --position last.`,
    );
  }
}

// Applies the placement to one backlog document and returns the backlog to
// publish alongside the entry that moved and whether it was returned from
// "## Taken".
export function placeEntry(source, request) {
  const document = parseBacklog(source);
  const entry = findEntry(document, request.identity, absent);
  requireStatedReturn(entry, request.returning);
  requireOtherAnchor(entry, request);

  const line = document.lines[entry.index];
  removeEntryLine(document, entry.index);
  // The destination is resolved against the backlog without this entry in it,
  // which is the same question adding an entry asks. Reading the document back
  // rather than adjusting indexes is what keeps one relative-placement rule:
  // no operation has to know how many lines a removal shifted.
  const remaining = parseBacklog(renderBacklog(document));
  insertEntryLine(remaining, queueIndexFor(remaining, request), line);

  return {
    source: renderBacklog(remaining),
    entry,
    returned: entry.list === takenHeading,
  };
}
