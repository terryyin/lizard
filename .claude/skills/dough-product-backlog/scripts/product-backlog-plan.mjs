// What an entry's active plan link says: which plan file it names, whether it
// names the same document as another link, and whether it names a document
// another entry lists as its home.
// Claiming work, recording preparation, refreshing a reference and the
// backlog's listing checks all settle those questions here rather than each
// spelling out what a plan path means. No filesystem or Node-only imports, so
// the pure backlog document model can share them.
//
// This owns no judgment about a plan's content: a link this accepts can still
// be refused by an operation that needs more of the document than its path.

import { splitHref } from "./product-backlog-identity.mjs";

// How the established backlog spells an active plan link.
export const planLabel = "plan";

// The plan file a link names. A `#fragment` is navigation inside that plan,
// never part of which plan the link is.
export function planFileOf(target) {
  return splitHref(target).path;
}

// Whether two links name the same document, whichever section either points
// into. The one comparison behind two questions: a recorded plan link already
// links a declared plan of that document, and a declared plan that is the
// entry's own canonical home is its own plan, taking no link that would name
// one document twice.
export function sameDocument(link, other) {
  return planFileOf(link) === planFileOf(other);
}

// Whether a plan link names another entry's canonical home: when its file
// equals that home as written, or the link equals that home exactly. A
// section is otherwise navigation; the other home is compared as written,
// since its anchor names which story in a shared seed it is.
export function planNamesHome(target, href) {
  return (
    target !== undefined && (planFileOf(target) === href || target === href)
  );
}
