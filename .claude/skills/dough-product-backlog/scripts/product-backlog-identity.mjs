// What a work item's identity is. An identity is recorded once and then kept:
// reading one off a written entry, composing one for a new entry, and spelling
// one back into an entry are all the same question asked from different sides.
// They are asked here, so the spelling has one owner.
//
// A recorded identity is independent of the link beside it. The link is
// navigation — where the canonical home is right now — and moving or renaming
// that home never re-identifies the work. That is why the entry records the
// identity in full rather than leaving it to be reconstructed from the link.

import { BacklogError } from "./product-backlog-refusal.mjs";

export const adoptionHint =
  "Identities are never allocated here; take one from the work item's " +
  "canonical home or from the separate identity-adoption operation.";

export function ambiguousHome(reason) {
  return new BacklogError(
    `Ambiguous canonical home: ${reason} ${adoptionHint}`,
  );
}

// A backlog link names a canonical home and, when that home holds more than
// one story, the anchor of the story inside it. Everything that reads or
// records an identity takes a link apart here.
export function splitHref(href) {
  const marker = href.indexOf("#");
  return marker === -1
    ? { path: href, anchor: "" }
    : { path: href.slice(0, marker), anchor: href.slice(marker + 1) };
}

// How an identity is spelled when it is first taken from a canonical home: the
// ID that home carries, narrowed by the story anchor when there is one. Only
// adoption composes an identity; everything afterwards carries the composed
// value about unchanged.
export function composeIdentity(token, anchor) {
  return anchor ? `${token}#${anchor}` : token;
}

// Whether a value recorded beside a link is the whole identity. One that
// names an anchor is read back as itself, whatever the link says; one that
// does not is the older shorthand, still read against the link's anchor. Every
// caller asks the question here, so the two spellings are told apart once.
export function recordsIdentityInFull(recorded) {
  return recorded.includes("#");
}

// The identity a written entry reads as. A recorded identity is written in
// full, so it reads back as itself however the link has since changed. An
// entry written before identities were recorded in full carries only the seed
// token, which is still read against the link's anchor so that such an entry
// names the same work item it always named. An entry that records nothing is
// still identified by the link it carries, as it was before adoption.
export function identityFor(href, recorded) {
  if (!recorded) {
    return href;
  }
  if (recordsIdentityInFull(recorded)) {
    return recorded;
  }
  return composeIdentity(recorded, splitHref(href).anchor);
}

// Whether an entry carries a name of its own for the work, rather than being
// identified by the link beside it. A link that spells the identity exactly
// leaves nothing to record, so such an entry names the work only as wherever
// its home currently is, and it gains a name of its own by adopting one. This
// is not `recordsIdentityInFull`, which asks how a recorded value is spelled:
// an identity taken from a bounded correction's plan path is a name of its own
// and carries no anchor at all.
export function recordsOwnIdentity(identity, href) {
  return identity !== href;
}

// What a written entry records for a given identity: the identity itself,
// unless the link already spells it exactly, in which case there is nothing
// to record beside it. This never asks the link to agree with the identity —
// a relocated home is the ordinary case, not a refusal.
export function recordedFor(identity, href) {
  if (!recordsOwnIdentity(identity, href)) {
    return "";
  }
  const { anchor } = splitHref(href);
  if (/[\s()[\]]/.test(identity)) {
    throw ambiguousHome(
      `identity "${identity}" cannot be written in an entry, which holds no ` +
        `whitespace, brackets, or parentheses.`,
    );
  }
  if (identity.startsWith("#")) {
    throw ambiguousHome(
      `identity "${identity}" names an anchor and no work item of its own.`,
    );
  }
  if (!recordsIdentityInFull(identity) && anchor !== "") {
    // An identity with no anchor, written beside a link that has one, would
    // read back as the anchored identity an older entry spells that way.
    throw ambiguousHome(
      `identity "${identity}" names no anchor but the link "${href}" names ` +
        `"${anchor}", so the entry could not be read back as "${identity}".`,
    );
  }
  return identity;
}
