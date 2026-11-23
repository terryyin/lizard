// What a shared-checkout note is: the Dashboard's local note, beside a launch's
// landing context, that another session was open in the same checkout since
// `since`. Its file name, shape and the range mark it yields have one owner
// here. No filesystem, Git, or Node-only imports, so the publisher and the
// Dashboard share it. The note stays local; only the mark on a published range,
// owned by the record contract, is published.

import { sharedCheckoutUncertainty } from "./session-record.mjs";

// The note's file name, in the launch's reporting directory beside
// landing-context.json.
const sharedCheckoutNoteFileName = "shared-checkout.json";

// The note's path beside the landing context file at `landingContextPath`.
export function sharedCheckoutNotePathBeside(landingContextPath) {
  const slash = landingContextPath.lastIndexOf("/");
  return `${slash < 0 ? "" : landingContextPath.slice(0, slash + 1)}${sharedCheckoutNoteFileName}`;
}

// A note that exists but cannot be read or parsed, or names no valid `since`:
// overlap of unknown start, so every range is marked.
const sharedCheckoutOfUnknownStart = Object.freeze({ since: undefined });

// The overlap a note's text records: `{ since }` when it names a valid time,
// otherwise overlap of unknown start.
export function parseSharedCheckoutNote(text) {
  let since;
  try {
    since = JSON.parse(text)?.since;
  } catch {
    return sharedCheckoutOfUnknownStart;
  }
  return typeof since === "string" && !Number.isNaN(Date.parse(since))
    ? { since }
    : sharedCheckoutOfUnknownStart;
}

// The note a failed read of its file records: a missing file is no note; any
// other failure is a note that exists but cannot be read, of unknown start.
export function sharedCheckoutNoteOnReadFailure(error) {
  return error?.code === "ENOENT" ? undefined : sharedCheckoutOfUnknownStart;
}

// The mark for a range accepted at `acceptedAt` under `note`: shared-checkout
// when the overlap's start is unknown or at or before the acceptance,
// otherwise none.
export function rangeUncertainty(note, acceptedAt) {
  if (!note) return undefined;
  return note.since === undefined ||
    Date.parse(acceptedAt) >= Date.parse(note.since)
    ? sharedCheckoutUncertainty
    : undefined;
}

// The note's text for an overlap that began at `since` (an ISO time), keeping
// the earlier start of the `recorded` note, if any. A recorded note of unknown
// start yields `notedLaunchedAt`, the noted launch's own start: the earliest
// the overlap could have begun.
export function sharedCheckoutNoteText(since, recorded, notedLaunchedAt) {
  const recordedSince =
    recorded === sharedCheckoutOfUnknownStart
      ? notedLaunchedAt
      : recorded?.since;
  const earliest =
    recordedSince !== undefined &&
    Date.parse(recordedSince) <= Date.parse(since)
      ? recordedSince
      : since;
  return JSON.stringify({ since: earliest });
}
