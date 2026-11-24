// What a published session close record is: the short published fact that one
// session closed with accepted Git comparisons, kept beside the backlog. The
// directory, file name, version, session-UUID rule, workflow values and range
// marks have one owner here. No filesystem, Git, or Node-only imports, so every
// reader of published session records shares it.

import { isWithinDoneWindow } from "../../dough-product-backlog/scripts/product-backlog-done-record.mjs";

// Session records live beside the backlog, one file per session UUID.
export const sessionRecordDirectory = "sessions";

// The record schema version every writer and reader shares.
export const sessionRecordVersion = 1;

// Workflow values used by the execution and preparation record writers.
export const executionSessionWorkflow = "dough-execute-plan";
export const refinementSessionWorkflow = "dough-story-refinement";

// The mark a published range carries when its comparison may include another
// session's work from the same checkout.
export const sharedCheckoutUncertainty = "shared-checkout";

// Whether a record closed at `closedAt` is still current at `now` (a Date):
// the done-record retention window, counted from the close, not the commit.
export function isCurrentSessionRecord({ closedAt }, now) {
  return isWithinDoneWindow(closedAt, now);
}

// Whether `value` is a session UUID as records spell it.
export function isSessionId(value) {
  return (
    typeof value === "string" &&
    /^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(value)
  );
}

const adHocStoryPrefix = "ad-hoc#";

export function adHocStoryIdentity(sessionId) {
  return isSessionId(sessionId) ? `${adHocStoryPrefix}${sessionId}` : undefined;
}

/** @param {string} identity */
export function adHocStorySessionId(identity) {
  if (!identity.startsWith(adHocStoryPrefix)) return undefined;
  const sessionId = identity.slice(adHocStoryPrefix.length);
  return isSessionId(sessionId) ? sessionId : undefined;
}

export function adHocCloseTitle(workflow, title) {
  return workflow === "ad-hoc" && typeof title === "string" && title.trim()
    ? title
    : undefined;
}

// The originating close carries the title and the identity derived from its
// own session. A follow-up carries its parent's identity.
export function isOriginatingAdHocClose(record) {
  if (adHocCloseTitle(record?.workflow, record?.title) === undefined)
    return false;
  const identity = adHocStoryIdentity(record.sessionId);
  return identity !== undefined && record.story === identity;
}

// A concurrent close of the same originating session keeps the first title,
// close time and comparison that reached the published repository.
export function matchingOriginatingAdHocClose(bytes, record) {
  if (!isOriginatingAdHocClose(record)) return undefined;
  try {
    const accepted = JSON.parse(bytes);
    return accepted?.version === sessionRecordVersion &&
      accepted.sessionId === record.sessionId &&
      isOriginatingAdHocClose(accepted)
      ? accepted
      : undefined;
  } catch {
    return undefined;
  }
}

// The file name (within the session-record directory) the record of `sessionId`
// takes. The same UUID always names the same file.
function sessionRecordFileName(sessionId) {
  return `${sessionId}.json`;
}

// The record's path relative to the backlog's directory.
function sessionRecordRelativePath(sessionId) {
  return `${sessionRecordDirectory}/${sessionRecordFileName(sessionId)}`;
}

// Keep the producer's POSIX path normalization without requiring Node.
function pathBesideBacklog(backlogPath, relativePath) {
  const absolute = backlogPath.startsWith("/");
  const directory = backlogPath.replace(/\/+$/, "").split("/").slice(0, -1);
  const parts = [];
  for (const part of directory) {
    if (part === "" || part === ".") continue;
    if (part !== "..") parts.push(part);
    else if (parts.length > 0 && parts.at(-1) !== "..") parts.pop();
    else if (!absolute) parts.push(part);
  }
  return `${absolute ? "/" : ""}${[...parts, relativePath].join("/")}`;
}

export function sessionRecordDirectoryBeside(backlogPath) {
  return pathBesideBacklog(backlogPath, sessionRecordDirectory);
}

export function sessionRecordPathBeside(backlogPath, sessionId) {
  return pathBesideBacklog(backlogPath, sessionRecordRelativePath(sessionId));
}
