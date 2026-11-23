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

// The workflow values a close record may carry: a standalone (Dashboard
// ad-hoc) session, including a story follow-up, a plan execution, or a story
// refinement's landed preparation.
export const standaloneSessionWorkflow = "ad-hoc";
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

// The file name (within the session-record directory) the record of `sessionId`
// takes. The same UUID always names the same file.
export function sessionRecordFileName(sessionId) {
  return `${sessionId}.json`;
}

// The record's path relative to the backlog's directory.
export function sessionRecordRelativePath(sessionId) {
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
