// Which session record files beside the backlog a record commit removes: only
// readable records at their own UUID's path whose close time is readable and
// outside the shared retention window at `now`. Anything else in the directory
// stays for a person to inspect. Records are read from the committed snapshot
// the record commit builds on, so a removal a failed attempt already staged is
// still selected when the commit is retried.
import { git } from "./publication-git.mjs";
import {
  isCurrentSessionRecord,
  isSessionId,
  sessionRecordDirectoryBeside,
  sessionRecordPathBeside,
} from "./session-record.mjs";

async function readableRecord(workspace, backlogPath, path) {
  try {
    const record = JSON.parse(
      (await git(workspace, "cat-file", "blob", `HEAD:./${path}`)).stdout,
    );
    const atOwnPath =
      isSessionId(record?.sessionId) &&
      sessionRecordPathBeside(backlogPath, record.sessionId) === path;
    const closed =
      typeof record.closedAt === "string" &&
      Number.isFinite(Date.parse(record.closedAt));
    return atOwnPath && closed ? record : undefined;
  } catch {
    return undefined;
  }
}

// The workspace-relative paths of expired session records at `now` (a Date).
export async function expiredSessionRecordPaths(workspace, backlogPath, now) {
  const committed = (
    await git(
      workspace,
      "ls-tree",
      "-r",
      "-z",
      "--name-only",
      "HEAD",
      "--",
      `${sessionRecordDirectoryBeside(backlogPath)}/`,
    )
  ).stdout
    .split("\0")
    .filter(Boolean);
  const expired = [];
  for (const path of committed) {
    const record = await readableRecord(workspace, backlogPath, path);
    if (record && !isCurrentSessionRecord(record, now)) expired.push(path);
  }
  return expired;
}
