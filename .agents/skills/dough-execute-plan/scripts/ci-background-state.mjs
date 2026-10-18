import { existsSync, readFileSync, writeFileSync, renameSync } from "node:fs";
import { randomUUID } from "node:crypto";
import { join } from "node:path";
import { liveClaudeStillRunning } from "./ci-background-session.mjs";

export const handoffPath = (directory) => join(directory, "background.json");

export function readBackgroundHandoff(directory) {
  return existsSync(handoffPath(directory))
    ? JSON.parse(readFileSync(handoffPath(directory), "utf8"))
    : undefined;
}

export function saveBackgroundHandoff(directory, handoff) {
  const temporary = join(directory, `background-${randomUUID()}.tmp`);
  writeFileSync(temporary, JSON.stringify(handoff), { mode: 0o600 });
  renameSync(temporary, handoffPath(directory));
}

export function backgroundCancellationRequested(directory) {
  return (
    existsSync(join(directory, "stop")) ||
    existsSync(join(directory, "background-cancel"))
  );
}

export function backgroundSessionLimit(handoff) {
  if (!liveClaudeStillRunning(handoff.liveSession)) return "session_closed";
  if (Date.now() >= handoff.deadlineAt) return "expired";
}
