import { randomUUID } from "node:crypto";
import { linkSync, renameSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";

// Atomic JSON publication shared by every mailbox record writer (events,
// delivery progress, worker identity, terminal results, revision coverage):
// write to a temp file, then rename into place so readers never observe a
// partially written record.
export function publishJson(directory, name, value) {
  const temporary = join(directory, `${name}.tmp`);
  writeFileSync(temporary, JSON.stringify(value), { mode: 0o600 });
  renameSync(temporary, join(directory, name));
}

// Exclusive variant for a claim that concurrent writers race to create:
// the complete record appears only by an exclusive link, so readers never
// observe it partially written. Returns whether this writer won the claim.
export function publishJsonExclusively(directory, name, value) {
  const draft = join(directory, `${name}.${randomUUID()}.draft`);
  writeFileSync(draft, JSON.stringify(value), { mode: 0o600 });
  try {
    linkSync(draft, join(directory, name));
    return true;
  } catch (error) {
    if (error.code === "EEXIST") return false;
    throw error;
  } finally {
    rmSync(draft, { force: true });
  }
}
