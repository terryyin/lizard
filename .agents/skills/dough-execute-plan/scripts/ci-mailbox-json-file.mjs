import { renameSync, writeFileSync } from "node:fs";
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
