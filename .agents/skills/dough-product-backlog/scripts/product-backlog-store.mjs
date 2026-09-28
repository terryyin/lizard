// Reads and replaces the files this tool edits, and applies one validated
// change under a cooperating lock. Cooperating script runs are serialized
// through a lock directory beside the target file and always re-read it
// inside the lock, so a concurrent run cannot lose the other's update. This
// coordinates script writers only; it cannot protect the file from an
// arbitrary external writer that ignores the lock, and it does not serialize
// across worktrees.

import {
  existsSync,
  mkdirSync,
  readFileSync,
  renameSync,
  rmdirSync,
  writeFileSync,
} from "node:fs";
import { setTimeout as delay } from "node:timers/promises";
import { BacklogError } from "./product-backlog-refusal.mjs";

// Where a project keeps its backlog unless a caller names another file.
export const defaultBacklogPath = ".planning/PRODUCT-BACKLOG.md";

const pollMilliseconds = 25;

function lockTimeout() {
  const configured = Number(process.env.DOUGH_BACKLOG_LOCK_TIMEOUT_MS);
  return Number.isFinite(configured) && configured > 0 ? configured : 10000;
}

async function acquire(lockPath) {
  const deadline = Date.now() + lockTimeout();
  for (;;) {
    try {
      mkdirSync(lockPath);
      return;
    } catch (error) {
      if (error.code !== "EEXIST") {
        throw error;
      }
      if (Date.now() >= deadline) {
        throw new BacklogError(
          `The file is locked by another run: ${lockPath}. Nothing was written. Wait for that run, or remove the lock by hand once you have established that no run holds it.`,
        );
      }
      await delay(pollMilliseconds);
    }
  }
}

// Replaces a whole document in one step, so an interrupted run leaves either
// the previous content or the new content and never a half-written file.
// Operations that also record identities in canonical homes reuse this.
export function replaceFile(path, contents) {
  const temporaryPath = `${path}.tmp-${process.pid}`;
  writeFileSync(temporaryPath, contents, "utf8");
  renameSync(temporaryPath, path);
}

// Reads a file this tool edits, refusing with `missing` rather than crashing
// when it is not there. The backlog and every canonical home come in here.
// A supplied path that names a directory is refused the same clean way,
// rather than surfacing Node's raw `EISDIR`: this is the one bounded
// filesystem-error case this tool covers, not general filesystem-error
// handling.
export function readFile(path, missing) {
  try {
    return readFileSync(path, "utf8");
  } catch (error) {
    if (error.code === "ENOENT") {
      throw new BacklogError(missing);
    }
    if (error.code === "EISDIR") {
      throw new BacklogError(
        `${path} is a directory, not a file. Supply the path to the file itself.`,
      );
    }
    throw error;
  }
}

// Reads any file this tool mutates, applies `change` to its current bytes, and
// replaces it atomically under a cooperating lock beside that path. A refused
// change leaves the file untouched. Callers supply the missing-file refusal so
// backlog and canonical-home wording stay accurate.
export async function applyToFile(path, change, missing) {
  // The lock is made beside the file, so a path that is not there at all
  // cannot be locked either. Establishing the file first keeps a mistyped path
  // an ordinary refusal instead of a failure to create its lock, and neither
  // the lock nor the directory a typo names is ever created. The read inside
  // the lock stays the authoritative one: it refuses the same way if the file
  // goes away while this run is waiting.
  if (!existsSync(path)) {
    throw new BacklogError(missing);
  }

  const lockPath = `${path}.lock`;
  await acquire(lockPath);
  try {
    const source = readFile(path, missing);
    replaceFile(path, change(source));
  } finally {
    rmdirSync(lockPath);
  }
}

// Reads the backlog, applies `change` to its current bytes, and replaces the
// file atomically. A refused change leaves the file untouched.
export async function applyToBacklog(path, change) {
  await applyToFile(path, change, `Backlog file not found: ${path}`);
}
