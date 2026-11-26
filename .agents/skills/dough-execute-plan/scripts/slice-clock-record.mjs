// The slice clock's record: local execution state of one checkout, kept in
// its Git directory so every process in that worktree reads the same record
// and nothing of it is committed.
//
// {
//   schemaVersion: 1,
//   lastClockCommandAt,   latest start, pause, status, or run start/end time
//   running: { identity, slice } | { identity, diagnosis } | null,
//            one slice or diagnosis round runs at a time
//   stories: {
//     [identity]: {
//       slices: {
//         [slice]: {
//           periods:  [{ startedAt, endedAt | null }],      started .. paused
//           commands: [{ id, startedAt, endedAt | null }],  run through the clock
//           overrunDetectedAt,   set once, when the slice is first seen at the limit
//           overrunReportedTo: [agent],   agents a host hook told of the overrun
//         },
//       },
//       overruns: [{ slice, detectedAt }],   one per overrun; kept when slices split
//       diagnoses: {
//         [name]: {
//           roundsAuthorized,   3 unless a human authorized more; never lowered
//           rounds: [{ periods, commands, overrunDetectedAt, overrunReportedTo }],
//                   one per start, as slices keep them; never reset
//         },
//       },
//     },
//   },
// }
//
// Times are epoch milliseconds. An open period or command lasts until now.
// A new session can settle them at lastClockCommandAt. Host hooks do not
// move that timestamp.
// `slice-clock-slices.mjs` holds what the entries mean: a slice and its
// overrun; `slice-clock-active-time.mjs` holds active time and
// `slice-clock-diagnosis.mjs` the rounds' rules.
import { execFileSync } from "node:child_process";
import {
  mkdirSync,
  readFileSync,
  renameSync,
  rmdirSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";

export const timeFileSetting = "DOUGH_SLICE_CLOCK_TIME_FILE";

const lockRetryMs = 10;
const lockStaleMs = 10_000;

// The clock's current time. The setting names a file holding an ISO time,
// read at every observation, so a spec moves time between and during clock
// commands without waiting.
export function currentTime(env = process.env) {
  const file = env[timeFileSetting];
  if (file === undefined) return Date.now();
  const supplied = readFileSync(file, "utf8").trim();
  const time = Date.parse(supplied);
  if (Number.isNaN(time))
    throw new Error(`${timeFileSetting} must hold an ISO time: ${supplied}`);
  return time;
}

// The record file of the worktree `checkout` is in.
export function recordPath(checkout) {
  const found = execFileSync(
    "git",
    ["rev-parse", "--git-path", "open-dough/slice-clock"],
    { cwd: checkout, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] },
  );
  return resolve(checkout, found.trim());
}

function emptyRecord() {
  return { schemaVersion: 1, running: null, stories: {} };
}

export function readRecord(file) {
  try {
    return JSON.parse(readFileSync(file, "utf8"));
  } catch (error) {
    if (error.code === "ENOENT") return emptyRecord();
    throw error;
  }
}

function wait(ms) {
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);
}

// Holds the record's lock directory; a lock left by a dead process is taken
// over once it is stale.
function locked(file, work) {
  const lock = `${file}.lock`;
  mkdirSync(dirname(file), { recursive: true });
  for (;;) {
    try {
      mkdirSync(lock);
      break;
    } catch (error) {
      if (error.code !== "EEXIST") throw error;
      try {
        if (Date.now() - statSync(lock).mtimeMs > lockStaleMs) rmdirSync(lock);
      } catch {
        // Released meanwhile; try again.
      }
      wait(lockRetryMs);
    }
  }
  try {
    return work();
  } finally {
    rmdirSync(lock);
  }
}

// Writes `file` whole, so a reader never sees it partly written.
export function writeWhole(file, text) {
  const draft = `${file}.${process.pid}.tmp`;
  writeFileSync(draft, text);
  renameSync(draft, file);
}

// Applies `change` to the record while no other clock process changes it,
// and returns what `change` returns.
export function updateRecord(file, change) {
  return locked(file, () => {
    const record = readRecord(file);
    const result = change(record);
    writeWhole(file, `${JSON.stringify(record)}\n`);
    return result;
  });
}
