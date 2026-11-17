// Runs one command through the slice clock: its time is excluded from the
// slice or diagnosis round running when it starts, its output goes to a log,
// and its exit status goes to an exit file once the run is recorded.
import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import { closeSync, openSync, writeFileSync } from "node:fs";
import { constants } from "node:os";
import { resolve } from "node:path";
import {
  currentTime,
  updateRecord,
  writeWhole,
} from "./slice-clock-record.mjs";
import { pinnedRound } from "./slice-clock-diagnosis.mjs";
import { sliceOf, sliceReport } from "./slice-clock-slices.mjs";

// The command's exit status; a command ended by a signal reports 128 plus
// the signal's number, as a shell does.
function runToEnd(command, args, log) {
  const output = openSync(log, "w");
  return new Promise((done) => {
    const child = spawn(command, args, {
      stdio: ["ignore", output, output],
    });
    child.on("error", (error) => {
      writeFileSync(output, `${error.message}\n`);
      done(127);
    });
    child.on("exit", (code, signal) =>
      done(code ?? 128 + (constants.signals[signal] ?? 0)),
    );
  }).finally(() => closeSync(output));
}

// The run's report; the caller has checked its flags.
export async function runThroughClock(file, { command, args, log, exitFile }) {
  const id = randomUUID();
  // The slice or round running when the command starts owns its interval,
  // even when it is paused before the command ends.
  const named = updateRecord(file, (record) => {
    const now = currentTime();
    Object.assign(record, { lastClockCommandAt: now });
    if (!record.running) return null;
    sliceOf(record, record.running).commands.push({
      id,
      startedAt: now,
      endedAt: null,
    });
    return pinnedRound(record, record.running);
  });
  const exitStatus = await runToEnd(command, args, log);
  const report = updateRecord(file, (record) => {
    const now = currentTime();
    Object.assign(record, { lastClockCommandAt: now });
    if (!named) return { slice: null };
    const command = sliceOf(record, named).commands.find(
      (entry) => entry.id === id,
    );
    command.endedAt ??= now;
    return sliceReport(record, named, now);
  });
  // Written last, so whoever waits on it reads a finished run.
  writeWhole(exitFile, `${exitStatus}\n`);
  return {
    command: "run",
    exitStatus,
    log: resolve(log),
    exitFile: resolve(exitFile),
    ...report,
  };
}
