// Keeps a slice's active time for the checkout it runs in, so no agent counts
// with its own clock. Active time is the wall time while the slice runs minus
// the time inside commands run through the clock, overlaps excluded once.
// Each command prints one JSON result line. A report of a slice at or past
// the limit carries `overrun: true` and a message; `storyOverruns` counts the
// story's overruns, each counted once when first seen. A diagnosis runs in
// rounds the same way, one slice or round at a time; its reports add
// `diagnosis`, `round`, `roundsAuthorized` and `cumulativeMinutes`, and a
// round at the limit reports `overrun: true` without a story overrun.
//
//   start --identity ID --slice KEY [--after-ended-session]
//       [--continues OLD-KEY | --split-from OLD-KEY]
//       start the slice, or resume it when paused; with --continues, slice
//       OLD-KEY of the story becomes KEY first and keeps its elapsed time;
//       with --split-from, KEY is a new slice split from overrun slice
//       OLD-KEY, whose clock is dropped first so its key can start at zero
//       with --after-ended-session, settle the running slice and its open
//       commands at the last clock command before starting from now
//   start --identity ID --diagnosis NAME [--rounds-authorized N]
//       [--after-ended-session]
//       start the diagnosis's next round, refused beyond its authorized
//       rounds (3 unless a human authorized N); a running round continues
//   pause                             pause the running slice or round
//   status [--identity ID (--slice KEY | --diagnosis NAME)]
//       report a slice or a diagnosis's latest round; the running one unless named
//   run --log FILE --exit-file FILE -- COMMAND [ARG]...
//       run COMMAND with its time excluded from the running slice or round, write its
//       output to FILE and, when it ends, its exit status to the exit file;
//       exits with that status
import { flagValues, isDirectCliEntry } from "./ci-direct-entry.mjs";
import { startRound } from "./slice-clock-diagnosis.mjs";
import {
  currentTime,
  recordPath,
  updateRecord,
} from "./slice-clock-record.mjs";
import { runThroughClock } from "./slice-clock-run.mjs";
import {
  continueSlice,
  dropOverrunSlice,
  ensureSlice,
  isRunning,
  labelOf,
  settleEndedSession,
  sliceOf,
  sliceReport,
} from "./slice-clock-slices.mjs";

const usage = `Usage: node slice-clock.mjs start --identity ID --slice KEY [--after-ended-session] [--continues OLD-KEY | --split-from OLD-KEY]
       node slice-clock.mjs start --identity ID --diagnosis NAME [--rounds-authorized N] [--after-ended-session]
       node slice-clock.mjs pause
       node slice-clock.mjs status [--identity ID (--slice KEY | --diagnosis NAME)]
       node slice-clock.mjs run --log FILE --exit-file FILE -- COMMAND [ARG]...`;

class UsageError extends Error {}

const noneRunning = "no slice is running in this checkout";

// The slice or diagnosis the flags name; undefined when they name none.
function namedSlice({ identity, slice, diagnosis }) {
  if ([identity, slice, diagnosis].every((value) => value === undefined))
    return undefined;
  if (
    identity === undefined ||
    (slice === undefined) === (diagnosis === undefined)
  )
    throw new UsageError("name --identity with one of --slice or --diagnosis");
  return diagnosis === undefined
    ? { identity, slice }
    : { identity, diagnosis };
}

// The human-authorized round total, when given.
function roundTotal(value, named) {
  if (value === undefined) return undefined;
  if (named.diagnosis === undefined)
    throw new UsageError("--rounds-authorized needs --diagnosis");
  if (!/^[1-9][0-9]*$/.test(value))
    throw new UsageError("--rounds-authorized needs a positive whole number");
  return Number(value);
}

function flags(argv, allowed) {
  let values;
  try {
    values = flagValues(argv);
  } catch (error) {
    throw new UsageError(error.message);
  }
  for (const key of Object.keys(values))
    if (!allowed.includes(key)) throw new UsageError(`unknown flag for ${key}`);
  return values;
}

function start(file, argv) {
  const afterEndedSession = argv.includes("--after-ended-session");
  const { continues, splitFrom, roundsAuthorized, ...name } = flags(
    argv.filter((arg) => arg !== "--after-ended-session"),
    [
      "identity",
      "slice",
      "diagnosis",
      "continues",
      "splitFrom",
      "roundsAuthorized",
    ],
  );
  if (continues !== undefined && splitFrom !== undefined)
    throw new UsageError("name --continues or --split-from, not both");
  const named = namedSlice(name);
  if (named === undefined)
    throw new UsageError("start needs --identity and --slice or --diagnosis");
  if (named.diagnosis !== undefined && (continues ?? splitFrom) !== undefined)
    throw new UsageError("--continues and --split-from name slices only");
  const authorized = roundTotal(roundsAuthorized, named);
  return updateRecord(file, (record) => {
    const now = currentTime();
    if (afterEndedSession) settleEndedSession(record);
    const { running } = record;
    const same = isRunning(record, named);
    if (running && !same)
      throw new Error(
        `${labelOf(running)} is running in this checkout; pause it first`,
      );
    if (continues !== undefined) continueSlice(record, named, continues);
    if (splitFrom !== undefined)
      dropOverrunSlice(record, named.identity, splitFrom);
    if (!same) {
      if (named.diagnosis !== undefined)
        startRound(record, named, authorized, now);
      else
        ensureSlice(record, named).periods.push({
          startedAt: now,
          endedAt: null,
        });
      Object.assign(record, { running: named });
    }
    Object.assign(record, { lastClockCommandAt: now });
    return { command: "start", ...sliceReport(record, named, now) };
  });
}

function pause(file, argv) {
  flags(argv, []);
  return updateRecord(file, (record) => {
    const named = record.running;
    if (!named) throw new Error(noneRunning);
    const now = currentTime();
    sliceOf(record, named).periods.at(-1).endedAt = now;
    Object.assign(record, { running: null });
    Object.assign(record, { lastClockCommandAt: now });
    return { command: "pause", ...sliceReport(record, named, now) };
  });
}

function status(file, argv) {
  const asked = namedSlice(flags(argv, ["identity", "slice", "diagnosis"]));
  return updateRecord(file, (record) => {
    const named = asked ?? record.running;
    if (!named) throw new Error(noneRunning);
    if (sliceOf(record, named) === undefined)
      throw new Error(`no record of ${labelOf(named)}`);
    const now = currentTime();
    Object.assign(record, { lastClockCommandAt: now });
    return { command: "status", ...sliceReport(record, named, now) };
  });
}

function run(file, argv) {
  const separator = argv.indexOf("--");
  const [command, ...args] = separator < 0 ? [] : argv.slice(separator + 1);
  const { log, exitFile } = flags(
    separator < 0 ? argv : argv.slice(0, separator),
    ["log", "exitFile"],
  );
  if (command === undefined || log === undefined || exitFile === undefined)
    throw new UsageError("run needs --log, --exit-file and a command after --");
  return runThroughClock(file, { command, args, log, exitFile });
}

const commands = { start, pause, status, run };

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  const [name, ...argv] = process.argv.slice(2);
  try {
    if (!Object.hasOwn(commands, name))
      throw new UsageError(`unknown command ${name ?? ""}`.trim());
    const result = await commands[name](recordPath(process.cwd()), argv);
    process.stdout.write(`${JSON.stringify(result)}\n`);
    process.exitCode = result.exitStatus ?? 0;
  } catch (error) {
    const usageError = error instanceof UsageError;
    process.stderr.write(`${error.message}\n${usageError ? `${usage}\n` : ""}`);
    process.exitCode = usageError ? 2 : 1;
  }
}
