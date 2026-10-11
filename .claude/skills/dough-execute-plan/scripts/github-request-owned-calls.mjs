// The `gh` processes one GitHub request owner (`./github-request-owner.mjs`)
// runs, as its replacement finds them: one `call.GENERATION.PID.json` in its
// endpoint directory (`./github-request-owner-endpoint.mjs`) for each, from
// its start until it is gone, naming the process, its exact arguments and
// when it started. An owner that is killed leaves its `gh` processes
// running at GitHub and their records standing; the owner that replaces it
// ends each of them before it asks GitHub anything, and counts one it can
// neither end nor tell from another process as one of its own turns until
// that process is gone. A recorded process ID alone proves nothing: only a
// process that runs those arguments and started when the record says is
// signalled, and it is looked at again before it is killed outright.
import { execFile } from "node:child_process";
import {
  readdirSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

// A record, or the draft of one its owner was ended while writing.
const callName = /^call\.(\d+)\.\d+\.json(\.draft)?$/;

// How long a signalled process may take to go, and how far a process's
// start as `ps` shows it may lie from the start its record names.
const endLimitMs = 1_000;
const startSlackMs = 2_000;

// The `gh` processes of the owner of `generation` in `directory`, recorded
// while they run.
export class OwnedCalls {
  #directory;

  #generation;

  /**
   * @param {string} directory
   * @param {number} generation
   */
  constructor(directory, generation) {
    this.#directory = directory;
    this.#generation = generation;
  }

  // Records the `gh` process `pid` just started with `args` against
  // `resource`'s allowance, whole for whoever reads it; answers what removes
  // the record once the process is gone. Throws when it cannot be recorded.
  /**
   * @param {number} pid
   * @param {readonly string[]} args
   * @param {string} resource
   * @returns {() => void}
   */
  started(pid, args, resource) {
    const file = join(this.#directory, `call.${this.#generation}.${pid}.json`);
    const record = { pid, args, resource, startedAtMs: Date.now() };
    writeFileSync(`${file}.draft`, JSON.stringify(record), { mode: 0o600 });
    renameSync(`${file}.draft`, file);
    return () => rmSync(file, { force: true });
  }
}

function seconds(elapsed) {
  const [, days = 0, hours = 0, minutes, rest] =
    /^(?:(\d+)-)?(?:(\d+):)?(\d+):(\d+)$/.exec(elapsed) ?? [];
  return (
    ((Number(days) * 24 + Number(hours)) * 60 + Number(minutes)) * 60 +
    Number(rest)
  );
}

// What the process table shows of `pid`: its state, the earliest and latest
// time it can have started -- the table tells elapsed time as a difference of
// whole seconds, a second off either way -- and its command line; undefined
// when no such process runs. Rejects when the table cannot be read.
/**
 * @param {number} pid
 * @returns {Promise<{ readonly state: string; readonly startedFromMs: number; readonly startedByMs: number; readonly command: string } | undefined>}
 */
export function shownProcess(pid) {
  return new Promise((resolve, reject) => {
    const fromMs = Date.now();
    execFile(
      "ps",
      [
        "-ww",
        "-p",
        String(pid),
        "-o",
        "stat=",
        "-o",
        "etime=",
        "-o",
        "command=",
      ],
      { encoding: "utf8" },
      (error, stdout) => {
        const [, state, elapsed, command] =
          /^\s*(\S+)\s+(\S+)\s+(.*\S)\s*$/.exec(stdout) ?? [];
        if (state !== undefined && !Number.isNaN(seconds(elapsed)))
          resolve({
            state,
            startedFromMs: fromMs - (seconds(elapsed) + 1) * 1000,
            startedByMs: Date.now() - (seconds(elapsed) - 1) * 1000,
            command,
          });
        else if (error === null || error.code === 1) resolve(undefined);
        else reject(error);
      },
    );
  });
}

function exists(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    // Another user's process is not one this owner's predecessor ran.
    return error.code === "EPERM" ? undefined : false;
  }
}

// Whether the process now running as a recorded call's `pid` is that call:
// "gone" when none runs or it has exited, "another" when it is some other
// process, and "unknown" when the process table cannot say.
async function recognized({ pid, args, startedAtMs }, shown) {
  const found = exists(pid);
  if (found !== true) return found === false ? "gone" : "another";
  let seen;
  try {
    seen = await shown(pid);
  } catch {
    return "unknown";
  }
  // An exited process stays as a zombie until reaped, or, on macOS, by its
  // bare name in parentheses while it exits.
  if (
    seen === undefined ||
    seen.state.startsWith("Z") ||
    /^\(.+\)$/.test(seen.command)
  )
    return "gone";
  const asked = `gh ${args.join(" ")}`;
  const { command, startedFromMs, startedByMs } = seen;
  return (command === asked || command.endsWith(`/${asked}`)) &&
    startedAtMs >= startedFromMs - startSlackMs &&
    startedAtMs <= startedByMs + startSlackMs
    ? "same"
    : "another";
}

async function goneWithin(pid, limitMs, everyMs = 20) {
  const lastAt = Date.now() + limitMs;
  while (exists(pid) === true && Date.now() < lastAt) await pause(everyMs);
  return exists(pid) !== true;
}

// Ends the recorded call if it still runs: asked to end, then -- still that
// very process a second later -- killed. Whether it is known to be gone or
// another's; false when it could not be told apart or outlived the kill.
async function ended(call, shown) {
  for (const signal of ["SIGTERM", "SIGKILL"]) {
    const which = await recognized(call, shown);
    if (which !== "same") return which !== "unknown";
    try {
      process.kill(call.pid, signal);
    } catch {
      // Gone since it was looked at.
    }
    if (await goneWithin(call.pid, endLimitMs)) return true;
  }
  return (await recognized(call, shown)) === "gone";
}

function recordedCall(file) {
  try {
    const { pid, args, resource, startedAtMs } = JSON.parse(
      readFileSync(file, "utf8"),
    );
    if (
      Number.isSafeInteger(pid) &&
      pid > 0 &&
      Array.isArray(args) &&
      typeof resource === "string" &&
      Number.isFinite(startedAtMs)
    )
      return { pid, args, resource, startedAtMs };
  } catch {
    // Not a record an owner wrote whole.
  }
  return undefined;
}

// Ends every `gh` call that owners before `generation` left running in
// `directory`, and removes the record of each one gone, never run or now
// another process. Answers, for each call that could not be ended or told
// apart, the resource it counts against and `gone`, which resolves once its
// process ID names no process and its record is removed; its record stands
// until then, for an owner that replaces this one meanwhile.
/**
 * @param {string} directory
 * @param {number} generation
 * @returns {Promise<{ readonly resource: string; readonly gone: Promise<void> }[]>}
 */
export async function earlierCallsEnded(
  directory,
  generation,
  shown = shownProcess,
) {
  const left = readdirSync(directory).filter(
    (name) => Number(callName.exec(name)?.[1] ?? generation) < generation,
  );
  const counted = [];
  await Promise.all(
    left.map(async (name) => {
      const file = join(directory, name);
      const call = recordedCall(file);
      if (call === undefined || (await ended(call, shown))) {
        rmSync(file, { force: true });
        return;
      }
      const gone = goneWithin(call.pid, Infinity, 250).then(() =>
        rmSync(file, { force: true }),
      );
      counted.push({ resource: call.resource, gone });
    }),
  );
  return counted;
}
