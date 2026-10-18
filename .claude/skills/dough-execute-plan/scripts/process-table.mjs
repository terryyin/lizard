// Process-table facts only. The request-call and mailbox owners decide which
// identity they trust and whether they may signal it; this module does neither.
import { execFile, execFileSync } from "node:child_process";

function argumentsFor(pid, includeElapsed = false) {
  return [
    "-ww",
    "-p",
    String(pid),
    "-o",
    "stat=",
    ...(includeElapsed ? ["-o", "etime="] : []),
    "-o",
    "command=",
  ];
}

function seconds(elapsed) {
  const [, days = 0, hours = 0, minutes, rest] =
    /^(?:(\d+)-)?(?:(\d+):)?(\d+):(\d+)$/.exec(elapsed) ?? [];
  return (
    ((Number(days) * 24 + Number(hours)) * 60 + Number(minutes)) * 60 +
    Number(rest)
  );
}

function facts(stdout, fromMs) {
  const [, state, elapsed, command] =
    /^\s*(\S+)\s+(\S+)\s+(.*\S)\s*$/.exec(stdout) ?? [];
  const elapsedSeconds = seconds(elapsed);
  if (state === undefined || Number.isNaN(elapsedSeconds)) return undefined;
  // ps reports elapsed time in whole seconds; allow a second either way and
  // include the time spent reading. Call owners retain their own start slack.
  return {
    state,
    startedFromMs: fromMs - (elapsedSeconds + 1) * 1000,
    startedByMs: Date.now() - (elapsedSeconds - 1) * 1000,
    command,
  };
}

// Nothing for a vanished process. Inspection failures remain errors so each
// owner can apply its own uncertainty/permission policy.
/**
 * @param {number} pid
 * @returns {Promise<{ readonly state: string; readonly startedFromMs: number; readonly startedByMs: number; readonly command: string } | undefined>}
 */
export function shownProcess(pid) {
  return new Promise((resolve, reject) => {
    const fromMs = Date.now();
    execFile(
      "ps",
      argumentsFor(pid, true),
      { encoding: "utf8" },
      (error, stdout) => {
        const seen = facts(stdout, fromMs);
        if (seen !== undefined) resolve(seen);
        else if (error === null || error.code === 1) resolve(undefined);
        else reject(error);
      },
    );
  });
}

export function shownProcessSync(pid) {
  try {
    // Mailbox identity needs state and command, never a call's start window.
    const stdout = execFileSync("ps", argumentsFor(pid), { encoding: "utf8" });
    const [, state, command] = /^\s*(\S+)\s*([\s\S]*)$/.exec(stdout) ?? [];
    return state === undefined ? undefined : { state, command: command.trim() };
  } catch (error) {
    if (error.status === 1) return undefined;
    throw error;
  }
}

// Linux can show a defunct main thread before a multithreaded process becomes
// a zombie. macOS can retain only its bare name in parentheses while it exits.
export function commandShowsExit(command) {
  return (
    command === undefined ||
    command.endsWith("<defunct>") ||
    /^\(.+\)$/.test(command)
  );
}

export function processShowsExit(seen) {
  return (
    seen === undefined ||
    seen.state.startsWith("Z") ||
    commandShowsExit(seen.command)
  );
}
