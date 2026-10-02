import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";
import { fileURLToPath } from "node:url";
import {
  readWorkerIdentity,
  recordWorkerIdentity,
  recordLostTerminalResult,
  workerLossReason,
} from "./ci-mailbox-store.mjs";

export const mailboxWorkerPath = fileURLToPath(
  new URL("./ci-mailbox.mjs", import.meta.url),
);

// A sandbox such as macOS `sandbox-exec` refuses to run the setuid `ps`: the
// process table is unreadable, which says nothing about whether a PID runs.
function inspectionDenied(error) {
  return error.code === "EPERM" || error.code === "EACCES";
}

function workerIsRunning(pid) {
  try {
    process.kill(pid, 0);
  } catch (error) {
    if (error.code === "ESRCH") return false;
    throw error;
  }
  try {
    // Linux may leave an exited detached worker as a zombie until its parent
    // reaps it. kill(pid, 0) still succeeds for that PID, but it cannot run.
    const state = execFileSync("ps", ["-p", String(pid), "-o", "stat="], {
      encoding: "utf8",
    }).trim();
    return state !== "" && !state.startsWith("Z");
  } catch (error) {
    if (error.status === 1) return false;
    // kill(pid, 0) found the PID; without its state it counts as running.
    if (inspectionDenied(error)) return true;
    throw error;
  }
}

async function waitForWorkerExit(pid, timeoutMs = 1_000) {
  const deadline = Date.now() + timeoutMs;
  while (workerIsRunning(pid) && Date.now() < deadline) await pause(20);
  return !workerIsRunning(pid);
}

function streamWorkerTitle(directory) {
  return `dough-ci:${createHash("sha256").update(directory).digest("hex")}`;
}

// A foreground stream has no mailbox argument in its original command.
// Bind its observable identity for the complete stream lifetime.
export async function withStreamWorkerIdentity(directory, run) {
  const originalTitle = process.title;
  try {
    process.title = streamWorkerTitle(directory);
    const identity = { pid: process.pid, mode: "stream" };
    if (!streamTitleVisible(directory))
      throw new Error("CI stream worker identity could not be verified");
    recordWorkerIdentity(directory, identity);
    return await run();
  } finally {
    process.title = originalTitle;
  }
}

// This process set its own title; a denied read cannot contradict it.
function streamTitleVisible(directory) {
  try {
    return readProcessCommandSync(process.pid) === streamWorkerTitle(directory);
  } catch (error) {
    if (inspectionDenied(error)) return true;
    throw error;
  }
}

function expectedWorkerCommand(directory, { mode } = {}) {
  if (mode === "stream") return streamWorkerTitle(directory);
  if (mode !== undefined) return undefined;
  return `${process.execPath} ${mailboxWorkerPath} worker ${directory}`;
}

function commandMatchesMailboxWorker(command, directory, identity) {
  if (command === expectedWorkerCommand(directory, identity)) return true;
  if (identity?.mode !== undefined) return false;
  if (typeof command !== "string") return false;
  // Accept any ci-mailbox worker bound to this exact mailbox directory. Hosts
  // may resolve a different installed skill copy than the one that started the
  // worker; the mailbox path is the identity that must not be confused.
  return (
    /(^|[/ ])ci-mailbox\.mjs worker /.test(command) &&
    command.endsWith(` worker ${directory}`)
  );
}

function readProcessCommandSync(pid) {
  try {
    return execFileSync("ps", ["-ww", "-p", String(pid), "-o", "command="], {
      encoding: "utf8",
    }).trim();
  } catch (error) {
    if (error.status === 1) return undefined;
    throw error;
  }
}

// An exited process is gone whatever its state: `ps` shows it as gone or
// `<defunct>`, including, on Linux, a node process whose main thread exited
// while other threads unwind and whose state is not yet a zombie's. macOS
// shows an exiting process, whose arguments are already gone, by its bare
// name in parentheses, such as `(node)`, before it becomes a zombie.
function commandShowsExit(command) {
  return (
    command === undefined ||
    command.endsWith("<defunct>") ||
    /^\(.+\)$/.test(command)
  );
}

// One classification of a live PID's command serves liveness and
// termination. Any command other than this mailbox's worker is a different
// process only if it still runs: a worker that exits during the read can show
// a transient command, such as `[node]`.
function classifyWorkerCommand(command, identity, directory) {
  if (commandShowsExit(command)) return "dead";
  if (commandMatchesMailboxWorker(command, directory, identity)) return "alive";
  return workerIsRunning(identity.pid) ? "unknown" : "dead";
}

// Only a verified mailbox worker may be signaled. When inspection is denied
// the worker stays unverified, and its caller keeps its lost-coverage path.
function verifyMailboxWorker(identity, directory, readCommand) {
  let command;
  try {
    command = readCommand(identity.pid);
  } catch (error) {
    if (inspectionDenied(error)) return false;
    throw error;
  }
  const state = classifyWorkerCommand(command, identity, directory);
  if (state === "unknown")
    throw new Error(
      `CI observer worker ${identity.pid} does not match this mailbox`,
    );
  return state === "alive";
}

// Read-only liveness check reusing the same identity rule as termination,
// without ever signaling a process. A PID that exists but runs a different
// command is reused/mismatched identity: this reports "unknown" rather than
// "alive" or "dead" so callers neither reassure a coordinator nor act on an
// unrelated process.
export function checkMailboxWorkerLiveness(
  identity = {},
  directory,
  { readCommand = readProcessCommandSync } = {},
) {
  const { pid } = identity;
  if (!(Number.isSafeInteger(pid) && pid > 0)) return "unknown";
  if (!workerIsRunning(pid)) return "dead";
  let command;
  try {
    command = readCommand(pid);
  } catch (error) {
    // Permission-denied process inspection must not end a completion wait:
    // the PID is live; command matching remains required before any signal.
    if (inspectionDenied(error)) return "alive";
    throw error;
  }
  return classifyWorkerCommand(command, identity, directory);
}

// Read-only: reports an already-recorded loss, or newly detects one from the
// worker's own recorded identity, without ever signaling a process. Returns
// undefined for anything short of a confirmed death (alive, uncertain
// identity, or already-terminal for another reason such as a normal stop),
// so an intentional completed stop is never mislabeled as unexpected death.
export function mailboxWorkerLoss(directory) {
  const resultPath = join(directory, "result.json");
  if (existsSync(resultPath)) {
    const result = JSON.parse(readFileSync(resultPath, "utf8"));
    return result.coverage?.state === "lost" ? result : undefined;
  }
  let identity;
  try {
    identity = readWorkerIdentity(directory);
  } catch (error) {
    if (error.code !== "ENOENT") throw error;
    return undefined;
  }
  if (checkMailboxWorkerLiveness(identity, directory) !== "dead")
    return undefined;
  return recordLostTerminalResult(directory, workerLossReason);
}

export async function terminateMailboxWorker(
  identity,
  directory,
  { readCommand = readProcessCommandSync } = {},
) {
  const { pid } = identity;
  if (!(Number.isSafeInteger(pid) && pid > 0))
    throw new Error("CI mailbox contains an invalid worker identity");
  if (!workerIsRunning(pid)) return;
  if (!verifyMailboxWorker(identity, directory, readCommand)) return;
  process.kill(pid, "SIGTERM");
  if (await waitForWorkerExit(pid)) return;
  if (!verifyMailboxWorker(identity, directory, readCommand)) return;
  process.kill(pid, "SIGKILL");
  if (!(await waitForWorkerExit(pid)))
    throw new Error(`CI observer worker ${pid} did not terminate`);
}

// After a normal terminal publication, wait for the matching worker to exit
// before treating stop as complete. Escalate only if voluntary exit stalls.
// Unknown identity is never signaled, but still gets the bounded wait: a
// worker exiting after its publication can show a transient command, such as
// `[node]`, before it stops running.
export async function awaitMailboxWorkerExit(identity, directory) {
  const { pid } = identity ?? {};
  if (!(Number.isSafeInteger(pid) && pid > 0)) return;
  const liveness = checkMailboxWorkerLiveness(identity, directory);
  if (liveness === "dead") return;
  if (await waitForWorkerExit(pid)) return;
  if (liveness === "alive") await terminateMailboxWorker(identity, directory);
}
