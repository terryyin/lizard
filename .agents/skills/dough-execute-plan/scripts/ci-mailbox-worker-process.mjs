import { execFile } from "node:child_process";
import { setTimeout as pause } from "node:timers/promises";
import { fileURLToPath } from "node:url";

export const mailboxWorkerPath = fileURLToPath(
  new URL("./ci-mailbox.mjs", import.meta.url),
);

function workerIsRunning(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    if (error.code === "ESRCH") return false;
    throw error;
  }
}

async function waitForWorkerExit(pid, timeoutMs = 1_000) {
  const deadline = Date.now() + timeoutMs;
  while (workerIsRunning(pid) && Date.now() < deadline) await pause(20);
  return !workerIsRunning(pid);
}

async function readProcessCommand(pid) {
  return new Promise((resolveCommand, rejectCommand) => {
    execFile(
      "ps",
      ["-ww", "-p", String(pid), "-o", "command="],
      (error, stdout) => {
        if (!error) {
          resolveCommand(stdout.trim());
          return;
        }
        if (error.code === 1) {
          resolveCommand(undefined);
          return;
        }
        rejectCommand(error);
      },
    );
  });
}

async function verifyMailboxWorker(pid, directory) {
  const command = await readProcessCommand(pid);
  if (command === undefined) return false;
  const expected = `${process.execPath} ${mailboxWorkerPath} worker ${directory}`;
  if (command !== expected)
    throw new Error(`CI observer worker ${pid} does not match this mailbox`);
  return true;
}

export async function terminateMailboxWorker({ pid }, directory) {
  if (!(Number.isSafeInteger(pid) && pid > 0))
    throw new Error("CI mailbox contains an invalid worker identity");
  if (!workerIsRunning(pid)) return;
  if (!(await verifyMailboxWorker(pid, directory))) return;
  process.kill(pid, "SIGTERM");
  if (await waitForWorkerExit(pid)) return;
  if (!(await verifyMailboxWorker(pid, directory))) return;
  process.kill(pid, "SIGKILL");
  if (!(await waitForWorkerExit(pid)))
    throw new Error(`CI observer worker ${pid} did not terminate`);
}
