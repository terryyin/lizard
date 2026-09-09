import { spawn } from "node:child_process";
import { once } from "node:events";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  watch,
  writeFileSync,
} from "node:fs";
import { basename, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import {
  publishMailboxEvent,
  readWorkerIdentity,
  recordLostTerminalResult,
  recordTerminalResult,
  recordWorkerIdentity,
  terminalResultDeadlineCode,
  waitForTerminalResult,
} from "./ci-mailbox-store.mjs";
import {
  mailboxWorkerPath,
  terminateMailboxWorker,
} from "./ci-mailbox-worker-process.mjs";
import { executionBudgetMs, watchCiExecution } from "./watch-ci-execution.mjs";

export {
  publishMailboxEvent,
  readDeliveryProgress,
  readMailboxEvents,
  readWorkerIdentity,
  recordDeliveryProgress,
  recordWorkerIdentity,
} from "./ci-mailbox-store.mjs";

export const checkoutRoot = fileURLToPath(
  new URL("../../../../", import.meta.url),
);
// Native hooks and Nix launchers share this directory; it must not follow TMPDIR.
export const mailboxRoot =
  process.env.DOUGH_CI_MAILBOX_ROOT ??
  join("/tmp", `dough-ci-${process.getuid?.() ?? "user"}`);
export const receiptPrefix = "CI_OBSERVER ";
const resultPrefix = "CI_OBSERVER_RESULT ";

export function readMailbox(
  directory,
  root = checkoutRoot,
  storage = mailboxRoot,
) {
  if (
    resolve(directory, "..") !== resolve(storage) ||
    !/^watch-/.test(basename(directory))
  ) {
    throw new Error("CI mailbox is outside the observer directory");
  }
  const request = JSON.parse(
    readFileSync(join(directory, "request.json"), "utf8"),
  );
  if (resolve(request.root) !== resolve(root))
    throw new Error("CI mailbox belongs to another checkout");
  return request;
}

export function createMailbox(
  request,
  { root = checkoutRoot, storage = mailboxRoot } = {},
) {
  mkdirSync(storage, { recursive: true, mode: 0o700 });
  const directory = mkdtempSync(join(storage, "watch-"));
  writeFileSync(
    join(directory, "request.json"),
    JSON.stringify({ ...request, root }),
    { mode: 0o600 },
  );
  mkdirSync(join(directory, "events"), { mode: 0o700 });
  return directory;
}

export async function runMailboxWorker(
  directory,
  { observe, onRecord, root = checkoutRoot, storage = mailboxRoot } = {},
) {
  const request = readMailbox(directory, root, storage);
  const abort = new AbortController();
  const stop = () => {
    if (existsSync(join(directory, "stop"))) abort.abort();
  };
  const subscription = watch(directory, stop);
  const stopFallback = setInterval(stop, 100);
  stopFallback.unref();
  stop();
  const recordEvent = (event) => {
    const sequence = publishMailboxEvent(directory, event);
    onRecord?.({ sequence, event });
  };
  let status;
  try {
    let event;
    if (!abort.signal.aborted)
      event = await (observe ?? watchCiExecution)({
        ...request,
        signal: abort.signal,
        emit: recordEvent,
      });
    status = abort.signal.aborted ? "stopped" : "finished";
    if (!abort.signal.aborted && event) recordEvent(event);
  } catch (error) {
    status = abort.signal.aborted ? "stopped" : "finished";
    if (!abort.signal.aborted)
      recordEvent({
        type: "CI_MONITOR_UNAVAILABLE",
        repo: request.repo,
        sha: request.sha,
        reason: String(error).slice(0, 600),
      });
  } finally {
    subscription.close();
    clearInterval(stopFallback);
  }
  recordTerminalResult(directory, request, status);
}

export async function streamMailboxWorker(request, options = {}) {
  const directory = createMailbox(request, options);
  const stopOnSignal = () => requestMailboxStop(directory, options);
  if (options.stopOnSignal) {
    process.once("SIGINT", stopOnSignal);
    process.once("SIGTERM", stopOnSignal);
  }
  options.write?.(
    `${receiptPrefix}${JSON.stringify({ directory, pid: process.pid })}\n`,
  );
  try {
    await runMailboxWorker(directory, {
      ...options,
      onRecord: (record) => options.write?.(`${JSON.stringify(record)}\n`),
    });
  } finally {
    if (options.stopOnSignal) {
      process.removeListener("SIGINT", stopOnSignal);
      process.removeListener("SIGTERM", stopOnSignal);
    }
  }
  return directory;
}

export function requestMailboxStop(directory, options = {}) {
  readMailbox(directory, options.root, options.storage);
  writeFileSync(join(directory, "stop"), "", { mode: 0o600 });
}

async function stopMailbox(directory) {
  requestMailboxStop(directory);
  try {
    return await waitForTerminalResult(directory);
  } catch (error) {
    if (error.code !== terminalResultDeadlineCode) throw error;
    await terminateMailboxWorker(readWorkerIdentity(directory), directory);
    return recordLostTerminalResult(directory);
  }
}

async function startMailbox(request) {
  const validRepository = /^[\w.-]+\/[\w.-]+$/.test(request.repo ?? "");
  const validExecution =
    request.mode === "execution" &&
    validRepository &&
    typeof request.branch === "string" &&
    request.branch.length > 0 &&
    Number.isFinite(request.maxDurationMs) &&
    request.maxDurationMs > 0;
  if (!validExecution)
    throw new Error("Expected --execution OWNER/REPO BRANCH [BUDGET_MS]");
  const directory = createMailbox(request);
  const child = spawn(
    process.execPath,
    [mailboxWorkerPath, "worker", directory],
    {
      cwd: checkoutRoot,
      detached: true,
      stdio: "ignore",
    },
  );
  await once(child, "spawn");
  recordWorkerIdentity(directory, { pid: child.pid });
  child.unref();
  return directory;
}

export function probeMailbox(options = {}) {
  const directory = createMailbox({ probe: true }, options);
  publishMailboxEvent(directory, { type: "CI_MONITOR_READY" });
  recordTerminalResult(directory, { probe: true }, "finished");
  return directory;
}

if (
  process.argv[1] &&
  import.meta.url === pathToFileURL(process.argv[1]).href
) {
  const [command, ...args] = process.argv.slice(2);
  if (command === "worker") {
    await runMailboxWorker(args[0]);
  } else if (["start", "stream"].includes(command)) {
    const [, repo, branch, budget] = args;
    const request = {
      mode: args[0] === "--execution" ? "execution" : undefined,
      repo,
      branch,
      maxDurationMs: budget ? Number(budget) : executionBudgetMs,
    };
    if (command === "stream") {
      const directory = await streamMailboxWorker(request, {
        write: (output) => process.stdout.write(output),
        stopOnSignal: true,
      });
      const terminal = await waitForTerminalResult(directory);
      process.stdout.write(
        `${resultPrefix}${JSON.stringify({ directory, terminal })}\n`,
      );
    } else {
      const directory = await startMailbox(request);
      process.stdout.write(
        `${receiptPrefix}${JSON.stringify({ directory })}\n`,
      );
    }
  } else if (command === "probe") {
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory: probeMailbox() })}\n`,
    );
  } else if (command === "stop") {
    const directory = args[0];
    const terminal = await stopMailbox(directory);
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory, terminal })}\n`,
    );
  } else {
    throw new Error(
      "Usage: ci-mailbox.mjs probe | start --execution OWNER/REPO BRANCH [BUDGET_MS] | stream --execution OWNER/REPO BRANCH [BUDGET_MS] | stop DIRECTORY",
    );
  }
}
