import { spawn } from "node:child_process";
import { once } from "node:events";
import {
  checkoutRoot,
  createMailbox,
  mailboxRoot,
  readMailbox,
  receiptPrefix,
} from "./ci-mailbox-location.mjs";
import {
  publishMailboxEvent,
  recordTerminalResult,
  recordWorkerIdentity,
} from "./ci-mailbox-store.mjs";
import {
  observeRevisionCoverage,
  withdrawIntegratedState,
} from "./ci-mailbox-revision-coverage.mjs";
import { listRegisteredRevisions } from "./ci-mailbox-revision-records.mjs";
import { watchMailboxChanges } from "./ci-mailbox-change-watch.mjs";
import {
  mailboxWorkerPath,
  withStreamWorkerIdentity,
} from "./ci-mailbox-worker-process.mjs";
import { watchCiExecution } from "./watch-ci-execution.mjs";
import { claimMailbox, codexStreamOwner } from "./ci-observer-owner.mjs";
import { requestMailboxStop } from "./ci-mailbox-complete.mjs";
import {
  monitorUnavailableEvent,
  observationCause,
} from "./ci-monitor-unavailable.mjs";

export async function runMailboxWorker(
  directory,
  { observe, onRecord, root = checkoutRoot, storage = mailboxRoot } = {},
) {
  const request = readMailbox(directory, root, storage);
  const changes = watchMailboxChanges(directory);
  const stopped = changes.stopSignal;
  const observeCoverage = (runs, observedAt, lookups) =>
    observeRevisionCoverage(directory, runs, request, observedAt, lookups);
  const recordEvent = (event) => {
    const sequence = publishMailboxEvent(directory, event);
    onRecord?.({ sequence, event });
  };
  let status;
  try {
    let event;
    if (!stopped.aborted)
      event = await (observe ?? watchCiExecution)({
        ...request,
        signal: stopped,
        emit: recordEvent,
        observeCoverage,
        withdrawIntegratedState: (detail) =>
          withdrawIntegratedState(directory, detail),
        registeredRevisions: async () => listRegisteredRevisions(directory),
        armRegistrationWake: changes.armRegistrationWake,
      });
    status = stopped.aborted ? "stopped" : "finished";
    if (!stopped.aborted && event) recordEvent(event);
  } catch (error) {
    status = stopped.aborted ? "stopped" : "finished";
    if (!stopped.aborted)
      recordEvent(
        monitorUnavailableEvent({
          repo: request.repo,
          branch: request.branch,
          sha: request.sha,
          reason: String(error).slice(0, 600),
          cause: observationCause.observationError,
        }),
      );
  } finally {
    changes.close();
  }
  recordTerminalResult(directory, request, status);
}

// `options.coordinator` names the coordinator arming this stream; its claim is
// on the mailbox before the receipt exposes the directory.
export async function streamMailboxWorker(request, options = {}) {
  const directory = createMailbox(request, options);
  const owner = codexStreamOwner({
    root: options.root ?? checkoutRoot,
    coordinator: options.coordinator,
  });
  if (owner) claimMailbox(directory, owner);
  return withStreamWorkerIdentity(directory, async () => {
    const stopOnSignal = () => requestMailboxStop(directory, options);
    try {
      if (options.stopOnSignal) {
        process.once("SIGINT", stopOnSignal);
        process.once("SIGTERM", stopOnSignal);
      }
      options.write?.(
        `${receiptPrefix}${JSON.stringify({ directory, pid: process.pid })}\n`,
      );
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
  });
}

export async function startExecutionMailbox(request, options = {}) {
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
  const directory = createMailbox(request, options);
  const child = spawn(
    process.execPath,
    [mailboxWorkerPath, "worker", directory],
    {
      cwd: options.root ?? checkoutRoot,
      detached: true,
      stdio: "ignore",
      env: options.env,
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
