import { spawn } from "node:child_process";
import { once } from "node:events";
import {
  checkoutRoot,
  createMailbox,
  mailboxRoot,
  readMailbox,
  receiptPrefix,
} from "./ci-mailbox-location.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import {
  acknowledgeDelivery,
  publishMailboxEvent,
  recordTerminalResult,
  recordWorkerIdentity,
  waitForTerminalResult,
} from "./ci-mailbox-store.mjs";
import {
  listRegisteredRevisions,
  observeRevisionCoverage,
  registerPushedRevision,
} from "./ci-mailbox-revision-coverage.mjs";
import { watchMailboxChanges } from "./ci-mailbox-change-watch.mjs";
import {
  mailboxWorkerPath,
  withStreamWorkerIdentity,
} from "./ci-mailbox-worker-process.mjs";
import { executionBudgetMs, watchCiExecution } from "./watch-ci-execution.mjs";
import { awaitRevision, writeRevisionReceipt } from "./ci-mailbox-await.mjs";
import { claimMailbox, codexStreamOwner } from "./ci-observer-owner.mjs";
import {
  completeRevision,
  requestMailboxStop,
  stopMailbox,
} from "./ci-mailbox-complete.mjs";

export {
  checkoutRoot,
  createMailbox,
  mailboxRoot,
  readMailbox,
  receiptPrefix,
} from "./ci-mailbox-location.mjs";
export {
  publishMailboxEvent,
  readDeliveryProgress,
  readMailboxEvents,
  readWorkerIdentity,
  recordDeliveryProgress,
  recordWorkerIdentity,
  workerLossReason,
} from "./ci-mailbox-store.mjs";
export {
  readRevisionCoverage,
  registerPushedRevision,
} from "./ci-mailbox-revision-coverage.mjs";
export { mailboxWorkerLoss } from "./ci-mailbox-worker-process.mjs";
export {
  completeRevision,
  requestMailboxStop,
} from "./ci-mailbox-complete.mjs";

const resultPrefix = "CI_OBSERVER_RESULT ";
export async function runMailboxWorker(
  directory,
  { observe, onRecord, root = checkoutRoot, storage = mailboxRoot } = {},
) {
  const request = readMailbox(directory, root, storage);
  const changes = watchMailboxChanges(directory);
  const stopped = changes.stopSignal;
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
        observeCoverage: (runs, observedAt, discoverAncestorCandidates) =>
          observeRevisionCoverage(directory, runs, request, observedAt, {
            discoverAncestorCandidates,
          }),
        registeredRevisions: async () => listRegisteredRevisions(directory),
        armRegistrationWake: changes.armRegistrationWake,
      });
    status = stopped.aborted ? "stopped" : "finished";
    if (!stopped.aborted && event) recordEvent(event);
  } catch (error) {
    status = stopped.aborted ? "stopped" : "finished";
    if (!stopped.aborted)
      recordEvent({
        type: "CI_MONITOR_UNAVAILABLE",
        repo: request.repo,
        sha: request.sha,
        reason: String(error).slice(0, 600),
      });
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

// Removes `--coordinator VALUE` from a stream command's arguments and returns
// the value.
function takeCoordinator(args) {
  const index = args.indexOf("--coordinator");
  if (index === -1) return undefined;
  const [, value] = args.splice(index, 2);
  if (!value || value.startsWith("--"))
    throw new Error("Expected --coordinator VALUE");
  return value;
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  const [command, ...args] = process.argv.slice(2);
  const coordinator = command === "stream" ? takeCoordinator(args) : undefined;
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
        coordinator,
      });
      const terminal = await waitForTerminalResult(directory);
      process.stdout.write(
        `${resultPrefix}${JSON.stringify({ directory, terminal })}\n`,
      );
    } else {
      const directory = await startExecutionMailbox(request);
      process.stdout.write(
        `${receiptPrefix}${JSON.stringify({ directory })}\n`,
      );
    }
  } else if (command === "probe") {
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory: probeMailbox() })}\n`,
    );
  } else if (command === "register-push") {
    const [directory, sha] = args;
    readMailbox(directory);
    const revision = registerPushedRevision(directory, sha);
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory, revision })}\n`,
    );
  } else if (command === "acknowledge") {
    const [directory, sequence] = args;
    readMailbox(directory);
    const deliveredThrough = acknowledgeDelivery(directory, Number(sequence));
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory, deliveredThrough })}\n`,
    );
  } else if (command === "await-revision") {
    const [directory, sha] = args;
    await writeRevisionReceipt(awaitRevision, directory, sha);
  } else if (command === "complete-revision") {
    const [directory, sha] = args;
    await writeRevisionReceipt(completeRevision, directory, sha);
  } else if (command === "stop") {
    const directory = args[0];
    const terminal = await stopMailbox(directory);
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory, terminal })}\n`,
    );
  } else {
    throw new Error(
      "Usage: ci-mailbox.mjs probe | start --execution OWNER/REPO BRANCH [BUDGET_MS] | stream --execution OWNER/REPO BRANCH [BUDGET_MS] [--coordinator VALUE] | register-push DIRECTORY SHA | acknowledge DIRECTORY SEQUENCE | await-revision DIRECTORY SHA | complete-revision DIRECTORY SHA | stop DIRECTORY",
    );
  }
}
