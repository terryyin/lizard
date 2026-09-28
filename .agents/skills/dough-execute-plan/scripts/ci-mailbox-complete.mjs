import { writeFileSync } from "node:fs";
import { join } from "node:path";
import { readMailbox } from "./ci-mailbox-location.mjs";
import { awaitRevision } from "./ci-mailbox-await.mjs";
import {
  readDeliveryProgress,
  readMailboxEvents,
  readWorkerIdentity,
  recordLostTerminalResult,
  terminalResultDeadline,
  terminalResultDeadlineCode,
  waitForTerminalResult,
  workerLossReason,
} from "./ci-mailbox-store.mjs";
import {
  awaitMailboxWorkerExit,
  checkMailboxWorkerLiveness,
  terminateMailboxWorker,
} from "./ci-mailbox-worker-process.mjs";

export function requestMailboxStop(directory, options = {}) {
  readMailbox(directory, options.root, options.storage);
  writeFileSync(join(directory, "stop"), "", { mode: 0o600 });
}

// Aborts once the recorded worker has exited. Only that worker publishes the
// terminal result, always before it exits, so a stop need not wait out its
// deadline for a worker that is already gone. A live PID running another
// command (unknown identity) is not a departure: the deadline stays in charge.
function watchWorkerDeparture(directory, readCommand) {
  const departed = new AbortController();
  let identity;
  try {
    identity = readWorkerIdentity(directory);
  } catch (error) {
    if (error.code !== "ENOENT") throw error;
    return { signal: departed.signal, close: () => undefined };
  }
  const check = () => {
    try {
      if (
        checkMailboxWorkerLiveness(identity, directory, { readCommand }) ===
        "dead"
      )
        departed.abort();
    } catch {
      // An unreadable process table leaves the lifecycle deadline in charge.
    }
  };
  const timer = setInterval(check, 50);
  check();
  return { signal: departed.signal, close: () => clearInterval(timer) };
}

export async function stopMailbox(directory, options = {}) {
  requestMailboxStop(directory, options);
  const { readCommand } = options;
  const departure = watchWorkerDeparture(directory, readCommand);
  let terminal;
  try {
    terminal = await waitForTerminalResult(directory, {
      deadline: AbortSignal.any([terminalResultDeadline(), departure.signal]),
    });
  } catch (error) {
    if (error.code !== terminalResultDeadlineCode) throw error;
  } finally {
    departure.close();
  }
  if (terminal) {
    try {
      await awaitMailboxWorkerExit(readWorkerIdentity(directory), directory);
    } catch (error) {
      if (error.code !== "ENOENT") throw error;
    }
    return terminal;
  }
  // Read before terminating, which itself ends the worker.
  const departed = departure.signal.aborted;
  await terminateMailboxWorker(readWorkerIdentity(directory), directory, {
    readCommand,
  });
  return recordLostTerminalResult(
    directory,
    departed ? workerLossReason : undefined,
  );
}

function unreadActionableFailures(directory) {
  const { deliveredThrough } = readDeliveryProgress(directory);
  return readMailboxEvents(directory, deliveredThrough)
    .map((record) => record.event)
    .filter((event) => event?.type === "CI_FAILURE");
}

function shutdownConfirmed(directory, terminal) {
  try {
    const liveness = checkMailboxWorkerLiveness(
      readWorkerIdentity(directory),
      directory,
    );
    if (liveness === "alive")
      return {
        status: "unconfirmed",
        limitation: "observer_still_running",
        terminal,
      };
  } catch (error) {
    if (error.code !== "ENOENT")
      return {
        status: "unconfirmed",
        limitation: String(error).slice(0, 600),
        terminal,
      };
  }
  return { status: "confirmed", terminal };
}

// Thin completion composition: reuse the read-only applicable wait, then shut
// down only when that wait's outcome permits ending observation. Failure and
// an interrupted wait retain the observer for diagnosis or an explicit stop.
export async function completeRevision(directory, sha, options = {}) {
  const awaited = await awaitRevision(directory, sha, options);
  if (
    awaited.verdict === "failure" ||
    awaited.unresolvedReason === "wait_cancelled"
  )
    return { ...awaited, shutdown: { status: "retained" } };

  const unreadFailures = unreadActionableFailures(directory);
  if (unreadFailures.length > 0)
    return {
      ...awaited,
      shutdown: { status: "retained", reason: "unread_actionable_failure" },
      unreadActionableFailures: unreadFailures,
    };

  try {
    const terminal = await stopMailbox(directory, options);
    return { ...awaited, shutdown: shutdownConfirmed(directory, terminal) };
  } catch (error) {
    return {
      ...awaited,
      shutdown: {
        status: "unconfirmed",
        limitation: String(error).slice(0, 600),
      },
    };
  }
}
