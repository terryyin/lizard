import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";
import { readMailbox } from "./ci-mailbox-location.mjs";
import { readRevisionCoverage } from "./ci-mailbox-revision-coverage.mjs";
import { readMailboxEvents, readWorkerIdentity } from "./ci-mailbox-store.mjs";
import { checkMailboxWorkerLiveness } from "./ci-mailbox-worker-process.mjs";
import { isFullGitRevision } from "./ci-revisions.mjs";

export const revisionWaitDeadlineMs = 10 * 60 * 1000;

const terminalVerdicts = new Set(["success", "failure"]);

function targetOf(request) {
  return { repo: request.repo, branch: request.branch };
}

function result(requestedSha, target, effectiveEvidence, outcome) {
  return { requestedSha, target, effectiveEvidence, ...outcome };
}

function effectiveCoverage(coverage) {
  if (!coverage)
    return {
      evidence: { source: "none" },
      outcome: { unresolvedReason: "missing_registration" },
    };
  if (coverage.state !== "not_required") {
    const evidence = { source: "exact", revision: coverage };
    if (terminalVerdicts.has(coverage.state))
      return { evidence, outcome: { verdict: coverage.state } };
    if (coverage.state === "incomplete")
      return { evidence, outcome: { unresolvedReason: "incomplete" } };
    return { evidence };
  }

  const evidence = {
    source: "not_required_basis",
    revision: { sha: coverage.sha, state: coverage.state },
    ...(coverage.basis ? { basis: coverage.basis } : {}),
  };
  if (terminalVerdicts.has(coverage.basis?.state))
    return { evidence, outcome: { verdict: coverage.basis.state } };
  if (coverage.basis?.state === "incomplete")
    return { evidence, outcome: { unresolvedReason: "incomplete" } };
  return { evidence };
}

function terminalObservation(directory) {
  const path = join(directory, "result.json");
  if (!existsSync(path)) return;
  const terminal = JSON.parse(readFileSync(path, "utf8"));
  if (terminal.coverage?.state === "lost")
    return {
      unresolvedReason: "observation_unavailable",
      detail: "worker_lost",
      terminal,
    };
  if (terminal.status === "stopped")
    return {
      unresolvedReason: "observation_cancelled",
      terminal,
    };
  return {
    unresolvedReason: "observation_unavailable",
    detail: "observation_ended",
    terminal,
  };
}

function unavailableObservation(directory) {
  const event = readMailboxEvents(directory)
    .map((record) => record.event)
    .findLast(({ type }) => type === "CI_MONITOR_UNAVAILABLE");
  return event
    ? {
        unresolvedReason: "observation_unavailable",
        detail: "monitor_unavailable",
        event,
      }
    : undefined;
}

function observerLiveness(directory) {
  try {
    return checkMailboxWorkerLiveness(readWorkerIdentity(directory), directory);
  } catch (error) {
    if (error.code === "ENOENT") return "unknown";
    throw error;
  }
}

export async function awaitRevision(
  directory,
  sha,
  {
    cancellation,
    deadlineMs = revisionWaitDeadlineMs,
    now = Date.now,
    sleep = pause,
    recheckMs = 50,
    root,
    storage,
    workerLiveness = observerLiveness,
  } = {},
) {
  if (!isFullGitRevision(sha))
    throw new Error("Expected a full Git revision SHA");
  if (!(Number.isFinite(deadlineMs) && deadlineMs > 0))
    throw new Error("Expected a finite positive revision wait deadline");
  const requestedSha = sha.toLowerCase();
  const request = readMailbox(directory, root, storage);
  const target = targetOf(request);
  const deadline = now() + deadlineMs;

  while (true) {
    let projection;
    try {
      projection = effectiveCoverage(
        readRevisionCoverage(directory).find(
          (revision) => revision.sha === requestedSha,
        ),
      );
    } catch (error) {
      return result(
        requestedSha,
        target,
        { source: "unreadable" },
        {
          unresolvedReason: "evidence_unreadable",
          detail: String(error).slice(0, 600),
        },
      );
    }
    if (projection.outcome)
      return result(
        requestedSha,
        target,
        projection.evidence,
        projection.outcome,
      );

    try {
      const unavailable = unavailableObservation(directory);
      if (unavailable)
        return result(requestedSha, target, projection.evidence, unavailable);
      const terminal = terminalObservation(directory);
      if (terminal)
        return result(requestedSha, target, projection.evidence, terminal);
      const liveness = workerLiveness(directory);
      if (liveness !== "alive") {
        // A normal stop publishes its terminal record before the worker exits.
        // Recheck after observing the exit so that boundary race is reported
        // as cancellation rather than unexpected loss.
        const terminalAfterExit = terminalObservation(directory);
        if (terminalAfterExit)
          return result(
            requestedSha,
            target,
            projection.evidence,
            terminalAfterExit,
          );
        return result(requestedSha, target, projection.evidence, {
          unresolvedReason: "observation_unavailable",
          detail:
            liveness === "dead" ? "worker_exited" : "worker_identity_unknown",
        });
      }
    } catch (error) {
      return result(requestedSha, target, projection.evidence, {
        unresolvedReason: "evidence_unreadable",
        detail: String(error).slice(0, 600),
      });
    }

    if (cancellation?.aborted)
      return result(requestedSha, target, projection.evidence, {
        unresolvedReason: "wait_cancelled",
      });
    const remaining = deadline - now();
    if (remaining <= 0)
      return result(requestedSha, target, projection.evidence, {
        unresolvedReason: "timeout",
      });
    try {
      await sleep(Math.min(recheckMs, remaining), undefined, {
        signal: cancellation,
      });
    } catch (error) {
      if (!cancellation?.aborted) throw error;
      return result(requestedSha, target, projection.evidence, {
        unresolvedReason: "wait_cancelled",
      });
    }
  }
}
