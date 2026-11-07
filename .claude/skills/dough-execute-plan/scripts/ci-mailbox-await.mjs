import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";
import { readMailbox, receiptPrefix } from "./ci-mailbox-location.mjs";
import {
  effectiveRevisionState,
  freshPendingRevision,
  readRevisionCoverage,
  revisionVerdicts,
} from "./ci-mailbox-revision-records.mjs";
import { readMailboxEvents, readWorkerIdentity } from "./ci-mailbox-store.mjs";
import { checkMailboxWorkerLiveness } from "./ci-mailbox-worker-process.mjs";
import { isFullGitRevision } from "./ci-revisions.mjs";

export const revisionWaitDeadlineMs = 10 * 60 * 1000;

const targetOf = (request) => ({ repo: request.repo, branch: request.branch });

function result(requestedSha, target, effectiveEvidence, outcome) {
  return { requestedSha, target, effectiveEvidence, ...outcome };
}

function effectiveCoverage(coverage, now) {
  if (!coverage)
    return {
      evidence: { source: "none" },
      outcome: { unresolvedReason: "missing_registration" },
    };
  const { integrated, ...revision } = coverage;
  // A validated descendant completes B while B retains its exact state or basis.
  if (integrated?.state === "success")
    return {
      evidence: {
        source: "integrated_state",
        accepted: revision,
        validated: integrated.validated,
        policy: integrated.policy,
        ancestry: integrated.ancestry,
      },
      outcome: { verdict: "success" },
    };
  const judged = integrated ? { integrated } : {};
  const evidence =
    coverage.state === "not_required"
      ? {
          source: "not_required_basis",
          revision: { sha: coverage.sha, state: coverage.state },
          ...(coverage.basis ? { basis: coverage.basis } : {}),
          ...judged,
        }
      : { source: "exact", revision, ...judged };
  const outcome = effectiveRevisionState(coverage);
  if (revisionVerdicts.has(outcome))
    return { evidence, outcome: { verdict: outcome } };
  // A cancelled attempt ends the wait unless fresh queue progress explains
  // pending work or the policy still permits integrated-state completion.
  if (
    outcome === "incomplete" &&
    !integrated &&
    !freshPendingRevision(coverage, now)
  )
    return { evidence, outcome: { unresolvedReason: "incomplete" } };
  return { evidence };
}

const recordedCoverage = (directory, requestedSha, now = Date.now()) =>
  effectiveCoverage(
    readRevisionCoverage(directory).find(
      (revision) => revision.sha === requestedSha,
    ),
    now,
  );

// The recorded verdict and its effective evidence, whether observation still
// runs. Undefined while no verdict is proved, including a cancelled attempt.
export function recordedCompletion(directory, sha) {
  let projection;
  try {
    projection = recordedCoverage(directory, sha.toLowerCase());
  } catch {
    return undefined;
  }
  const { evidence, outcome } = projection;
  return outcome?.verdict ? { verdict: outcome.verdict, evidence } : undefined;
}

export const recordedVerdict = (directory, sha) =>
  recordedCompletion(directory, sha)?.verdict;

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
    return { unresolvedReason: "observation_cancelled", terminal };
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
      projection = recordedCoverage(directory, requestedSha, now());
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

// Runs the revision command `run` (`awaitRevision` or `completeRevision`) so
// that SIGINT or SIGTERM cancels it, and writes its receipt.
export async function writeRevisionReceipt(run, directory, sha) {
  const cancellation = new AbortController();
  const cancel = () => cancellation.abort();
  process.once("SIGINT", cancel);
  process.once("SIGTERM", cancel);
  try {
    const result = await run(directory, sha, {
      cancellation: cancellation.signal,
    });
    process.stdout.write(`${receiptPrefix}${JSON.stringify(result)}\n`);
  } finally {
    process.removeListener("SIGINT", cancel);
    process.removeListener("SIGTERM", cancel);
  }
}
