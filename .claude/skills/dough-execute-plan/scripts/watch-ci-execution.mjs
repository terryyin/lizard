import { setTimeout as pause } from "node:timers/promises";
import { createCommandAcquisition } from "./ci-command-adapter.mjs";
import { ciAttemptKey } from "./ci-failures.mjs";
import { GitHubAcquisitionHeld } from "./ci-github-actions.mjs";
import { createGitHubAcquisition } from "./ci-runs.mjs";
import { readCiConfiguration } from "./ci-project-configuration.mjs";

export const executionBudgetMs = 8 * 60 * 60 * 1000;

// One execution, one incremental stream. Polling never calls an AI service.
export async function watchCiExecution({
  repo,
  branch,
  signal,
  gh,
  sleep = pause,
  emit = () => undefined,
  pollMs = 30_000,
  maxDurationMs = executionBudgetMs,
  now = Date.now,
  root = process.cwd(),
  observeCoverage = () => [],
  // A failed discovery withdraws locally observed progress as well as any
  // integrated-state evidence, including without a declared policy.
  withdrawIntegratedState = () => undefined,
  registeredRevisions = async () => [],
  // Returns a signal that aborts once a revision is registered after this
  // call; a pending recheck pause then ends at once.
  armRegistrationWake = () => undefined,
  adapterTimeoutMs,
}) {
  if (typeof branch !== "string" || !branch.trim())
    throw new Error("Execution CI observation requires a branch");
  if (!(Number.isFinite(maxDurationMs) && maxDurationMs > 0))
    throw new Error(
      "Execution CI observation requires a finite positive budget",
    );

  const { adapter, workflow, integratedPolicy } = readCiConfiguration(root);
  const observationAbort = new AbortController();
  const stopObservation = () => observationAbort.abort(signal?.reason);
  if (signal?.aborted) stopObservation();
  else signal?.addEventListener("abort", stopObservation, { once: true });
  const observationSignal = observationAbort.signal;
  const startedAt = now();
  const reportedIncomplete = new Set();
  // The acquisition's history and current-attempt reads, for coverage.
  const { acquireRuns, acquireFailure, ...coverageLookups } = adapter
    ? createCommandAcquisition({
        command: adapter,
        repo,
        branch,
        root,
        timeoutMs: adapterTimeoutMs,
        registeredRevisions,
      })
    : createGitHubAcquisition({
        repo,
        branch,
        startedAt,
        workflow,
        gh,
        signal: observationSignal,
      });
  let consecutiveErrors = 0;
  // A failure whose diagnostic is still unavailable stays known across polls,
  // so losing observation by any path still delivers it before the loss.
  let knownFailureEvent;

  const unavailable = (reason) => ({
    type: "CI_MONITOR_UNAVAILABLE",
    repo,
    branch,
    ...(adapter ? {} : { workflow }),
    reason: String(reason).slice(0, 600),
  });
  const reportObservationLoss = async (reason) => {
    if (knownFailureEvent) await emit(knownFailureEvent);
    await emit(unavailable(reason));
  };

  // A wait GitHub directed suspends acquisition until it ends, or until the
  // budget does: reported once per suspension, it is no observation error, and
  // a registration cannot shorten it.
  let suspensionReported = false;
  const suspendAcquisition = async ({ waitSeconds }) => {
    const waitMs = waitSeconds * 1000;
    if (!suspensionReported) {
      suspensionReported = true;
      await emit({
        type: "CI_MONITOR_THROTTLED",
        repo,
        branch,
        workflow,
        waitSeconds,
        resumesAt: new Date(now() + waitMs).toISOString(),
      });
    }
    const remainingMs = maxDurationMs - (now() - startedAt);
    await sleep(Math.max(Math.min(waitMs, remainingMs), 0), undefined, {
      signal: observationSignal,
    });
  };
  const held = (error) => error instanceof GitHubAcquisitionHeld;

  // Only the pause after a completed poll ends early on a registration;
  // retrying after a poll error keeps its full interval.
  const pauseUntilNextPoll = async (registration) => {
    try {
      await sleep(pollMs, undefined, {
        signal: registration
          ? AbortSignal.any([observationSignal, registration])
          : observationSignal,
      });
    } catch (error) {
      if (observationSignal.aborted || !registration?.aborted) throw error;
    }
  };

  try {
    while (now() - startedAt < maxDurationMs) {
      if (observationSignal.aborted) return;
      // Armed before the poll, so a registration landing during it still
      // brings one more check right after it.
      const registration = armRegistrationWake();
      let matching;
      try {
        matching = await acquireRuns(observationSignal);
      } catch (error) {
        if (held(error)) {
          await suspendAcquisition(error);
          continue;
        }
        // Ending observation lists nothing: it withdraws no recorded evidence.
        if (observationSignal.aborted) return;
        await withdrawIntegratedState(error);
        consecutiveErrors += 1;
        if (consecutiveErrors === 3) throw error;
        await sleep(pollMs, undefined, { signal: observationSignal });
        continue;
      }

      const registeredShas = (await registeredRevisions()).map((sha) =>
        String(sha).toLowerCase(),
      );
      const actionable =
        registeredShas.length === 0
          ? matching
          : matching.filter((run) =>
              registeredShas.includes(run.headSha?.toLowerCase()),
            );
      // Coverage selection runs before failure acquisition so a `not_required`
      // revision's resolved basis ancestor (an attempt this poll's ordinary
      // `matching`/`actionable` lists may not include at all — the ancestor is
      // usually not itself registered here) can be folded into the same
      // failure-acquisition call as ordinary actionable runs, reusing its
      // existing per-run/job dedup: a failure shared by several ignored-only
      // descendants is still delivered exactly once. Coverage's own events are
      // still emitted after the failure event, preserving prior ordering.
      let coverageResult;
      try {
        coverageResult = await observeCoverage(matching, now(), {
          ...coverageLookups,
          integratedPolicy,
          signal: observationSignal,
        });
      } catch (error) {
        if (!held(error)) throw error;
        await suspendAcquisition(error);
        continue;
      }
      const coverageEvents = Array.isArray(coverageResult)
        ? coverageResult
        : (coverageResult?.events ?? []);
      const ancestorRuns = Array.isArray(coverageResult)
        ? []
        : (coverageResult?.ancestorRuns ?? []);
      const actionableKeys = new Set(
        actionable.map((run) => `${run.databaseId}:${run.attempt}`),
      );
      const actionableWithAncestors = ancestorRuns.length
        ? [
            ...actionable,
            ...ancestorRuns.filter(
              (run) => !actionableKeys.has(`${run.databaseId}:${run.attempt}`),
            ),
          ]
        : actionable;
      // A wait met while inspecting failures keeps what this poll already
      // learned: its coverage and incomplete records are still emitted below.
      let suspension;
      try {
        const { event, observationError, deferredFailureEvent } =
          await acquireFailure(actionableWithAncestors, observationSignal);
        if (event) await emit(event);
        knownFailureEvent = deferredFailureEvent;
        if (observationError) {
          consecutiveErrors += 1;
          if (consecutiveErrors === 3) throw new Error(observationError);
        } else {
          consecutiveErrors = 0;
        }
      } catch (error) {
        if (!held(error)) throw error;
        suspension = error;
      }
      for (const coverageEvent of coverageEvents) await emit(coverageEvent);
      const incomplete = actionable.find(
        (run) =>
          run.status === "completed" &&
          run.conclusion === "cancelled" &&
          !reportedIncomplete.has(ciAttemptKey(run.databaseId, run.attempt)),
      );
      if (incomplete) {
        await emit({
          type: "CI_INCOMPLETE",
          repo,
          sha: incomplete.headSha,
          branch: incomplete.headBranch,
          ...(adapter ? {} : { workflow }),
          runId: incomplete.databaseId,
          attempt: incomplete.attempt,
          conclusion: incomplete.conclusion,
          url: incomplete.url,
        });
        reportedIncomplete.add(
          ciAttemptKey(incomplete.databaseId, incomplete.attempt),
        );
      }
      if (suspension) {
        await suspendAcquisition(suspension);
        continue;
      }
      suspensionReported = false;
      await pauseUntilNextPoll(registration);
    }
    await reportObservationLoss(
      `Execution observation budget expired after ${maxDurationMs} ms.`,
    );
  } catch (error) {
    if (observationSignal.aborted) return;
    await reportObservationLoss(error instanceof Error ? error.message : error);
  } finally {
    signal?.removeEventListener("abort", stopObservation);
    observationAbort.abort();
  }
}
