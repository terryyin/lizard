import { setTimeout as pause } from "node:timers/promises";
import {
  ciAttemptKey,
  inspectRunsForFailure,
  reportedFailureEvidence,
} from "./ci-failures.mjs";
import {
  ciWorkflowFile,
  listRunsArguments,
  matchingCiRuns,
  readGitHubActions,
  startupCiRuns,
  viewRunArguments,
} from "./ci-runs.mjs";

export const executionBudgetMs = 8 * 60 * 60 * 1000;

// One execution, one incremental stream. Polling never calls an AI service.
export async function watchCiExecution({
  repo,
  branch,
  signal,
  gh = readGitHubActions,
  sleep = pause,
  emit = () => undefined,
  pollMs = 30_000,
  maxDurationMs = executionBudgetMs,
  now = Date.now,
}) {
  if (typeof branch !== "string" || !branch.trim())
    throw new Error("Execution CI observation requires a branch");
  if (!(Number.isFinite(maxDurationMs) && maxDurationMs > 0))
    throw new Error(
      "Execution CI observation requires a finite positive budget",
    );

  const observationAbort = new AbortController();
  const stopObservation = () => observationAbort.abort(signal?.reason);
  if (signal?.aborted) stopObservation();
  else signal?.addEventListener("abort", stopObservation, { once: true });
  const observationSignal = observationAbort.signal;
  const startedAt = now();
  const priorAttempts = new Map();
  const reportedFailures = new Set();
  const completedAttempts = new Set();
  const reportedIncomplete = new Set();
  const trackedRuns = new Map();
  const observedRunIds = new Set();
  const startupAttempts = new Map();
  const startupBoundary = `<=${new Date(startedAt).toISOString()}`;
  let startupDiscovery = true;
  let consecutiveErrors = 0;

  const unavailable = (reason) => ({
    type: "CI_MONITOR_UNAVAILABLE",
    repo,
    branch,
    workflow: ciWorkflowFile,
    reason: String(reason).slice(0, 600),
  });

  try {
    while (now() - startedAt < maxDurationMs) {
      if (observationSignal.aborted) return;
      let runs;
      let matching;
      try {
        runs = await gh(
          listRunsArguments({
            repo,
            branch,
            created: startupDiscovery ? startupBoundary : undefined,
            limit: startupDiscovery ? 100 : 20,
            includeCreatedAt: true,
          }),
          observationSignal,
        );
        matching = matchingCiRuns(runs, { branch });
        if (startupDiscovery) {
          for (const run of matching)
            startupAttempts.set(run.databaseId, run.attempt);
          matching = startupCiRuns(matching);
        } else {
          matching = matching.filter(
            (run) =>
              observedRunIds.has(run.databaseId) ||
              !startupAttempts.has(run.databaseId) ||
              run.status !== "completed" ||
              run.attempt > startupAttempts.get(run.databaseId),
          );
        }
        for (const run of matching) observedRunIds.add(run.databaseId);
        startupDiscovery = false;
        const visibleRunIds = new Set(matching.map((run) => run.databaseId));
        for (const run of matching) {
          if (run.status === "completed") trackedRuns.delete(run.databaseId);
          else trackedRuns.set(run.databaseId, run);
        }
        for (const [runId, tracked] of trackedRuns) {
          if (visibleRunIds.has(runId)) continue;
          const refreshed = {
            ...tracked,
            ...(await gh(viewRunArguments({ repo, runId }), observationSignal)),
          };
          trackedRuns.set(runId, refreshed);
          matching.push(refreshed);
        }
      } catch (error) {
        consecutiveErrors += 1;
        if (consecutiveErrors === 3) throw error;
        await sleep(pollMs, undefined, { signal: observationSignal });
        continue;
      }

      const { event, observationError } = await inspectRunsForFailure({
        repo,
        runs: matching,
        signal: observationSignal,
        gh,
        priorAttempts,
        reportedFailures,
        completedAttempts,
      });
      if (event) {
        await emit(event);
        for (const evidence of reportedFailureEvidence(event))
          reportedFailures.add(evidence);
      }
      if (observationError) {
        consecutiveErrors += 1;
        if (consecutiveErrors === 3) throw new Error(observationError);
      } else {
        consecutiveErrors = 0;
      }
      const incomplete = matching.find(
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
          workflow: ciWorkflowFile,
          runId: incomplete.databaseId,
          attempt: incomplete.attempt,
          conclusion: incomplete.conclusion,
          url: incomplete.url,
        });
        reportedIncomplete.add(
          ciAttemptKey(incomplete.databaseId, incomplete.attempt),
        );
      }
      for (const run of matching) {
        if (run.status === "completed") trackedRuns.delete(run.databaseId);
      }

      await sleep(pollMs, undefined, { signal: observationSignal });
    }
    await emit(
      unavailable(
        `Execution observation budget expired after ${maxDurationMs} ms.`,
      ),
    );
  } catch (error) {
    if (observationSignal.aborted) return;
    await emit(unavailable(error instanceof Error ? error.message : error));
  } finally {
    signal?.removeEventListener("abort", stopObservation);
    observationAbort.abort();
  }
}
