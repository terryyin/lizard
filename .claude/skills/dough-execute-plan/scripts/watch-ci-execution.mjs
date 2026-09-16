import { setTimeout as pause } from "node:timers/promises";
import {
  createCommandFailureAcquisition,
  createCommandRunAcquisition,
  readCiAdapter,
} from "./ci-command-adapter.mjs";
import {
  ciAttemptKey,
  createGitHubFailureAcquisition,
} from "./ci-failures.mjs";
import {
  ciWorkflowFile,
  createGitHubRunAcquisition,
  readGitHubActions,
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
  root = process.cwd(),
  observeCoverage = () => [],
  adapterTimeoutMs,
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
  const reportedIncomplete = new Set();
  const adapter = readCiAdapter(root);
  const acquireRuns = adapter
    ? createCommandRunAcquisition({
        command: adapter,
        repo,
        branch,
        root,
        timeoutMs: adapterTimeoutMs,
      })
    : createGitHubRunAcquisition({ repo, branch, startedAt, gh });
  const acquireFailure = adapter
    ? createCommandFailureAcquisition({
        command: adapter,
        repo,
        branch,
        root,
        timeoutMs: adapterTimeoutMs,
      })
    : createGitHubFailureAcquisition({ repo, gh });
  let consecutiveErrors = 0;

  const unavailable = (reason) => ({
    type: "CI_MONITOR_UNAVAILABLE",
    repo,
    branch,
    ...(adapter ? {} : { workflow: ciWorkflowFile }),
    reason: String(reason).slice(0, 600),
  });

  try {
    while (now() - startedAt < maxDurationMs) {
      if (observationSignal.aborted) return;
      let matching;
      try {
        matching = await acquireRuns(observationSignal);
      } catch (error) {
        consecutiveErrors += 1;
        if (consecutiveErrors === 3) throw error;
        await sleep(pollMs, undefined, { signal: observationSignal });
        continue;
      }

      const { event, observationError, deferredFailureEvent } =
        await acquireFailure(matching, observationSignal);
      if (event) {
        await emit(event);
      }
      if (observationError) {
        consecutiveErrors += 1;
        if (consecutiveErrors === 3) {
          if (deferredFailureEvent) await emit(deferredFailureEvent);
          throw new Error(observationError);
        }
      } else {
        consecutiveErrors = 0;
      }
      for (const coverageEvent of await observeCoverage(matching))
        await emit(coverageEvent);
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
          ...(adapter ? {} : { workflow: ciWorkflowFile }),
          runId: incomplete.databaseId,
          attempt: incomplete.attempt,
          conclusion: incomplete.conclusion,
          url: incomplete.url,
        });
        reportedIncomplete.add(
          ciAttemptKey(incomplete.databaseId, incomplete.attempt),
        );
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
