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
  discoverApplicabilityCandidateRuns,
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
  // GitHub-only: CI-path applicability reuses a proved ancestor attempt, and
  // that ancestor is not always among the runs the ordinary poll already
  // surfaced (older completed runs intentionally drop out of `matching` once
  // seen, per createGitHubRunAcquisition's own startup/observed-run pruning).
  // A custom adapter does not own GitHub workflow-path applicability, so it
  // gets no candidate discovery here; ci-mailbox-revision-coverage.mjs then
  // has nothing to broaden with and an ignored-only revision simply stays
  // undiscovered, unchanged from before this wiring existed.
  // Deliberately left run-shaped (not flattened to bare SHAs): coverage
  // selection reuses these same candidates to resolve a `not_required`
  // revision's basis ancestor to its actual attempt below, so that ancestor's
  // eventual failure stays actionable for every revision that reuses it.
  const discoverAncestorCandidates = adapter
    ? undefined
    : async () =>
        discoverApplicabilityCandidateRuns({
          repo,
          branch,
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
    ...(adapter ? {} : { workflow: ciWorkflowFile }),
    reason: String(reason).slice(0, 600),
  });
  const reportObservationLoss = async (reason) => {
    if (knownFailureEvent) await emit(knownFailureEvent);
    await emit(unavailable(reason));
  };

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
      const coverageResult = await observeCoverage(
        matching,
        now(),
        discoverAncestorCandidates,
      );
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
      const { event, observationError, deferredFailureEvent } =
        await acquireFailure(actionableWithAncestors, observationSignal);
      if (event) {
        await emit(event);
      }
      knownFailureEvent = deferredFailureEvent;
      if (observationError) {
        consecutiveErrors += 1;
        if (consecutiveErrors === 3) throw new Error(observationError);
      } else {
        consecutiveErrors = 0;
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
