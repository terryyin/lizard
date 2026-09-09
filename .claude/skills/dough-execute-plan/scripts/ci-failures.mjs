import { ciWorkflowFile } from "./ci-runs.mjs";

export const ciAttemptKey = (runId, attempt) => `${runId}:${attempt}`;

const ciRunFallbackKey = (runId, attempt) =>
  `${ciAttemptKey(runId, attempt)}:run`;

const ciJobKey = (runId, attempt, job) =>
  `${ciAttemptKey(runId, attempt)}:job:${job.jobId ?? job.name}`;

export function reportedFailureEvidence(event) {
  const evidence = [];
  for (const failure of [event, ...(event.relatedFailures ?? [])]) {
    if (failure.failedJobs?.length) {
      for (const job of failure.failedJobs)
        evidence.push(ciJobKey(failure.runId, failure.attempt, job));
    } else {
      evidence.push(ciRunFallbackKey(failure.runId, failure.attempt));
    }
  }
  return evidence;
}

export async function inspectRunsForFailure({
  repo,
  runs,
  signal,
  gh,
  priorAttempts,
  reportedFailures = new Set(),
  completedAttempts = new Set(),
}) {
  const attempts = [...runs];
  let historyError;
  let observationError;
  history: for (const run of runs) {
    for (let attempt = 1; attempt < run.attempt; attempt += 1) {
      const key = ciAttemptKey(run.databaseId, attempt);
      if (!priorAttempts.has(key)) {
        try {
          const prior = await gh(
            [
              "run",
              "view",
              String(run.databaseId),
              "--repo",
              repo,
              "--attempt",
              String(attempt),
              "--json",
              "status,conclusion",
            ],
            signal,
          );
          priorAttempts.set(key, { ...run, ...prior, attempt });
        } catch (error) {
          historyError = `Could not inspect earlier CI attempts: ${String(error.message).slice(0, 600)}`;
          observationError = historyError;
          break history;
        }
      }
      attempts.push(priorAttempts.get(key));
    }
  }
  const failures = attempts.filter(
    (run) =>
      run.status === "completed" &&
      [
        "failure",
        "timed_out",
        "startup_failure",
        "action_required",
        "stale",
      ].includes(run.conclusion) &&
      !completedAttempts.has(ciAttemptKey(run.databaseId, run.attempt)),
  );
  for (const [index, failed] of failures.entries()) {
    let jobs;
    try {
      ({ jobs } = await gh(
        [
          "run",
          "view",
          String(failed.databaseId),
          "--repo",
          repo,
          "--attempt",
          String(failed.attempt),
          "--json",
          "jobs",
        ],
        signal,
      ));
      completedAttempts.add(ciAttemptKey(failed.databaseId, failed.attempt));
    } catch (error) {
      observationError = `Could not inspect CI jobs: ${String(error.message).slice(0, 600)}`;
      if (
        reportedFailures.has(
          ciRunFallbackKey(failed.databaseId, failed.attempt),
        )
      )
        continue;
      return {
        event: failureEvent({
          repo,
          run: failed,
          detailsUnavailable: true,
          historyUnavailable: historyError,
        }),
        observationError,
      };
    }
    const failedJobs = jobs.filter(isFailedJob).map(failedJobEvidence);
    const newFailedJobs = failedJobs.filter(
      (job) =>
        !reportedFailures.has(ciJobKey(failed.databaseId, failed.attempt, job)),
    );
    if (!newFailedJobs.length && failedJobs.length) continue;
    if (
      !newFailedJobs.length &&
      reportedFailures.has(ciRunFallbackKey(failed.databaseId, failed.attempt))
    )
      continue;

    const event = failureEvent({
      repo,
      run: failed,
      failedJobs: newFailedJobs,
    });
    if (historyError) event.historyUnavailable = historyError;
    const relatedFailures = failures
      .slice(index + 1)
      .filter(
        (run) =>
          !reportedFailures.has(ciRunFallbackKey(run.databaseId, run.attempt)),
      );
    if (relatedFailures.length) {
      event.relatedFailures = relatedFailures.map((run) => ({
        runId: run.databaseId,
        attempt: run.attempt,
        conclusion: run.conclusion,
        url: run.url,
      }));
    }
    return { event, observationError };
  }

  for (const run of attempts.filter(
    (attempt) => attempt.status !== "completed",
  )) {
    let jobs;
    try {
      ({ jobs } = await gh(
        [
          "run",
          "view",
          String(run.databaseId),
          "--repo",
          repo,
          "--attempt",
          String(run.attempt),
          "--json",
          "jobs",
        ],
        signal,
      ));
    } catch (error) {
      observationError ??= `Could not inspect CI jobs: ${String(error.message).slice(0, 600)}`;
      continue;
    }
    const failedJobs = jobs
      .filter(isFailedJob)
      .map(failedJobEvidence)
      .filter(
        (job) =>
          !reportedFailures.has(ciJobKey(run.databaseId, run.attempt, job)),
      );
    if (failedJobs.length) {
      return {
        event: failureEvent({
          repo,
          run,
          failedJobs,
          historyUnavailable: historyError,
        }),
      };
    }
  }

  return { historyError, observationError };
}

function failureEvent({
  repo,
  run,
  failedJobs,
  detailsUnavailable,
  historyUnavailable,
}) {
  const event = {
    type: "CI_FAILURE",
    repo,
    sha: run.headSha,
    branch: run.headBranch,
    workflow: ciWorkflowFile,
    runId: run.databaseId,
    attempt: run.attempt,
    conclusion: run.conclusion,
    url: run.url,
  };
  if (failedJobs) event.failedJobs = failedJobs;
  if (detailsUnavailable) event.detailsUnavailable = true;
  if (historyUnavailable) event.historyUnavailable = historyUnavailable;
  return event;
}

function isFailedJob(job) {
  return (
    job.conclusion &&
    !["success", "skipped", "neutral", "cancelled"].includes(job.conclusion)
  );
}

function failedJobEvidence(job) {
  return {
    ...(job.databaseId === undefined ? {} : { jobId: job.databaseId }),
    name: job.name,
    conclusion: job.conclusion,
  };
}
