// Selected-attempt dashboard operations share the adapter command transport.
import { runAdapterForAttempt, truncateUtf8 } from "./ci-command-run.mjs";
import { isFullGitRevision } from "./ci-revisions.mjs";
const unavailableReasonBytes = 600;
const isObject = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);

// The dashboard reads proof or jobs only for a selected attempt. This
// boundary checks the response envelope; the CI proof read checks a proof's
// contents (`../../dough-test-optimization/scripts/ci-proof-read.mjs`).
export async function readCommandProof(command, check, attempt, options) {
  const response = await runAdapterForAttempt(
    command,
    "proof",
    check,
    attempt,
    options,
  );
  const { summary, unavailable } = isObject(response) ? response : {};
  if (isObject(summary) && unavailable === undefined) return { summary };
  if (typeof unavailable === "string" && unavailable && summary === undefined)
    return {
      unavailable: truncateUtf8(unavailable, unavailableReasonBytes).text,
    };
  throw new Error("CI adapter proof must return summary or unavailable");
}

export async function readCommandJobs(command, check, attempt, options) {
  const response = await runAdapterForAttempt(
    command,
    "jobs",
    check,
    attempt,
    options,
  );
  const { jobs, unavailable } = isObject(response) ? response : {};
  if (Array.isArray(jobs) && unavailable === undefined) return { jobs };
  if (typeof unavailable === "string" && unavailable && jobs === undefined)
    return {
      unavailable: truncateUtf8(unavailable, unavailableReasonBytes).text,
    };
  throw new Error("CI adapter jobs must return jobs or unavailable");
}

/** @typedef {{runId: string | number, attemptId: string | number, sha: string}} RebuildAttempt */
/** @returns {Promise<{result: "accepted", attempt: RebuildAttempt} | {result: "refused" | "uncertain", reason: string}>} */
export async function requestCommandRebuild(command, check, attempt, options) {
  try {
    const response = await runAdapterForAttempt(
      command,
      "rebuild",
      check,
      attempt,
      options,
    );
    if (isObject(response)) {
      if (
        response.result === "accepted" &&
        response.reason === undefined &&
        validAttempt(response.attempt)
      )
        return {
          result: "accepted",
          attempt: {
            runId: response.attempt.runId,
            attemptId: response.attempt.attemptId,
            sha: response.attempt.sha,
          },
        };
      if (
        ["refused", "uncertain"].includes(response.result) &&
        response.attempt === undefined &&
        typeof response.reason === "string" &&
        response.reason.trim()
      )
        return {
          result: response.result,
          reason: truncateUtf8(response.reason, unavailableReasonBytes).text,
        };
    }
    throw new Error(
      "CI rebuild must return accepted with an attempt, refused with a reason, or uncertain with a reason",
    );
  } catch (error) {
    return {
      result: "uncertain",
      reason: truncateUtf8(
        error instanceof Error ? error.message : String(error),
        unavailableReasonBytes,
      ).text,
    };
  }
}
/** @param {unknown} attempt @returns {attempt is RebuildAttempt} */
function validAttempt(attempt) {
  const identity = (value) =>
    (typeof value === "string" && value.length > 0) ||
    (typeof value === "number" && Number.isFinite(value));
  return (
    isObject(attempt) &&
    identity(attempt.runId) &&
    identity(attempt.attemptId) &&
    isFullGitRevision(attempt.sha)
  );
}
