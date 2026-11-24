// Selected-attempt dashboard operations share the adapter command transport.
import { runAdapterForAttempt, truncateUtf8 } from "./ci-command-run.mjs";
import { isFullGitRevision } from "./ci-revisions.mjs";
const unavailableReasonBytes = 600;
const isObject = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);

// The dashboard reads proof or jobs only for a selected attempt. This
// boundary checks the response envelope, the named answer or a reason it is
// unavailable; the CI proof read checks a proof's contents
// (`../../dough-test-optimization/scripts/ci-proof-read.mjs`).
async function readAttemptAnswer(operation, field, accepts, command, ...call) {
  const response = await runAdapterForAttempt(command, operation, ...call);
  const envelope = isObject(response) ? response : {};
  const { [field]: answer, unavailable } = envelope;
  if (accepts(answer) && unavailable === undefined) return { [field]: answer };
  if (typeof unavailable === "string" && unavailable && answer === undefined)
    return {
      unavailable: truncateUtf8(unavailable, unavailableReasonBytes).text,
    };
  throw new Error(
    `CI adapter ${operation} must return ${field} or unavailable`,
  );
}

/** @returns {Promise<{summary: any, unavailable?: undefined} | {summary?: undefined, unavailable: string}>} */
export const readCommandProof = (command, check, attempt, options) =>
  readAttemptAnswer(
    "proof",
    "summary",
    isObject,
    command,
    check,
    attempt,
    options,
  );

/** @returns {Promise<{jobs: any[], unavailable?: undefined} | {jobs?: undefined, unavailable: string}>} */
export const readCommandJobs = (command, check, attempt, options) =>
  readAttemptAnswer(
    "jobs",
    "jobs",
    Array.isArray,
    command,
    check,
    attempt,
    options,
  );

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
