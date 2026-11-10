// The check a CI proof read (./ci-proof-read.mjs) makes of a summary before
// using it: valid by the collector contract (./ci-proof-contract.mjs), then
// naming the asked attempt's repository, attempt and revision. Missing,
// outdated or conflicting evidence is never a result. A usable summary is
// read in the current version (the contract's currentSummaryOf: a version 1
// summary's results as legacy evidence), so every reader, the dashboard's
// and the lead reading alike, sees one version. A lead reading
// (./ci-proof-leads.mjs) checks a summary retained elsewhere the same way,
// or, when the project's CI could not name its attempt, by its repository
// alone.
import { currentSummaryOf, summaryProblems } from "./ci-proof-contract.mjs";

/**
 * The attempt a summary must name: its repository and revision, and a GitHub
 * Actions run attempt's `runId` and `runAttempt` or a CI adapter attempt's
 * `attemptId`.
 * @typedef {{ readonly repository: string; readonly revision: string } & (
 *   | { readonly runId: number; readonly runAttempt: number }
 *   | { readonly attemptId: string }
 * )} AskedAttempt
 */

/** @typedef {{ readonly summary: Record<string, unknown> }} UsableSummary */

/**
 * Why a summary that was read is not the asked attempt's: the collector
 * contract's `problems` with it, or else the first identity fact it `names`
 * otherwise, with its `value` there.
 * @typedef {(
 *   | { readonly why: "invalid"; readonly problems: readonly string[] }
 *   | {
 *       readonly why: "mismatched";
 *       readonly names: "repository" | "run" | "attempt" | "revision";
 *       readonly value: unknown;
 *     }
 * )} SummaryRefusal
 */

// SUMMARY's repository when it is not REPOSITORY, compared ignoring case.
function repositoryMismatch(summary, repository) {
  if (
    typeof summary.repository !== "string" ||
    summary.repository.toLowerCase() !== repository.toLowerCase()
  )
    return { names: "repository", value: summary.repository };
  return undefined;
}

// The first identity fact by which SUMMARY, a valid summary, is not ASKED's:
// what it `names` and its `value` there; undefined when it is ASKED's. The
// repository is compared ignoring case. A GitHub Actions run attempt is
// named by its `runId`, `runAttempt` and revision as written; a CI adapter's
// attempt by its text `attemptId` and its revision lowered, as an adapter
// may report a revision in either case, against the asked revision, which
// is lower case.
function identityMismatch(summary, asked) {
  const repository = repositoryMismatch(summary, asked.repository);
  if (repository !== undefined) return repository;
  if (asked.attemptId === undefined) {
    if (summary.runId !== asked.runId)
      return { names: "run", value: summary.runId };
    if (summary.runAttempt !== asked.runAttempt)
      return { names: "attempt", value: summary.runAttempt };
    if (summary.revision !== asked.revision)
      return { names: "revision", value: summary.revision };
    return undefined;
  }
  if (summary.attemptId === undefined)
    return { names: "run", value: summary.runId };
  if (summary.attemptId !== asked.attemptId)
    return { names: "attempt", value: summary.attemptId };
  if (summary.revision.toLowerCase() !== asked.revision)
    return { names: "revision", value: summary.revision };
  return undefined;
}

/**
 * VALUE as ASKED's summary, in the current version: valid by the collector
 * contract and naming the asked repository, attempt and revision; otherwise
 * why it is not.
 * @param {unknown} value
 * @param {AskedAttempt} asked
 * @returns {UsableSummary | SummaryRefusal}
 */
export function checkedSummary(value, asked) {
  return checked(value, (summary) => identityMismatch(summary, asked));
}

/**
 * VALUE as a summary of REPOSITORY whose attempt the project's CI could not
 * name, in the current version: valid by the collector contract and naming
 * the repository, its attempt and revision as it names them; otherwise why
 * it is not.
 * @param {unknown} value
 * @param {string} repository
 * @returns {UsableSummary | SummaryRefusal}
 */
export function checkedRetainedSummary(value, repository) {
  return checked(value, (summary) => repositoryMismatch(summary, repository));
}

function checked(value, mismatchOf) {
  const problems = summaryProblems(value);
  if (problems.length > 0) return { why: "invalid", problems };
  const mismatch = mismatchOf(value);
  if (mismatch !== undefined) return { why: "mismatched", ...mismatch };
  return { summary: currentSummaryOf(value) };
}
