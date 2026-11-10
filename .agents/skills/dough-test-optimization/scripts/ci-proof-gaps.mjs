// The gaps a lead reading (./ci-proof-leads.mjs) states in its evidence and
// how to close them: a summary the project retains elsewhere, read from a
// file and checked (./ci-proof-check.mjs); why CI could not answer, and how
// to rank anyway;
// why a read gave no usable summary; how the attempt's revision relates to
// the requested one by local Git (./ci-proof-revisions.mjs); and the gaps of
// the attempt chosen (./ci-proof-attempts.mjs).
import { readFileSync, statSync } from "node:fs";
import { defaultAdapterTimeoutMs } from "../../dough-execute-plan/scripts/ci-command-run.mjs";
import {
  GitHubAcquisitionFailed,
  GitHubAcquisitionHeld,
} from "../../dough-execute-plan/scripts/ci-github-actions.mjs";
import { askedOf, identityOf } from "./ci-proof-attempts.mjs";
import { checkedRetainedSummary, checkedSummary } from "./ci-proof-check.mjs";
import {
  SUMMARY_READ_LIMIT_BYTES,
  summaryArtifactName,
} from "./ci-proof-contract.mjs";
import { changedPaths, commitsBetween } from "./ci-proof-revisions.mjs";

/**
 * The summary in FILE, within the summary read limit, checked as ATTEMPT's
 * in REPOSITORY, or by REPOSITORY alone when CI named no attempt; otherwise
 * why it cannot be used.
 * @param {string} file
 * @param {string} repository
 * @param {import("./ci-proof-attempts.mjs").ListedAttempt} [attempt]
 */
export function fileSummary(file, repository, attempt) {
  let text;
  try {
    if (statSync(file).size > SUMMARY_READ_LIMIT_BYTES)
      return { why: "too-large", limitBytes: SUMMARY_READ_LIMIT_BYTES };
    text = readFileSync(file, "utf8");
  } catch (error) {
    return { why: "unreadable", problem: error.message };
  }
  let value;
  try {
    value = JSON.parse(text);
  } catch {
    return { why: "not-json" };
  }
  return attempt
    ? checkedSummary(value, askedOf(repository, attempt))
    : checkedRetainedSummary(value, repository);
}

/**
 * The attempt SUMMARY names, unconfirmed by CI.
 * @param {Record<string, any>} summary
 * @returns {import("./ci-proof-attempts.mjs").ListedAttempt}
 */
export const namedBy = (summary) => ({
  runId: summary.runId ?? summary.attemptId,
  ...(summary.attemptId === undefined
    ? { runAttempt: summary.runAttempt }
    : { attemptId: summary.attemptId }),
  revision: summary.revision.toLowerCase(),
  status: "unconfirmed",
  conclusion: null,
});

/**
 * Why READ, of ATTEMPT when one was named, gave no usable summary.
 * @param {Record<string, any>} read
 * @param {import("./ci-proof-attempts.mjs").ListedAttempt} [attempt]
 */
export function refusalText(read, attempt) {
  const asked = attempt
    ? `${identityOf(attempt)} at ${attempt.revision}`
    : "this project's";
  const reasons = {
    "no-artifact": () => "the run kept no summary artifact for this attempt",
    expired: () => "the attempt's summary artifact has expired",
    "several-artifacts": () =>
      "the run keeps several summary artifacts for this attempt, so its proof is ambiguous",
    "unreadable-listing": () => "GitHub's artifact listing could not be read",
    "too-large": () =>
      `the summary is larger than the ${read.limitBytes} bytes a reading reads`,
    "unopenable-archive": () =>
      `the summary artifact could not be opened: ${read.problem}`,
    "not-json": () => "the summary is not JSON",
    unreadable: () => `the summary file could not be read: ${read.problem}`,
    invalid: () =>
      `the summary is not valid by the CI proof contract: ${read.problems.slice(0, 3).join("; ")}`,
    mismatched: () =>
      `refused: the summary names ${read.names} ${JSON.stringify(read.value)}, not ${asked}`,
    "adapter-unavailable": () =>
      `the CI adapter has no proof for this attempt: ${read.unavailable}`,
  };
  return reasons[read.why]();
}

/**
 * How REVISION, the attempt's, relates to REQUESTED as ANCESTRY judges it,
 * and the paths under PATHS that changed between them when Git has both.
 * @param {string} root
 * @param {string} revision
 * @param {string} requested
 * @param {(revision: string) => Promise<string>} ancestry
 * @param {readonly string[]} paths
 */
export async function compatibilityOf(
  root,
  revision,
  requested,
  ancestry,
  paths,
) {
  const relation = await ancestry(revision);
  if (relation === "requested")
    return { text: "the attempt tested the requested revision", changed: [] };
  if (relation === "unknown")
    return {
      text: "local Git does not have the attempt's revision; fetch it to compare in-scope paths",
      changed: [],
    };
  const changed = await changedPaths(root, revision, requested, paths);
  const counted = `${changed.length || "no"} in-scope path${changed.length === 1 ? "" : "s"} changed`;
  if (relation === "other")
    return {
      text: `the attempt's revision is not an ancestor of the requested revision; ${counted} between them`,
      changed,
    };
  const commits = await commitsBetween(root, revision, requested);
  return {
    text: `earlier: the attempt's revision is an ancestor of the requested one, ${commits} commit${commits === 1 ? "" : "s"} before it; ${counted} since`,
    changed,
  };
}

/**
 * The gaps of a choice (./ci-proof-attempts.mjs): the attempt read did not
 * succeed, the requested revision's own attempts were passed over, or
 * completed attempts that could be read instead name revisions local Git
 * does not have.
 * @param {{
 *   chosen?: import("./ci-proof-attempts.mjs").ListedAttempt;
 *   passedOver: readonly import("./ci-proof-attempts.mjs").ListedAttempt[];
 *   unknown: number;
 * }} choice
 */
export function choiceGaps({ chosen, passedOver, unknown }) {
  const gaps = [];
  if (chosen && chosen.conclusion !== "success")
    gaps.push(
      `the attempt read ended ${chosen.conclusion}: its proof may be partial; its failed, missing and untimed results are listed here`,
    );
  for (const attempt of passedOver.slice(0, 3))
    gaps.push(
      attempt.status === "completed"
        ? `the requested revision's ${identityOf(attempt)} ended ${attempt.conclusion}; its proof is not read`
        : `the requested revision's ${identityOf(attempt)} has not completed (${attempt.status})`,
    );
  const one = unknown === 1;
  if (unknown > 0)
    gaps.push(
      `${unknown} completed attempt${one ? "" : "s"} that could be read instead name${one ? "s a revision" : " revisions"} local Git does not have; fetch and rerun to consider ${one ? "it" : "them"}`,
    );
  return gaps;
}

/**
 * Why CI could not answer, from what a list or read threw: GitHub's own
 * refusal, failure or directed wait, or how the CI adapter ended.
 * @param {unknown} error
 */
export function failureText(error) {
  if (
    error instanceof GitHubAcquisitionFailed ||
    error instanceof GitHubAcquisitionHeld
  )
    return error.message;
  if (!(error instanceof Error)) return String(error);
  const { code, killed, stderr } = /** @type {any} */ (error);
  if (killed === true)
    return `it gave no answer within ${defaultAdapterTimeoutMs / 1000} s`;
  if (typeof code !== "number") return error.message;
  const line = String(stderr ?? "")
    .split("\n")
    .map((text) => text.trim())
    .filter((text) => text !== "")
    .at(-1);
  return `it exited with status ${code}${line === undefined ? "" : ` (${line})`}`;
}

/**
 * How to rank anyway when CI cannot give ATTEMPT's summary, or any when
 * ATTEMPT is undefined: a summary this project retains elsewhere, passed
 * with --summary.
 * @param {import("./ci-proof-attempts.mjs").ProjectCi} ci
 * @param {import("./ci-proof-attempts.mjs").ListedAttempt} [attempt]
 */
export function summaryRemedy(ci, attempt) {
  if (ci.adapter !== undefined)
    return `save this project's retained full summary ${attempt ? `for ${identityOf(attempt)}` : "of an attempt at or before the requested revision"} to a file and rerun with --summary <file>`;
  const page = `https://github.com/${ci.repository}/actions/runs/${attempt ? String(attempt.runId) : "<run-id>"}`;
  const artifact = attempt
    ? summaryArtifactName(attempt.runAttempt)
    : "ci-proof-summary-attempt-<n>";
  return `${attempt ? "" : `choose a successful push run of ${ci.workflow} at or before the requested revision, then `}download its ${artifact} artifact another way, such as from ${page}, and rerun with --summary <its ci-proof-summary.json>`;
}
