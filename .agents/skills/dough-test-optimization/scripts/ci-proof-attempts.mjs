// The attempt whose retained CI proof a lead reading uses
// (./ci-proof-leads.mjs), as the project's configured CI lists it: the
// selected GitHub Actions workflow's newest push runs on every branch, each
// at its latest attempt, or every attempt the CI adapter's `discover`
// answers, whatever branch labels it. The newest complete, successful
// attempt at the requested revision is preferred, then the newest at an
// ancestor by local Git, then the newest completed one that did not
// succeed, at the requested revision and then at an ancestor. Its summary is
// read through the shared CI proof read (./ci-proof-read.mjs).
import { createCommandAcquisition } from "../../dough-execute-plan/scripts/ci-command-adapter.mjs";
import { createGitHubActions } from "../../dough-execute-plan/scripts/ci-github-actions.mjs";
import {
  readCiAdapterSummary,
  readGitHubActionsSummary,
} from "./ci-proof-read.mjs";

// How many of the newest attempts a reading lists: GitHub's largest page.
const LISTED_ATTEMPTS = 100;

/**
 * The project's configured CI as a lead reading asks it: its repository and
 * either the GitHub Actions `workflow`, asked through `gitHub`, or the CI
 * `adapter` command, whose `discover` lists `branch` and which runs in
 * `root`.
 * @typedef {{
 *   readonly repository: string;
 *   readonly workflow?: string;
 *   readonly gitHub?: import("./ci-proof-read.mjs").GitHubAsks;
 *   readonly adapter?: readonly string[];
 *   readonly branch?: string;
 *   readonly root?: string;
 * }} ProjectCi
 */

/**
 * One listed attempt: a GitHub Actions run's `runId` and `runAttempt`, or a
 * CI adapter's text `runId` and `attemptId` with the branch whose discover
 * listed it; the revision it tested, lower case; and how it ended.
 * @typedef {{
 *   readonly runId: number | string;
 *   readonly runAttempt?: number;
 *   readonly attemptId?: string;
 *   readonly branch?: string;
 *   readonly revision: string;
 *   readonly status: string;
 *   readonly conclusion: string | null;
 * }} ListedAttempt
 */

/**
 * CI's newest attempts, newest first. A failure to ask is thrown.
 * @param {ProjectCi} ci
 * @param {AbortSignal} signal
 * @returns {Promise<ListedAttempt[]>}
 */
export async function listedAttempts(ci, signal) {
  if (ci.adapter === undefined) {
    const runs = await createGitHubActions({
      repo: ci.repository,
      gh: ci.gitHub.readGitHubActions,
      workflow: ci.workflow,
    }).listRuns({ limit: LISTED_ATTEMPTS }, signal);
    return runs.map((run) => ({
      runId: run.databaseId,
      runAttempt: run.attempt,
      revision: String(run.headSha).toLowerCase(),
      status: run.status,
      conclusion: run.conclusion ?? null,
    }));
  }
  const acquisition = createCommandAcquisition({
    command: ci.adapter,
    repo: ci.repository,
    branch: ci.branch,
    root: ci.root,
    includeOtherBranches: true,
  });
  await acquisition.acquireRuns(signal);
  return (await acquisition.discoverAnsweredAttempts()).map((run) => ({
    runId: String(run.databaseId),
    attemptId: String(run.attempt),
    branch: ci.branch,
    revision: run.headSha.toLowerCase(),
    status: run.status,
    conclusion: run.conclusion ?? null,
  }));
}

const isCompleted = (attempt) => attempt.status === "completed";
const succeeded = (attempt) =>
  isCompleted(attempt) && attempt.conclusion === "success";
const failedOrIncomplete = (attempt) =>
  isCompleted(attempt) && !succeeded(attempt);

/**
 * The attempt to read among ATTEMPTS, newest first, for REQUESTED, as
 * ANCESTRY (./ci-proof-revisions.mjs) relates each revision to it: the
 * newest successful attempt at REQUESTED, else the newest at an ancestor,
 * else the newest completed one that did not succeed, at REQUESTED and then
 * at an ancestor. With it come `passedOver`, the requested revision's other
 * attempts that did not succeed or have not completed, and `unknown`, how
 * many completed attempts name a revision local Git does not have and would
 * be read instead were a fetch to show that revision an ancestor: none when
 * the one chosen succeeded at REQUESTED; the successful ones listed before
 * it when it succeeded at an ancestor; every successful one, of any age,
 * when the one chosen did not succeed, with the unsuccessful ones listed
 * before it when it is at an ancestor; and every one when none is chosen.
 * @param {readonly ListedAttempt[]} attempts
 * @param {string} requested
 * @param {(revision: string) => Promise<string>} ancestry
 */
export async function chosenAttempt(attempts, requested, ancestry) {
  const passedOver = (chosen) =>
    attempts.filter(
      (attempt) =>
        attempt !== chosen &&
        attempt.revision === requested &&
        !succeeded(attempt),
    );
  // The newest of CANDIDATES at REQUESTED, else the newest at an ancestor.
  const newestOf = async (candidates) => {
    const exact = candidates.find((attempt) => attempt.revision === requested);
    if (exact) return exact;
    for (const attempt of candidates)
      if ((await ancestry(attempt.revision)) === "ancestor") return attempt;
    return undefined;
  };
  // How many attempts that ENDED so, listed before BEFORE when it is given,
  // name a revision local Git does not have.
  const unknownAmong = async (ended, before) => {
    const listed = before
      ? attempts.slice(0, attempts.indexOf(before))
      : attempts;
    let unknown = 0;
    for (const attempt of listed.filter(ended))
      if ((await ancestry(attempt.revision)) === "unknown") unknown += 1;
    return unknown;
  };
  const success = await newestOf(attempts.filter(succeeded));
  if (success)
    return {
      chosen: success,
      passedOver: passedOver(success),
      unknown:
        success.revision === requested
          ? 0
          : await unknownAmong(succeeded, success),
    };
  const fallback = await newestOf(attempts.filter(failedOrIncomplete));
  return {
    chosen: fallback,
    passedOver: passedOver(fallback),
    unknown:
      (await unknownAmong(succeeded)) +
      (fallback?.revision === requested
        ? 0
        : await unknownAmong(failedOrIncomplete, fallback)),
  };
}

/** @param {ListedAttempt} attempt */
export const identityOf = (attempt) =>
  attempt.attemptId === undefined
    ? `run ${attempt.runId} attempt ${attempt.runAttempt}`
    : `attemptId ${attempt.attemptId}${attempt.runId === attempt.attemptId ? "" : ` (run ${attempt.runId})`}`;

/**
 * ATTEMPT as a read asks for it, which a summary must name to be ATTEMPT's
 * (./ci-proof-check.mjs).
 * @param {string} repository
 * @param {ListedAttempt} attempt
 */
export const askedOf = (repository, attempt) =>
  attempt.attemptId === undefined
    ? {
        repository,
        runId: Number(attempt.runId),
        runAttempt: Number(attempt.runAttempt),
        revision: attempt.revision,
      }
    : { repository, attemptId: attempt.attemptId, revision: attempt.revision };

/**
 * ATTEMPT's summary as CI keeps it, or why it cannot be used. A failure to
 * ask is thrown.
 * @param {ProjectCi} ci
 * @param {ListedAttempt} attempt
 * @param {AbortSignal} signal
 */
export function readAttemptSummary(ci, attempt, signal) {
  if (ci.adapter === undefined)
    return readGitHubActionsSummary(
      ci.repository,
      askedOf(ci.repository, attempt),
      signal,
      ci.gitHub,
    );
  return readCiAdapterSummary(ci.adapter, ci.repository, attempt, {
    signal,
    cwd: ci.root,
  });
}

/** @param {ProjectCi} ci */
export const ciName = (ci) =>
  ci.adapter === undefined
    ? `GitHub Actions workflow ${ci.workflow}`
    : "the CI adapter";

/**
 * Where CI's summaries come from, the adapter's command included.
 * @param {ProjectCi} ci
 */
export const ciSource = (ci) =>
  ci.adapter === undefined
    ? ciName(ci)
    : `${ciName(ci)} (${ci.adapter.join(" ")})`;
