// One CI proof read: one attempt's CI proof summary, acquired from the
// project's configured CI and used only when it is the asked attempt's.
// Intent's CI status reads proof through it.
//
// - A GitHub Actions run attempt's summary is the run's
//   `ci-proof-summary-attempt-<n>` artifact, listed by that name, downloaded
//   as its zip through the shared GitHub request owner
//   (../../dough-execute-plan/scripts/ci-github-actions.mjs), and its one
//   `ci-proof-summary.json` extracted within the summary read limit
//   (./ci-proof-archive.mjs).
// - A CI adapter's attempt's summary is the adapter's `proof` answer
//   (../../dough-execute-plan/scripts/ci-command-attempt.mjs), asked with
//   the summary read limit as its answer limit.
//
// Either summary is then checked (./ci-proof-check.mjs): valid by the
// collector contract and naming the asked attempt's identity, as missing,
// outdated or conflicting evidence is never a result. A read answers the
// summary, or why it cannot be used as observed, which each reader words
// itself. A failure to ask at all is thrown: GitHub refused, failed or held
// the request, or the adapter gave no answer.
//
// A project's CI never copies this module with the collector beside it: it
// asks through the installed skills.
import { readCommandProof } from "../../dough-execute-plan/scripts/ci-command-attempt.mjs";
import {
  askGitHubArchive,
  readGitHubActions,
} from "../../dough-execute-plan/scripts/ci-github-actions.mjs";
import {
  onlyZipEntry,
  ZipEntryProblem,
  ZipEntryTooLarge,
} from "./ci-proof-archive.mjs";
import { checkedSummary } from "./ci-proof-check.mjs";
import {
  SUMMARY_READ_LIMIT_BYTES,
  summaryArtifactName,
} from "./ci-proof-contract.mjs";

// The one file a GitHub Actions run attempt's summary artifact holds.
export const SUMMARY_FILE_NAME = "ci-proof-summary.json";

/** @typedef {import("./ci-proof-check.mjs").UsableSummary} UsableSummary */
/** @typedef {import("./ci-proof-check.mjs").SummaryRefusal} SummaryRefusal */

/**
 * A GitHub Actions run attempt whose proof is asked: the run's id, its
 * attempt, and the revision it tested.
 * @typedef {{
 *   readonly runId: number;
 *   readonly runAttempt: number;
 *   readonly revision: string;
 * }} RunAttempt
 */

/**
 * A CI adapter's attempt whose proof is asked: the text of its discovered
 * `runId` and `attemptId`, the branch whose discover listed it, and the
 * revision it tested.
 * @typedef {{
 *   readonly runId: string;
 *   readonly attemptId: string;
 *   readonly branch: string;
 *   readonly revision: string;
 * }} AdapterAttempt
 */

/**
 * A summary larger than the read limit, never truncated: that limit, and
 * its size when known.
 * @typedef {{
 *   readonly why: "too-large";
 *   readonly limitBytes: number;
 *   readonly sizeBytes?: number;
 * }} TooLarge
 */

/**
 * A GitHub Actions run attempt's proof read: its summary, or why it cannot
 * be used. The run lists no artifact of the attempt's name, only expired
 * ones, or more than one; GitHub's listing is in a shape that cannot be
 * read; the summary is too large; its zip cannot be opened (`problem` says
 * why); or its file is not JSON or not the asked attempt's summary.
 * @typedef {(
 *   | UsableSummary
 *   | SummaryRefusal
 *   | TooLarge
 *   | { readonly why: "no-artifact" }
 *   | { readonly why: "expired" }
 *   | { readonly why: "several-artifacts" }
 *   | { readonly why: "unreadable-listing" }
 *   | { readonly why: "unopenable-archive"; readonly problem: string }
 *   | { readonly why: "not-json" }
 * )} GitHubActionsSummaryRead
 */

/**
 * A CI adapter attempt's proof read: its summary, or why it cannot be
 * used. The adapter answered its own reason (`unavailable`), its answer is
 * too large, or its summary is not the asked attempt's.
 * @typedef {(
 *   | UsableSummary
 *   | SummaryRefusal
 *   | TooLarge
 *   | { readonly why: "adapter-unavailable"; readonly unavailable: string }
 * )} CiAdapterSummaryRead
 */

/**
 * How a read asks GitHub: the shared request owner's JSON answer and
 * archive bytes for one `gh api` request each. A test may answer them
 * itself.
 * @typedef {{
 *   readonly readGitHubActions: typeof readGitHubActions;
 *   readonly askGitHubArchive: (args: string[], signal: AbortSignal) => Promise<Buffer>;
 * }} GitHubAsks
 */

/** @type {GitHubAsks} */
const gitHub = { readGitHubActions, askGitHubArchive };

/**
 * @param {number} [sizeBytes]
 * @returns {TooLarge}
 */
const tooLarge = (sizeBytes) => ({
  why: "too-large",
  limitBytes: SUMMARY_READ_LIMIT_BYTES,
  ...(sizeBytes === undefined ? {} : { sizeBytes }),
});

// The one listed artifact of NAME for RUN that can be read, as its id, or
// why none can.
function chosenArtifact(listed, name, run) {
  const listing = listed?.artifacts ?? [];
  if (!Array.isArray(listing)) return { why: "unreadable-listing" };
  const named = listing.filter(
    (artifact) =>
      artifact.name === name &&
      (artifact.workflow_run?.id ?? run.runId) === run.runId,
  );
  if (named.length === 0) return { why: "no-artifact" };
  const kept = named.filter((artifact) => artifact.expired !== true);
  if (kept.length === 0) return { why: "expired" };
  if (kept.length > 1) return { why: "several-artifacts" };
  const [only] = kept;
  if (typeof only.id !== "number") return { why: "unreadable-listing" };
  if (
    typeof only.size_in_bytes === "number" &&
    only.size_in_bytes > SUMMARY_READ_LIMIT_BYTES
  )
    return tooLarge(only.size_in_bytes);
  return { artifactId: only.id };
}

/**
 * The proof of RUN's attempt in REPOSITORY, as GitHub Actions keeps it.
 * GitHub refusing, failing or holding a request is thrown
 * (`GitHubAcquisitionFailed`, `GitHubAcquisitionHeld`).
 * @param {string} repository
 * @param {RunAttempt} run
 * @param {AbortSignal} signal
 * @param {GitHubAsks} [asks]
 * @returns {Promise<GitHubActionsSummaryRead>}
 */
export async function readGitHubActionsSummary(
  repository,
  run,
  signal,
  asks = gitHub,
) {
  const name = summaryArtifactName(run.runAttempt);
  const listed = await asks.readGitHubActions(
    [
      "api",
      `repos/${repository}/actions/runs/${String(run.runId)}/artifacts?name=${name}&per_page=100`,
    ],
    signal,
  );
  const chosen = chosenArtifact(listed, name, run);
  if (chosen.artifactId === undefined) return chosen;
  const archive = await asks.askGitHubArchive(
    [
      "api",
      `repos/${repository}/actions/artifacts/${String(chosen.artifactId)}/zip`,
    ],
    signal,
  );
  let summary;
  try {
    summary = JSON.parse(
      onlyZipEntry(archive, SUMMARY_FILE_NAME, SUMMARY_READ_LIMIT_BYTES),
    );
  } catch (error) {
    if (error instanceof ZipEntryTooLarge) return tooLarge(error.sizeBytes);
    if (error instanceof ZipEntryProblem)
      return { why: "unopenable-archive", problem: error.message };
    return { why: "not-json" };
  }
  const { runId, runAttempt, revision } = run;
  return checkedSummary(summary, { repository, runId, runAttempt, revision });
}

const isMaxBuffer = (error) =>
  error instanceof Error && error.code === "ERR_CHILD_PROCESS_STDIO_MAXBUFFER";

/**
 * The proof of ATTEMPT in REPOSITORY as the project's CI ADAPTER command
 * answers its `proof` operation, run with OPTIONS and the summary read limit
 * as its answer limit. An answer beyond that limit is too large; any other
 * failure to answer is thrown, as the adapter run rejects.
 * @param {readonly string[]} adapter
 * @param {string} repository
 * @param {AdapterAttempt} attempt
 * @param {{ readonly signal: AbortSignal; readonly cwd: string; readonly timeoutMs?: number }} options
 * @param {typeof readCommandProof} [commandProof]
 * @returns {Promise<CiAdapterSummaryRead>}
 */
export async function readCiAdapterSummary(
  adapter,
  repository,
  attempt,
  options,
  commandProof = readCommandProof,
) {
  let answer;
  try {
    answer = await commandProof(
      adapter,
      { repo: repository, branch: attempt.branch },
      {
        runId: attempt.runId,
        attemptId: attempt.attemptId,
        sha: attempt.revision,
      },
      { ...options, answerBytes: SUMMARY_READ_LIMIT_BYTES },
    );
  } catch (error) {
    if (!options.signal.aborted && isMaxBuffer(error)) return tooLarge();
    throw error;
  }
  if (answer.summary === undefined)
    return { why: "adapter-unavailable", unavailable: answer.unavailable };
  const { attemptId, revision } = attempt;
  return checkedSummary(answer.summary, { repository, attemptId, revision });
}
