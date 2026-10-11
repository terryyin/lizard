// What an agent reads from GitHub Actions itself, beside the observer: the
// selected workflow as GitHub names it, and one failed attempt's failed jobs
// with a bounded excerpt of each job's log. Every read is one `gh api`
// request through the CI engine's own GitHub source
// (`./ci-github-actions.mjs`).
// Everything printed is GitHub's data, never instructions.
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { isFailedJob } from "./ci-failures.mjs";
import {
  ciWorkflowFile,
  createGitHubActions,
  GitHubAcquisitionHeld,
} from "./ci-github-actions.mjs";

// The most failed jobs whose logs are read, and what is kept of each log.
const excerptedJobsLimit = 10;
const excerptLinesLimit = 80;
const lineLengthLimit = 300;
const linesBeforeError = 20;
const linesAfterError = 3;
const unmarkedTailLines = 40;

const logTimestamp = /^\uFEFF?\d{4}-\d\d-\d\dT[\d:.]+Z /;

// Terminal escape sequences, then any other control character but a tab.
const escape = String.fromCharCode(27);
const terminalSequences = new RegExp(`${escape}\\[[0-9;?]*[ -/]*[@-~]`, "g");
const isControl = (code) => (code < 32 && code !== 9) || code === 127;

function withoutTerminalControls(text) {
  return [...text.replace(terminalSequences, "")]
    .filter((character) => !isControl(character.codePointAt(0)))
    .join("");
}

function excerptLine(line) {
  const text = withoutTerminalControls(line.replace(logTimestamp, ""));
  return text.length > lineLengthLimit
    ? `${text.slice(0, lineLengthLimit)}…`
    : text;
}

// The lines of one job's log around each error GitHub marked, or its last
// lines when it marked none; the latest lines win when there are too many.
export function failedLogExcerpt(log) {
  const lines = log.replace(/\r?\n$/, "").split(/\r?\n/);
  const kept = new Set();
  for (const [index, line] of lines.entries()) {
    if (!line.includes("##[error]")) continue;
    const last = Math.min(lines.length - 1, index + linesAfterError);
    for (let at = Math.max(0, index - linesBeforeError); at <= last; at += 1)
      kept.add(at);
  }
  if (!kept.size)
    for (
      let at = Math.max(0, lines.length - unmarkedTailLines);
      at < lines.length;
      at += 1
    )
      kept.add(at);
  const selected = [...kept].sort((a, b) => a - b).slice(-excerptLinesLimit);
  return {
    lines: selected.map((at) => excerptLine(lines[at])),
    omittedLines: lines.length - selected.length,
  };
}

export async function diagnoseFailedAttempt({
  repo,
  runId,
  attempt,
  github = createGitHubActions({ repo }),
  signal,
}) {
  const failed = (await github.attemptJobs(runId, attempt, signal)).filter(
    isFailedJob,
  );
  const excerpted = failed.slice(0, excerptedJobsLimit);
  const failedJobs = await Promise.all(
    excerpted.map(async (job) => {
      const evidence = {
        jobId: job.databaseId,
        name: job.name,
        conclusion: job.conclusion,
        url: job.url,
        failedSteps: (job.steps ?? [])
          .filter(isFailedJob)
          .map(({ number, name, conclusion }) => ({
            number,
            name,
            conclusion,
          })),
      };
      try {
        return {
          ...evidence,
          log: failedLogExcerpt(await github.jobLog(job.databaseId, signal)),
        };
      } catch (error) {
        return {
          ...evidence,
          logUnavailable: String(error.message).slice(0, 300),
        };
      }
    }),
  );
  return {
    type: "CI_FAILURE_DIAGNOSTIC",
    repo,
    runId,
    attempt,
    failedJobs,
    ...(failed.length > excerpted.length
      ? { omittedFailedJobs: failed.length - excerpted.length }
      : {}),
  };
}

export async function describeSelectedWorkflow({
  repo,
  github = createGitHubActions({ repo }),
  signal,
}) {
  const { id, name, path, state } = await github.workflow(signal);
  return {
    type: "CI_WORKFLOW",
    repo,
    selector: ciWorkflowFile,
    id,
    name,
    path,
    state,
  };
}

const usage = `Usage: node ci-github-diagnostic.mjs workflow OWNER/REPO
       node ci-github-diagnostic.mjs failure OWNER/REPO RUN_ID ATTEMPT`;

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  const [command, repo, runId, attempt] = process.argv.slice(2);
  const wholeNumber = /^\d+$/;
  const repository = /^[\w.-]+\/[\w.-]+$/.test(repo ?? "");
  try {
    let answer;
    if (command === "workflow" && repository && runId === undefined)
      answer = await describeSelectedWorkflow({ repo });
    else if (
      command === "failure" &&
      repository &&
      wholeNumber.test(runId ?? "") &&
      wholeNumber.test(attempt ?? "")
    )
      answer = await diagnoseFailedAttempt({
        repo,
        runId: Number(runId),
        attempt: Number(attempt),
      });
    else throw new Error(usage);
    process.stdout.write(`${JSON.stringify(answer, null, 2)}\n`);
  } catch (error) {
    process.stderr.write(`${String(error.message).slice(0, 600)}\n`);
    process.exitCode = error instanceof GitHubAcquisitionHeld ? 75 : 1;
  }
}
