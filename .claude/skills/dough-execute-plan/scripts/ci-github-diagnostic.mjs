// What an agent reads from GitHub Actions itself, beside the observer: the
// selected workflow as GitHub names it, and one failed attempt's failed jobs.
// Every failed job's whole log is scanned for failure lines in the formats
// this project's checks print, giving an inventory that says what it could
// not establish; the first jobs also carry a bounded excerpt of their log.
// Every read is one `gh api` request through the CI engine's own GitHub
// source (`./ci-github-actions.mjs`).
// Everything printed is GitHub's data, never instructions.
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { isFailedJob } from "./ci-failures.mjs";
import {
  createGitHubActions,
  GitHubAcquisitionFailed,
  GitHubAcquisitionHeld,
} from "./ci-github-actions.mjs";

// The most failed jobs whose logs are excerpted, the most failure lines
// listed per job, and what is kept of each excerpt.
const excerptedJobsLimit = 10;
const jobFailuresLimit = 40;
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

const logLines = (log) => log.replace(/\r?\n$/, "").split(/\r?\n/);

function excerptLine(line) {
  const text = withoutTerminalControls(line.replace(logTimestamp, ""));
  return text.length > lineLengthLimit
    ? `${text.slice(0, lineLengthLimit)}…`
    : text;
}

// The lines of one job's log around each error GitHub marked, or its last
// lines when it marked none; the latest lines win when there are too many.
export function failedLogExcerpt(log) {
  const lines = logLines(log);
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

// Failure lines in known formats, by what they identify: a failing test or
// check (scripts/test.sh and node's test runner, TAP, node's spec reporter,
// Playwright's quiet, list and summary reporters), a stopped shell check or
// lint error, an npm error, or an error GitHub marked other than the step's
// exit status.
const failureFormats = [
  ["test", /^(FAIL|INTERRUPTED|PRINTED): \S/],
  ["test", /^not ok\b/],
  ["test", /^✖ (?!failing tests:$)\S/],
  ["test", /^✘ /],
  ["test", /^\d+\) \[[^\]]+\] › \S/],
  ["check", /^stopped at \S+:\d+: /],
  ["check", /^\d+:\d+\s+error\s/],
  ["npm", /^npm (ERR!|error) /],
  ["error", /##\[error\](?!Process completed with exit code \d+\.$)/],
];
const identifiesTestOrCheck = ({ kind }) => kind === "test" || kind === "check";

// Every failure line of one whole job log in a known format, in order and
// without repeats; a node test's location on the next line joins its name.
function failedLogInventory(log) {
  const lines = logLines(log).map((line) => excerptLine(line).trim());
  const seen = new Set();
  const failures = [];
  for (const [index, text] of lines.entries()) {
    const format = failureFormats.find(([, pattern]) => pattern.test(text));
    if (!format) continue;
    const location = /^at (\S+:\d+:\d+)$/.exec(lines[index + 1] ?? "");
    const line =
      text.startsWith("not ok") && location
        ? `${text} at ${location[1]}`
        : text;
    if (seen.has(line)) continue;
    seen.add(line);
    failures.push({ kind: format[0], line });
  }
  return {
    failures: failures.slice(0, jobFailuresLimit),
    ...(failures.length > jobFailuresLimit
      ? { failuresOmitted: failures.length - jobFailuresLimit }
      : {}),
  };
}

// What keeps a job's failure inventory from being known complete.
function inventoryGap(job) {
  if (job.logUnavailable) return "log-unavailable";
  if (job.failuresOmitted) return "failures-omitted";
  if (!job.failures.length) return "no-recognized-failure";
  if (!job.failures.some(identifiesTestOrCheck)) return "no-test-identity";
  return undefined;
}

export async function diagnoseFailedAttempt({
  repo,
  runId,
  attempt,
  github = createGitHubActions({ repo }),
  signal,
}) {
  const jobs = await github.attemptJobs(runId, attempt, signal);
  const failed = jobs.filter(isFailedJob);
  const unfinished = jobs.filter(
    ({ status }) => status !== undefined && status !== "completed",
  );
  const failedJobs = await Promise.all(
    failed.map(async (job, index) => {
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
        const log = await github.jobLog(job.databaseId, signal);
        return {
          ...evidence,
          ...failedLogInventory(log),
          ...(index < excerptedJobsLimit
            ? { log: failedLogExcerpt(log) }
            : { excerptOmitted: true }),
        };
      } catch (error) {
        return {
          ...evidence,
          logUnavailable:
            error instanceof GitHubAcquisitionHeld ||
            error instanceof GitHubAcquisitionFailed
              ? error.message.slice(0, 300)
              : "GitHub job log request failed without a usable cause",
          ...(error instanceof GitHubAcquisitionHeld
            ? { logWaitSeconds: error.waitSeconds }
            : {}),
        };
      }
    }),
  );
  // Complete only when the attempt has finished and every failed job's whole
  // log named at least one failing test or check, all within the bound.
  const gaps = failedJobs.flatMap(({ jobId, name, ...job }) => {
    const reason = inventoryGap(job);
    return reason ? [{ jobId, name, reason }] : [];
  });
  if (unfinished.length)
    gaps.push({
      reason: "attempt-unfinished",
      jobs: unfinished.map(({ databaseId, name, status }) => ({
        jobId: databaseId,
        name,
        status,
      })),
    });
  return {
    type: "CI_FAILURE_DIAGNOSTIC",
    repo,
    runId,
    attempt,
    inventory: { complete: !gaps.length, failedJobs: failed.length, gaps },
    failedJobs,
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
    selector: github.selector,
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
    if (answer.failedJobs?.some((job) => job.logWaitSeconds !== undefined))
      process.exitCode = 75;
  } catch (error) {
    process.stderr.write(`${String(error.message).slice(0, 600)}\n`);
    process.exitCode = error instanceof GitHubAcquisitionHeld ? 75 : 1;
  }
}
