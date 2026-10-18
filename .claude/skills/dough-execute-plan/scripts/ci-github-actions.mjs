// GitHub Actions as the CI engine reads it: the selected workflow's runs,
// attempts, jobs and job logs, each one `gh api` request through the one
// request source (`./github-requests.mjs`), whose owner this process shares
// with every dashboard and CI observer of this user.
import { GitHubRequests } from "./github-requests.mjs";
import { GhFailure } from "./github-request-answer.mjs";
import { ownerFailureText } from "./github-request-owner-failure.mjs";

// Workflow identity is the selector every listing requests; the display name
// is not a second, independent filter (`./ci-runs.mjs` checks a supplied one).
export const ciWorkflowFile = process.env.DOUGH_CI_WORKFLOW ?? "ci.yml";

// Every request this process asks of GitHub for CI evidence: one `gh api`
// call each, sharing the owner's turns at GitHub and any wait GitHub
// directed. One call may run 20 seconds; a failed job's log may print up to
// 16 MiB.
const requests = new GitHubRequests({
  boundMs: 20_000,
  backoffBaseMs: 60_000,
  maxBufferBytes: 16 * 1024 * 1024,
});

// GitHub directed a wait, or one it directed earlier still stands: nothing
// more is asked of GitHub for `waitSeconds`. Not a failed observation.
export class GitHubAcquisitionHeld extends Error {
  constructor(waitSeconds) {
    super(`GitHub directed a wait of ${waitSeconds} s before the next request`);
    this.waitSeconds = waitSeconds;
  }
}

function failureText(failure) {
  if (failure.kind === "owner-unavailable")
    return ownerFailureText(failure.cause);
  if (failure.kind === "http") return `HTTP ${failure.status}`;
  return (
    {
      "not-logged-in": "gh is not logged in",
      "not-installed": "gh is not installed",
      unreachable: "GitHub is unreachable",
      "output-bound": "gh output exceeded the request's size limit",
      "timed-out": "gh did not answer within the request's time limit",
    }[failure.kind] ?? "gh failed"
  );
}

// A failed acquisition whose message contains only its route and safe cause.
export class GitHubAcquisitionFailed extends Error {
  constructor(route, failure) {
    super(`GitHub request ${route} failed: ${failureText(failure)}`);
  }
}

// The body of one `gh api` answer, for a fixed argument array starting with
// `api`. A wait GitHub directed is a `GitHubAcquisitionHeld`.
export async function askGitHub(args, signal = new AbortController().signal) {
  let answer;
  try {
    answer = await requests.request(args, signal);
  } catch (error) {
    if (error instanceof GhFailure)
      throw new GitHubAcquisitionFailed(args[1], error.reason);
    throw error;
  }
  const { failure, body } = answer;
  if (failure === undefined) return body;
  if (failure.kind === "rate-limited" || failure.kind === "held-back")
    throw new GitHubAcquisitionHeld(failure.waitSeconds);
  throw new GitHubAcquisitionFailed(args[1], failure);
}

export async function readGitHubActions(args, signal) {
  return JSON.parse(await askGitHub(args, signal));
}

const defined = (fields) =>
  Object.fromEntries(
    Object.entries(fields).filter(([, value]) => value !== undefined),
  );

const observedRun = (run, workflowName) =>
  defined({
    databaseId: run.id,
    attempt: run.run_attempt,
    headSha: run.head_sha,
    headBranch: run.head_branch,
    workflowName,
    event: run.event,
    status: run.status,
    conclusion: run.conclusion,
    url: run.html_url,
    createdAt: run.created_at,
  });

const jobsPageSize = 100;

// GitHub Actions evidence for one repository's selected workflow, each read
// one `gh api` request, asked through `gh` when its answer is JSON: the
// workflow is resolved once, a run listing is one page of at most 100 runs,
// and an attempt's jobs are read a page at a time until GitHub has no more.
export function createGitHubActions({ repo, gh = readGitHubActions }) {
  const api = (route) => ["api", `repos/${repo}/actions/${route}`];
  let workflow;
  const selectedWorkflow = async (signal) => {
    workflow ??= await gh(
      api(`workflows/${encodeURIComponent(ciWorkflowFile)}`),
      signal,
    );
    return workflow;
  };
  // The run as its latest attempt reports it, with the workflow that ran it.
  const currentRun = async (runId, signal) => {
    const run = await gh(api(`runs/${runId}`), signal);
    return defined({ ...observedRun(run), workflowId: run.workflow_id });
  };

  return {
    // The selected workflow as GitHub names it.
    workflow: selectedWorkflow,

    async listRuns({ branch, created, limit = 20 }, signal) {
      const { id, name } = await selectedWorkflow(signal);
      const query = new URLSearchParams({
        branch,
        event: "push",
        ...(created ? { created } : {}),
        per_page: String(limit),
        page: "1",
      });
      const { workflow_runs: runs = [] } = await gh(
        api(`workflows/${id}/runs?${query}`),
        signal,
      );
      return runs.slice(0, limit).map((run) => observedRun(run, name));
    },

    // The run's latest attempt.
    async viewRun(runId, signal) {
      const { attempt, status, conclusion, url } = await currentRun(
        runId,
        signal,
      );
      return defined({ attempt, status, conclusion, url });
    },

    // For judging one candidate's current evidence.
    viewCurrentRun: currentRun,

    async viewAttempt(runId, attempt, signal) {
      const { status, conclusion } = await gh(
        api(`runs/${runId}/attempts/${attempt}`),
        signal,
      );
      return defined({ status, conclusion });
    },

    async attemptJobs(runId, attempt, signal) {
      const jobs = [];
      for (let page = 1; ; page += 1) {
        const { jobs: listed = [], total_count: total = 0 } = await gh(
          api(
            `runs/${runId}/attempts/${attempt}/jobs?per_page=${jobsPageSize}&page=${page}`,
          ),
          signal,
        );
        jobs.push(...listed);
        if (listed.length < jobsPageSize || jobs.length >= total) break;
      }
      return jobs.map(({ id, html_url: url, ...job }) =>
        defined({ ...job, databaseId: id, url }),
      );
    },

    // The plain-text log of one job.
    async jobLog(jobId, signal) {
      return askGitHub(api(`jobs/${jobId}/logs`), signal);
    },
  };
}
