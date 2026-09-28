import { execFile } from "node:child_process";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);
export const ciWorkflowFile = process.env.DOUGH_CI_WORKFLOW ?? "ci.yml";
// Workflow identity is the `--workflow` selector every listing requests; the
// display name is not a second, independent filter. An explicitly supplied
// display name only confirms that selection: a selected run reporting another
// name is inconsistent setup, rejected loudly instead of silently dropped.
const expectedWorkflowName = process.env.DOUGH_CI_WORKFLOW_NAME || undefined;

export async function readGitHubActions(args, signal) {
  const { stdout } = await execFileAsync("gh", args, {
    timeout: 20_000,
    signal,
    maxBuffer: 1024 * 1024,
    env: { ...process.env, GH_PROMPT_DISABLED: "1" },
  });
  return JSON.parse(stdout);
}

export function matchingCiRuns(runs, { branch, sha }) {
  const misnamed = expectedWorkflowName
    ? runs.find(({ workflowName }) => workflowName !== expectedWorkflowName)
    : undefined;
  if (misnamed)
    throw new Error(
      `DOUGH_CI_WORKFLOW_NAME ${JSON.stringify(expectedWorkflowName)} does not match ${JSON.stringify(misnamed.workflowName)}, the display name GitHub reports for runs of the selected workflow ${ciWorkflowFile}; correct or unset DOUGH_CI_WORKFLOW_NAME.`,
    );
  return runs.filter(
    (run) =>
      (!sha || run.headSha === sha) &&
      run.headBranch === branch &&
      run.event === "push",
  );
}

const runFields =
  "databaseId,attempt,headSha,headBranch,workflowName,event,status,conclusion,url";

export function startupCiRuns(runs) {
  const completedWithCreationTime = runs.filter(
    (run) =>
      run.status === "completed" && Number.isFinite(Date.parse(run.createdAt)),
  );
  if (!completedWithCreationTime.length) return runs;

  const newestCompleted = completedWithCreationTime.reduce((newest, run) =>
    Date.parse(run.createdAt) > Date.parse(newest.createdAt) ? run : newest,
  );
  return runs.filter(
    (run) => run.status !== "completed" || run === newestCompleted,
  );
}

// Startup-snapshot rule shared by every run acquisition: the first poll keeps
// the newest completed attempt and unfinished attempts; the older completed
// attempts it drops are history and never resurface on later polls, while a
// newer attempt of the same run does.
export function createStartupRunFilter() {
  let startupDiscovery = true;
  const historyAttempts = new Map();
  return (runs) => {
    if (!startupDiscovery)
      return runs.filter(
        (run) =>
          run.status !== "completed" ||
          !historyAttempts.get(run.databaseId)?.has(run.attempt),
      );
    startupDiscovery = false;
    const current = startupCiRuns(runs);
    for (const run of runs) {
      if (current.includes(run)) continue;
      const attempts = historyAttempts.get(run.databaseId) ?? new Set();
      historyAttempts.set(run.databaseId, attempts.add(run.attempt));
    }
    return current;
  };
}

export function listRunsArguments({
  repo,
  branch,
  sha,
  created,
  limit = 20,
  includeCreatedAt = false,
}) {
  const args = [
    "run",
    "list",
    "--repo",
    repo,
    "--workflow",
    ciWorkflowFile,
    "--branch",
    branch,
  ];
  if (sha) args.push("--commit", sha);
  if (created) args.push("--created", created);
  return [
    ...args,
    "--event",
    "push",
    "--limit",
    String(limit),
    "--json",
    includeCreatedAt ? `${runFields},createdAt` : runFields,
  ];
}

export function viewRunArguments({ repo, runId }) {
  return [
    "run",
    "view",
    String(runId),
    "--repo",
    repo,
    "--json",
    "attempt,status,conclusion,url",
  ];
}

// Bounded-history lookup used only to find an applicable ancestor attempt
// for CI-path-applicability classification (ci-path-applicability.mjs via
// ci-mailbox-revision-coverage.mjs). Distinct from the polling acquisition
// above: it is not part of the ordinary per-poll run/failure loop and is
// called on demand, only when a registered revision's exact-SHA classification
// cannot find a proved ancestor among the runs already visible to that poll.
// `limit` bounds how far back the search looks; it does not track state
// across calls the way `createGitHubRunAcquisition` does.
export async function discoverApplicabilityCandidateRuns({
  repo,
  branch,
  gh = readGitHubActions,
  signal,
  limit = 100,
}) {
  const runs = await gh(
    listRunsArguments({ repo, branch, limit, includeCreatedAt: true }),
    signal,
  );
  return matchingCiRuns(runs, { branch });
}

export function createGitHubRunAcquisition({
  repo,
  branch,
  startedAt,
  gh = readGitHubActions,
}) {
  const trackedRuns = new Map();
  const startupBoundary = `<=${new Date(startedAt).toISOString()}`;
  const currentRuns = createStartupRunFilter();
  let startupDiscovery = true;

  return async (signal) => {
    const runs = await gh(
      listRunsArguments({
        repo,
        branch,
        created: startupDiscovery ? startupBoundary : undefined,
        limit: startupDiscovery ? 100 : 20,
        includeCreatedAt: true,
      }),
      signal,
    );
    const matching = currentRuns(matchingCiRuns(runs, { branch }));
    startupDiscovery = false;

    const visibleRunIds = new Set(matching.map((run) => run.databaseId));
    for (const run of matching) {
      if (run.status === "completed") trackedRuns.delete(run.databaseId);
      else trackedRuns.set(run.databaseId, run);
    }
    for (const [runId, tracked] of trackedRuns) {
      if (visibleRunIds.has(runId)) continue;
      const refreshed = {
        ...tracked,
        ...(await gh(viewRunArguments({ repo, runId }), signal)),
      };
      trackedRuns.set(runId, refreshed);
      matching.push(refreshed);
    }
    for (const run of matching) {
      if (run.status === "completed") trackedRuns.delete(run.databaseId);
    }
    return matching;
  };
}
