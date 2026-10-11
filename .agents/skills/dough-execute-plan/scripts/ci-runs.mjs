import { createGitHubFailureAcquisition } from "./ci-failures.mjs";
import { ciWorkflowFile, createGitHubActions } from "./ci-github-actions.mjs";

// An explicitly supplied display name only confirms the workflow the selector
// chose: a selected run reporting another name is inconsistent setup,
// rejected loudly instead of silently dropped.
const expectedWorkflowName = process.env.DOUGH_CI_WORKFLOW_NAME || undefined;

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

// Bounded-history lookup used only to find an applicable ancestor attempt
// for CI-path-applicability classification (ci-path-applicability.mjs via
// ci-mailbox-revision-coverage.mjs). Distinct from the polling acquisition
// above: it is not part of the ordinary per-poll run/failure loop and is
// called on demand, only when a registered revision's exact-SHA classification
// cannot find a proved ancestor among the runs already visible to that poll.
// `limit` bounds how far back the search looks; it does not track state
// across calls the way `createGitHubRunAcquisition` does.
async function discoverApplicabilityCandidateRuns({
  branch,
  github,
  signal,
  limit = 100,
}) {
  return matchingCiRuns(await github.listRuns({ branch, limit }, signal), {
    branch,
  });
}

function createGitHubRunAcquisition({ branch, startedAt, github }) {
  const trackedRuns = new Map();
  const startupBoundary = `<=${new Date(startedAt).toISOString()}`;
  const currentRuns = createStartupRunFilter();
  let startupDiscovery = true;

  return async (signal) => {
    const runs = await github.listRuns(
      {
        branch,
        created: startupDiscovery ? startupBoundary : undefined,
        limit: startupDiscovery ? 100 : 20,
      },
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
        ...(await github.viewRun(runId, signal)),
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

// The GitHub default as the observer reads it: runs and failures over one
// reader of GitHub Actions, asked through `gh`. CI-path applicability reuses
// a proved ancestor attempt, and that ancestor is not always among the runs
// the ordinary poll already surfaced (older completed runs intentionally drop
// out of `matching` once seen, per createGitHubRunAcquisition's own
// startup/observed-run pruning), so `discoverAncestorCandidates` finds them.
// Its candidates are deliberately left run-shaped (not flattened to bare
// SHAs): coverage selection reuses them to resolve a `not_required`
// revision's basis ancestor to its actual attempt, so that ancestor's
// eventual failure stays actionable for every revision that reuses it.
export function createGitHubAcquisition({
  repo,
  branch,
  startedAt,
  gh,
  signal,
}) {
  const github = createGitHubActions({ repo, gh });
  return {
    acquireRuns: createGitHubRunAcquisition({ branch, startedAt, github }),
    acquireFailure: createGitHubFailureAcquisition({ repo, github }),
    discoverAncestorCandidates: async () =>
      discoverApplicabilityCandidateRuns({ branch, github, signal }),
  };
}
