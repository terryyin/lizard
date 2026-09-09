import { execFile } from "node:child_process";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);
export const ciWorkflowFile = process.env.DOUGH_CI_WORKFLOW ?? "ci.yml";
const workflowName = process.env.DOUGH_CI_WORKFLOW_NAME ?? "CI";

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
  return runs.filter(
    (run) =>
      (!sha || run.headSha === sha) &&
      run.headBranch === branch &&
      run.workflowName === workflowName &&
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
