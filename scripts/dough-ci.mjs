// Project command adapter for Open Dough's CI observer.
// Select before importing the shared client, which reads its selector at import.
process.env.DOUGH_CI_WORKFLOW = process.argv[2];

try {
  if (!process.argv[2]) throw new Error("a workflow selector is required");
  let input = "";
  for await (const chunk of process.stdin) input += chunk;
  const request = JSON.parse(input);
  if (!["discover", "diagnose"].includes(request.operation))
    throw new Error("unsupported CI adapter operation");
  const { createGitHubActions } = await import(
    "../.agents/skills/dough-execute-plan/scripts/ci-github-actions.mjs"
  );
  const { isFailedJob } = await import(
    "../.agents/skills/dough-execute-plan/scripts/ci-failures.mjs"
  );
  const github = createGitHubActions({ repo: request.check.repo });
  let response;
  if (request.operation === "discover") {
    const runs = await github.listRuns({ branch: request.check.branch });
    response = {
      attempts: runs.map((run) => ({
        runId: run.databaseId,
        attemptId: run.attempt,
        sha: run.headSha,
        outcome:
          run.status !== "completed"
            ? "pending"
            : run.conclusion === "success"
              ? "success"
              : isFailedJob(run)
                ? "failure"
                : "incomplete",
        branch: run.headBranch,
        check: run.workflowName,
        context: run.event,
        url: run.url,
        time: run.createdAt,
      })),
    };
  } else {
    const { diagnoseFailedAttempt } = await import(
      "../.agents/skills/dough-execute-plan/scripts/ci-github-diagnostic.mjs"
    );
    const diagnostic = await diagnoseFailedAttempt({
      repo: request.check.repo,
      runId: request.attempt.runId,
      attempt: request.attempt.attemptId,
      github,
    });
    const useful = diagnostic.failedJobs
      .map((job) =>
        [
          `${job.name}: ${job.conclusion}`,
          ...job.failedSteps.map((step) => `${step.name}: ${step.conclusion}`),
          ...(job.log?.lines ?? [job.logUnavailable]),
        ].join("\n"),
      )
      .join("\n\n");
    if (!useful) {
      response = { unavailable: "GitHub returned no failed jobs for this attempt" };
    } else {
      let excerpt = "";
      let bytes = 0;
      for (const character of useful) {
        const size = Buffer.byteLength(character);
        if (bytes + size > 16 * 1024) break;
        excerpt += character;
        bytes += size;
      }
      response = {
        excerpt,
        truncated:
          excerpt !== useful ||
          Boolean(diagnostic.omittedFailedJobs) ||
          diagnostic.failedJobs.some((job) => job.log?.omittedLines > 0),
      };
    }
  }
  const output = JSON.stringify(response);
  if (Buffer.byteLength(output) + 1 > 64 * 1024)
    throw new Error("CI adapter response exceeded 64 KiB");
  process.stdout.write(`${output}\n`);
} catch (error) {
  process.stderr.write(`${String(error.message).slice(0, 600)}\n`);
  process.exitCode = 2;
}
