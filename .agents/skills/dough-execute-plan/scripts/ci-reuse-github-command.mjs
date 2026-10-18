#!/usr/bin/env node
// GitHub scheduling example's callable acquisition/decision boundary.
import { appendFile, readFile, writeFile } from "node:fs/promises";
import { pathToFileURL } from "node:url";
import { readGitHubReuseFacts } from "./ci-reuse-github.mjs";
import { reusableValidation } from "./ci-reuse-policy.mjs";

export async function decideGitHubReuse(
  policyPath,
  reportPath,
  env = process.env,
) {
  const target = {
    runId: env.GITHUB_RUN_ID,
    attemptId: env.GITHUB_RUN_ATTEMPT,
    sha: env.GITHUB_SHA,
    ref: env.GITHUB_REF_NAME,
    url: `${env.GITHUB_SERVER_URL}/${env.GITHUB_REPOSITORY}/actions/runs/${env.GITHUB_RUN_ID}/attempts/${env.GITHUB_RUN_ATTEMPT}`,
  };
  let decision;
  try {
    const policy = JSON.parse(await readFile(policyPath, "utf8"));
    const query = await readGitHubReuseFacts({
      sha: target.sha,
      policy,
      currentRunId: target.runId,
      freshOverride:
        env.DOUGH_CI_FRESH === "false" &&
        target.ref === policy.targetRef &&
        env.GITHUB_REPOSITORY === policy.repo
          ? false
          : true,
    });
    decision = reusableValidation({
      sha: target.sha,
      targetRef: target.ref,
      policy,
      query,
    });
  } catch {
    decision = {
      reuse: false,
      reason: "unreadable-policy-or-evidence",
      sha: target.sha,
    };
  }
  const report = { target, ...decision };
  await writeFile(reportPath, `${JSON.stringify(report, null, 2)}\n`);
  if (env.GITHUB_OUTPUT)
    await appendFile(env.GITHUB_OUTPUT, `reuse=${decision.reuse}\n`);
  if (env.GITHUB_STEP_SUMMARY)
    await appendFile(
      env.GITHUB_STEP_SUMMARY,
      `### Validation reuse decision\n\nTarget: ${target.url}\n\n\`\`\`json\n${JSON.stringify(report, null, 2)}\n\`\`\`\n`,
    );
  return report;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href)
  await decideGitHubReuse(process.argv[2], process.argv[3]);
