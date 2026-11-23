#!/usr/bin/env node
// Story Branch wrap-up's record stage, after the integrated SHA's completion
// receipt and the consumer visit, before retirement. The record keeps the
// session's accepted branch pairs, original hashes included, and any
// separately evidenced continuation; the integration merge only proves their
// acceptance on trunk and adds no range. A launch's landing-context reference,
// which only the integration takes, names the session and keeps its branch
// ranges. The record publishes as a separate records-only commit, and a rerun
// resumes the sealed record. Prints `{ record }` or `{ stopped, record,
// recovery }`; a stop exits 1.
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { adoptLaunchReference } from "../../dough-execute-plan/scripts/execution-session-evidence.mjs";
import {
  flagValues,
  isDirectCliEntry,
} from "../../dough-execute-plan/scripts/ci-direct-entry.mjs";
import { originTrackingRef } from "../../dough-execute-plan/scripts/publication-git.mjs";
import { publishClosureRecord } from "./trunk-closure-session-record.mjs";

const usage =
  "usage: story-branch-session-record.mjs record --workspace PATH [--inspection PATH] --published-tip SHA --integrated-sha SHA --branch NAME [--target-ref refs/heads/<branch>] [--remote NAME] [--backlog-path PATH] [--landing-context PATH]";

export async function publishStoryBranchRecord({
  workspace,
  inspection = workspace,
  publishedTip,
  integratedSha,
  branch,
  targetRef = "refs/heads/main",
  remote = "origin",
  backlogPath,
  landingContext,
}) {
  if (landingContext) {
    const { reference } = JSON.parse(await readFile(landingContext, "utf8"));
    adoptLaunchReference(workspace, reference);
  }
  return publishClosureRecord(
    {
      workspace,
      inspection,
      tracking: originTrackingRef(targetRef, remote),
      branch,
      acceptedSha: integratedSha,
      closedSha: publishedTip,
      targetRef,
      remote,
      ...(backlogPath ? { backlogPath } : {}),
      recovery: "rerun the same record command",
    },
    { integratedSha },
  );
}

function argumentsOf(argv) {
  if (argv[0] !== "record") throw new Error(usage);
  const {
    workspace,
    inspection,
    publishedTip,
    integratedSha,
    branch,
    ...rest
  } = flagValues(argv.slice(1));
  const known = /^(targetRef|remote|backlogPath|landingContext)$/;
  if (
    !workspace ||
    !publishedTip ||
    !integratedSha ||
    !branch ||
    Object.keys(rest).some((key) => !known.test(key))
  )
    throw new Error(usage);
  return {
    workspace: resolve(workspace),
    inspection: resolve(inspection ?? workspace),
    publishedTip,
    integratedSha,
    branch,
    ...rest,
  };
}

async function runCli(argv) {
  try {
    const result = await publishStoryBranchRecord(argumentsOf(argv));
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (result.stopped) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  runCli(process.argv.slice(2));
}
