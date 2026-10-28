#!/usr/bin/env node
// CLI for history-preserving trunk integration. Library behavior lives in
// history-preserving-publish.mjs. The CLI runs unawaited: --supplier-identity
// lazily imports supplier-integration.mjs, which imports this module back.
// Installed guidance is the agent's contract.
import { resolve } from "node:path";
import { flagValues, isDirectCliEntry } from "./ci-direct-entry.mjs";
export {
  publishHistoryPreservingCandidate,
  trunkTarget,
} from "./history-preserving-publish.mjs";
import { publishHistoryPreservingCandidate } from "./history-preserving-publish.mjs";

const usage =
  "usage: history-preserving-publication.mjs integrate --workspace PATH --published-tip SHA --branch NAME [--target-ref refs/heads/<branch>] [--remote NAME] [--backlog-path PATH] [--landing-context PATH] [--supplier-identity IDENTITY]";

function argumentsOf(argv) {
  if (argv[0] !== "integrate") throw new Error(usage);
  const result = flagValues(argv.slice(1));
  const { workspace, publishedTip, branch, ...rest } = result;
  const known =
    /^(targetRef|remote|backlogPath|landingContext|supplierIdentity)$/;
  if (
    !workspace ||
    !publishedTip ||
    !branch ||
    Object.keys(rest).some((key) => !known.test(key))
  ) {
    throw new Error(usage);
  }
  return { ownedWorkspace: resolve(workspace), publishedTip, branch, ...rest };
}

async function runCli(argv) {
  try {
    const request = argumentsOf(argv);
    const integrate = request.supplierIdentity
      ? (await import("./supplier-integration.mjs")).integrateSelectedSupplier
      : publishHistoryPreservingCandidate;
    const result = await integrate(request);
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (result.classification === "preserved") process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  runCli(process.argv.slice(2));
}
