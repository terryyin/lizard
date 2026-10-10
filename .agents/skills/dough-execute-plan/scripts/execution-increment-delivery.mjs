#!/usr/bin/env node
// Resolve runtime and observation, publish an authorized increment, and attach
// acceptance. Held proof stops before push; local-only authority never pushes.
import { resolve } from "node:path";
import { resolveCheckoutRuntime } from "./ci-checkout-runtime.mjs";
import { registerPushedRevision, mailboxRoot } from "./ci-mailbox.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { publishExecutionIncrement } from "./execution-increment-publication.mjs";
import { establishObservation } from "./execution-increment-observation.mjs";
export { resumeManagedExecutionIncrement } from "./execution-increment-resume.mjs";
import { targetBranchName } from "./publication-git.mjs";
import { executionBudgetMs } from "./watch-ci-execution.mjs";

export async function deliverManagedExecutionIncrement(request) {
  const {
    workspace,
    branch,
    previouslyPublishedBase,
    targetRef,
    repo,
    host = "cursor",
    preferredAlias,
    session,
    coordinator,
    observerDirectory,
    authority = "publish",
    remote = "origin",
    maxDurationMs = executionBudgetMs,
    env = process.env,
    root,
    storage,
    register,
    validate,
    validatedCandidate,
    defaultCheckout,
    backlogPath,
    beforeRetryPush,
    beforePush,
    landingContext,
    oneShotIdentity,
  } = request;

  if (authority !== "publish") {
    return {
      ok: true,
      publication: "pending",
      report: "local-only",
      receipt: null,
      observation: null,
    };
  }

  const refusal = requestRefusal(request);
  if (refusal) {
    return {
      ok: false,
      publication: "refused",
      error: refusal,
      receipt: null,
      observation: null,
    };
  }

  const runtime = resolveCheckoutRuntime(workspace, { host, preferredAlias });
  const { alias, skillRoot, entrypoint } = runtime;
  const runtimeSummary = { alias, skillRoot, entrypoint };
  const observerRoot = root ?? runtime.checkout;
  const observerStorage = storage ?? mailboxRoot;
  const targetBranch = targetBranchName(targetRef);

  const established = await establishObservation({
    repo,
    branch: targetBranch,
    host,
    session,
    coordinator,
    observerDirectory,
    workspace,
    runtime,
    maxDurationMs,
    env,
    root: observerRoot,
    storage: observerStorage,
  });

  // Queued one-shot work stops on a fetched target tip where another owner
  // holds it. Its guard reads backlog and profile records through the
  // product-backlog skill, so it is loaded only then.
  const onFetchedTarget = oneShotIdentity
    ? (await import("./one-shot-ownership.mjs")).queuedOwnershipGuard({
        workspace,
        identity: oneShotIdentity,
        backlogPath,
      })
    : undefined;

  // Observation attaches before the first applicable push when a live owner is
  // established. An unavailable bridge still preserves accepted publication.
  const published = await publishExecutionIncrement({
    workspace,
    branch,
    previouslyPublishedBase,
    targetRef,
    remote,
    register,
    validate,
    validatedCandidate,
    defaultCheckout,
    backlogPath,
    beforeRetryPush,
    beforePush,
    landingContext,
    onFetchedTarget,
  });

  // Started but unbound: keep the directory for recovery context without
  // claiming live coverage.
  const observation =
    established.directory && established.observation.state === "unobserved"
      ? { ...established.observation, directory: established.directory }
      : established.observation;
  if (!published.ok) {
    // Live owner is kept for later validated resume; no SHA registered yet.
    return {
      ok: false,
      publication: published.publication,
      status: published.status,
      report: published.status,
      receipt: null,
      candidate: published.candidate,
      preRebaseSha: published.preRebaseSha,
      remoteTip: published.remoteTip,
      previouslyPublishedBase: published.previouslyPublishedBase,
      suffixBase: published.suffixBase,
      reconciliations: published.reconciliations,
      // A transport-timeout stop names the stalled stage and its bound.
      stage: published.stage,
      pushIssued: published.pushIssued,
      boundMs: published.boundMs,
      remote: published.remote,
      target: published.target,
      replay: published.replay,
      validation: published.validation,
      ownership: published.ownership,
      error: published.error,
      observation,
      runtime: runtimeSummary,
      startReceipt: established.startReceipt,
      maintenance: published.maintenance ?? null,
    };
  }

  if (observation.directory && observation.state !== "unobserved") {
    registerPushedRevision(observation.directory, published.receipt.sha);
  }

  return {
    ok: true,
    publication: "accepted",
    report: "accepted",
    // A retry whose candidate the remote already held pushed nothing.
    ...(published.classification && {
      classification: published.classification,
    }),
    receipt: published.receipt,
    ...(published.landing === undefined ? {} : { landing: published.landing }),
    preRebaseSha: published.preRebaseSha,
    remoteTip: published.remoteTip,
    suffixBase: published.suffixBase,
    reconciliations: published.reconciliations,
    observation,
    runtime: runtimeSummary,
    startReceipt: established.startReceipt,
    // Deferred local refresh is independent of remote acceptance.
    maintenance: published.maintenance ?? null,
  };
}

// Why a delivery request is refused before anything is fetched, pushed, or
// observed: a missing field, or a Story Branch increment aimed anywhere but its
// execution branch without declaring a one-shot landing on trunk.
function requestRefusal(request) {
  for (const field of [
    "workspace",
    "branch",
    "previouslyPublishedBase",
    "targetRef",
    "repo",
  ]) {
    if (!request[field]) return `missing ${field}`;
  }
  const { mode, tracking, branch, targetRef } = request;
  const executionTarget = `refs/heads/${branch}`;
  if (
    mode === "story-branch" &&
    tracking !== "one-shot" &&
    targetRef !== executionTarget
  ) {
    return `Story Branch Mode delivers only to its execution branch: use --target-ref ${executionTarget}, not ${targetRef}; a one-shot landing declares --tracking one-shot`;
  }
  return null;
}

const usage =
  "usage: execution-increment-delivery.mjs deliver --mode trunk|story-branch --workspace PATH --branch NAME --previously-published-base SHA --target-ref refs/heads/<branch> --repo OWNER/REPO [--tracking one-shot] [--host cursor|claude|codex] [--preferred-alias .agents|.claude] [--authority publish|local-only] [--session-json JSON] [--coordinator VALUE --observer-directory PATH] [--max-duration-ms MS] [--validated-candidate SHA] [--default-checkout PATH] [--one-shot-identity ID] [--landing-context PATH]";

function argumentsOf(argv) {
  if (argv[0] !== "deliver") throw new Error(usage);
  const result = { authority: "publish" };
  for (let index = 1; index < argv.length; index += 1) {
    const flag = argv[index];
    // Every flag carries a value; a flag in a value's place is unknown.
    if (
      !flag.startsWith("--") ||
      index + 1 >= argv.length ||
      argv[index + 1].startsWith("--")
    ) {
      throw new Error(`invalid argument ${flag}`);
    }
    const key = flag
      .slice(2)
      .replace(/-[a-z]/g, (match) => match[1].toUpperCase());
    result[key] = argv[++index];
  }
  if (!["trunk", "story-branch"].includes(result.mode)) {
    throw new Error(usage);
  }
  if (result.sessionJson) {
    result.session = JSON.parse(result.sessionJson);
    delete result.sessionJson;
  }
  if (result.maxDurationMs !== undefined) {
    result.maxDurationMs = Number(result.maxDurationMs);
  }
  return result;
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const args = argumentsOf(process.argv.slice(2));
    const result = await deliverManagedExecutionIncrement({
      ...args,
      workspace: resolve(args.workspace),
      defaultCheckout: args.defaultCheckout
        ? resolve(args.defaultCheckout)
        : undefined,
    });
    if (result.startReceipt) process.stdout.write(result.startReceipt);
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
