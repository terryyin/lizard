#!/usr/bin/env node
// Resolve runtime and observation, publish an authorized increment, and attach
// acceptance. Held proof stops before push; local-only authority never pushes.
import { prepareAndDeliverFreshIncrement } from "./execution-increment-fresh.mjs";
import {
  checkoutRuntimeReceipt,
  resolveCheckoutRuntime,
} from "./ci-checkout-runtime.mjs";
import {
  registerPushedRevision,
  recordDeliveryProgress,
  mailboxRoot,
} from "./ci-mailbox.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { publishExecutionIncrement } from "./execution-increment-publication.mjs";
import { establishObservation } from "./execution-increment-observation.mjs";
import { recordAcceptedIncrement } from "./execution-session-evidence.mjs";
import { resolveLaunchLanding } from "./resolve-publication-landing-context.mjs";
export { resumeManagedExecutionIncrement } from "./execution-increment-resume.mjs";
import { targetBranchName } from "./publication-git.mjs";
import {
  managedIncrementRequestRefusal,
  readIncrementCliRequest,
} from "./execution-increment-request.mjs";

const managedStopFields = [
  "ok",
  "publication",
  "status",
  "receipt",
  "candidate",
  "preRebaseSha",
  "remoteTip",
  "previouslyPublishedBase",
  "suffixBase",
  "reconciliations",
  "problem",
  "stage",
  "pushIssued",
  "boundMs",
  "remote",
  "target",
  "replay",
  "validation",
  "ownership",
  "error",
];

// Existing-candidate delivery and recovery intentionally require no fresh commit.
export function deliverFreshExecutionIncrement(request) {
  return prepareAndDeliverFreshIncrement(
    request,
    deliverManagedExecutionIncrement,
  );
}

export async function deliverManagedExecutionIncrement(request) {
  const {
    workspace,
    branch,
    previouslyPublishedBase,
    targetRef,
    repo,
    host = "cursor",
    session,
    coordinator,
    observerDirectory,
    authority = "publish",
    remote = "origin",
    env = process.env,
    root,
    storage,
    validate,
    validatedCandidate,
    defaultCheckout,
    backlogPath,
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

  const refusal = managedIncrementRequestRefusal(request);
  if (refusal) {
    return {
      ok: false,
      publication: "refused",
      error: refusal,
      receipt: null,
      observation: null,
    };
  }

  const runtime = resolveCheckoutRuntime(workspace, { host });
  const runtimeSummary = checkoutRuntimeReceipt(runtime);
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

  // The launch reference, when this publication carries one, names the
  // session whose evidence the accepted pair joins.
  const { landingContext: resolvedLanding, reference } =
    await resolveLaunchLanding({
      landingContext,
      workspace,
      branch,
      remote,
      targetRef,
    });

  // Observation attaches before the first applicable push when a live owner is
  // established. An unavailable bridge still preserves accepted publication.
  const published = await publishExecutionIncrement({
    workspace,
    branch,
    previouslyPublishedBase,
    targetRef,
    remote,
    validate,
    validatedCandidate,
    defaultCheckout,
    backlogPath,
    beforePush,
    landingContext: resolvedLanding,
    onFetchedTarget,
  });

  // Started but unbound: keep the directory for recovery context without
  // claiming live coverage.
  const observation =
    established.directory && established.observation.state === "unobserved"
      ? { ...established.observation, directory: established.directory }
      : established.observation;
  if (published.ok) {
    recordAcceptedIncrement(workspace, {
      base: published.suffixBase,
      candidate: published.receipt.sha,
      reference,
    });

    if (observation.directory && observation.state !== "unobserved") {
      registerPushedRevision(observation.directory, published.receipt.sha);
      if (host === "claude" && request.mode === "story-branch")
        recordDeliveryProgress(observation.directory, {
          acceptedPublication: {
            sha: published.receipt.sha,
            mode: request.mode,
            targetRef,
          },
        });
    }
  }

  // Keep the established managed stop surface, including absent values.
  const result = published.ok
    ? published
    : Object.fromEntries(
        managedStopFields.map((field) => [field, published[field]]),
      );
  return {
    ...result,
    report: published.ok ? "accepted" : published.status,
    observation,
    runtime: runtimeSummary,
    startReceipt: established.startReceipt,
    maintenance: published.maintenance ?? null,
  };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const { fresh, request } = await readIncrementCliRequest(
      process.argv.slice(2),
    );
    const operation = fresh
      ? deliverFreshExecutionIncrement
      : deliverManagedExecutionIncrement;
    const result = await operation(request);
    if (result.startReceipt) process.stdout.write(result.startReceipt);
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
