// Resume an accepted managed delivery without duplicate push or guessed
// observation. Verifies remote acceptance, recovers only the live observer
// its retained owner evidence names, and reports an actionable gap when
// coverage cannot be restored. A fetch or push that outlasts the transport
// bound stops as held stops do.
import { resolve } from "node:path";
import { resolveCheckoutRuntime } from "./ci-checkout-runtime.mjs";
import {
  rejectRetiredObserverFlags,
  withExplicitSession,
} from "./ci-host-bridge.mjs";
import { mailboxRoot } from "./ci-mailbox.mjs";
import { registerPushedRevision } from "./ci-mailbox-revision-records.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { recoverObservationForResume } from "./execution-increment-observation-recovery.mjs";
import { stopped } from "./applicable-candidate-proof.mjs";
import { resumeInterruptedPublication } from "./publication-resume.mjs";
import { recordAcceptedIncrement } from "./execution-session-evidence.mjs";
import { resolveLaunchLanding } from "./resolve-publication-landing-context.mjs";
import { targetBranchName } from "./publication-git.mjs";

export async function resumeManagedExecutionIncrement(request) {
  const {
    workspace,
    candidateSha,
    suffixBase,
    targetRef,
    repo,
    remote = "origin",
    supersededShas = [],
    defaultCheckout,
    host = "cursor",
    session,
    coordinator,
    observerDirectory,
    env = process.env,
    root,
    storage,
    oneShotIdentity,
    landingContext,
    branch,
  } = request;

  for (const field of ["workspace", "candidateSha", "targetRef", "repo"]) {
    if (!request[field]) {
      return {
        ok: false,
        publication: "refused",
        error: `missing ${field}`,
        receipt: null,
        observation: null,
      };
    }
  }

  const runtime = resolveCheckoutRuntime(workspace, { host });
  const observerRoot = root ?? runtime.checkout;
  const observerStorage = storage ?? mailboxRoot;
  const targetBranch = targetBranchName(targetRef);
  const observation = recoverObservationForResume({
    repo,
    branch: targetBranch,
    host,
    session,
    coordinator,
    observerDirectory,
    env,
    root: observerRoot,
    storage: observerStorage,
  });

  const liveDirectory =
    observation.state === "recovered" ? observation.directory : null;

  // Queued one-shot work keeps its ownership guard when resume must push.
  const onFetchedTarget = oneShotIdentity
    ? (await import("./one-shot-ownership.mjs")).queuedOwnershipGuard({
        workspace,
        identity: oneShotIdentity,
      })
    : undefined;
  const { landingContext: resolvedLanding, reference } =
    await resolveLaunchLanding({
      landingContext,
      workspace,
      branch,
      remote,
      targetRef,
    });
  let published;
  try {
    published = await resumeInterruptedPublication({
      ownedWorkspace: workspace,
      defaultCheckout,
      candidateSha,
      suffixBase,
      remote,
      landingContext: resolvedLanding,
      branch,
      backlogPath: request.backlogPath,
      supersededShas,
      targetRef,
      onFetchedTarget,
    });
  } catch (error) {
    if (error?.code !== "transport-timeout") throw error;
    // A stalled fetch or push stops with the candidate preserved; the same
    // resume run again settles acceptance from the remote.
    return stopped("transport-timeout", {
      stage: error.stage,
      pushCount: error.pushCount,
      pushIssued: error.pushIssued,
      classification: error.classification,
      boundMs: error.boundMs,
      remote: error.remote,
      target: targetRef,
      candidate: candidateSha,
      remoteTip: error.remoteTip,
      observation,
    });
  }
  const comparison =
    published.suffixBase === undefined
      ? {}
      : { suffixBase: published.suffixBase };
  if (published.held)
    return stopped(published.held.status, {
      candidate: candidateSha,
      pushCount: 0,
      classification: published.classification,
      ...comparison,
      ...published.held.fields,
      observation,
    });

  // An accepted resume with a known base joins the session evidence; a repeat
  // of an already recorded pair adds nothing.
  if (published.suffixBase !== undefined)
    recordAcceptedIncrement(workspace, {
      base: published.suffixBase,
      candidate: published.acceptedSha,
      reference,
    });

  // Register on the recovered owner after remote acceptance.
  if (liveDirectory)
    registerPushedRevision(liveDirectory, published.acceptedSha, {
      onlyIfMissing: true,
    });

  return {
    ok: true,
    publication: "accepted",
    report: "accepted",
    pushCount: published.pushCount,
    classification: published.classification,
    ...(published.landing === undefined ? {} : { landing: published.landing }),
    receipt: {
      sha: published.acceptedSha,
      target: targetRef,
    },
    ...comparison,
    observation,
    runtime: {
      alias: runtime.alias,
      skillRoot: runtime.skillRoot,
      entrypoint: runtime.entrypoint,
    },
    maintenance: published.maintenance ?? null,
  };
}

function argumentsOf(argv) {
  if (argv[0] !== "resume") {
    throw new Error(
      "usage: execution-increment-resume.mjs resume --workspace PATH --candidate-sha SHA --target-ref REF --repo OWNER/REPO [--suffix-base SHA] [--remote NAME] [--host cursor|claude|codex] [--session-json JSON] [--coordinator VALUE --observer-directory PATH] [--default-checkout PATH] [--superseded-sha SHA]... [--one-shot-identity ID] [--landing-context PATH]",
    );
  }
  const result = { supersededShas: [] };
  for (let index = 1; index < argv.length; index += 1) {
    const flag = argv[index];
    if (flag === "--superseded-sha") {
      result.supersededShas.push(argv[++index]);
      continue;
    }
    if (!flag.startsWith("--") || index + 1 >= argv.length) {
      throw new Error(`invalid argument ${flag}`);
    }
    const key = flag
      .slice(2)
      .replace(/-[a-z]/g, (match) => match[1].toUpperCase());
    result[key] = argv[++index];
  }
  rejectRetiredObserverFlags(result);
  return withExplicitSession(result);
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const args = argumentsOf(process.argv.slice(2));
    const result = await resumeManagedExecutionIncrement({
      ...args,
      workspace: resolve(args.workspace),
      defaultCheckout: args.defaultCheckout
        ? resolve(args.defaultCheckout)
        : undefined,
    });
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
