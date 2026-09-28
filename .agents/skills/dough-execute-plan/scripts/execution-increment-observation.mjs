// Establish or reuse matching CI observation for managed delivery: live
// mailbox match, host-bridge readiness, start, and coordinator binding.
// Resume recovers only an unambiguous live owner; it never starts a replacement.
import {
  bindHostObserver,
  resolveHostSession,
  verifyHostBridge,
} from "./ci-host-bridge.mjs";
import { receiptPrefix, startExecutionMailbox } from "./ci-mailbox.mjs";
import {
  classifyMatchingObservationOwnership,
  findLiveMatchingMailbox,
} from "./ci-mailbox-match.mjs";

function coverageGap(reason, extras = {}) {
  return {
    state: "unobserved",
    pendingCi: "unobserved",
    reason,
    ...extras,
  };
}

function observationAttached(directory, { reused = false } = {}) {
  return {
    state: reused ? "reused" : "attached",
    directory,
    reused,
  };
}

// Resume-only recovery: attach when exactly one live match exists. Ended,
// lost, ambiguous, or missing owners become actionable coverage gaps.
export function recoverObservationForResume({
  repo,
  branch,
  root,
  storage,
} = {}) {
  const ownership = classifyMatchingObservationOwnership({
    repo,
    branch,
    root,
    storage,
  });
  if (ownership.kind === "live") {
    return {
      observation: {
        state: "recovered",
        directory: ownership.directory,
        reused: true,
      },
      ownership,
    };
  }
  return {
    observation: coverageGap(ownership.reason, {
      directory: ownership.directory,
      ownership: ownership.kind,
    }),
    ownership,
  };
}

export async function establishObservation({
  repo,
  branch,
  host,
  session,
  workspace,
  runtime,
  maxDurationMs,
  env,
  root,
  storage,
  codexBridgeAvailable,
}) {
  const existing = findLiveMatchingMailbox({
    repo,
    branch,
    root,
    storage,
  });
  if (existing) {
    return {
      observation: observationAttached(existing, { reused: true }),
      startReceipt: null,
    };
  }

  // One resolved owner for both readiness and binding.
  const owner = resolveHostSession({ host, session, env });
  const bridge = await verifyHostBridge({
    host,
    session: owner,
    workspace,
    hookPath: runtime.hookEntrypoint,
    env,
    root,
    storage,
    codexBridgeAvailable,
  });
  if (!bridge.ready) {
    return {
      observation: coverageGap(bridge.reason ?? "host bridge unavailable"),
      startReceipt: null,
    };
  }

  const directory = await startExecutionMailbox(
    {
      mode: "execution",
      repo,
      branch,
      maxDurationMs,
    },
    { root, storage, env },
  );
  const startReceipt = `${receiptPrefix}${JSON.stringify({ directory })}\n`;
  const binding = await bindHostObserver({
    host,
    session: owner,
    receipt: startReceipt,
    workspace,
    hookPath: runtime.hookEntrypoint,
    env,
  });
  if (!binding.attached) {
    return {
      observation: coverageGap(
        binding.context || "host bridge did not attach the observer",
      ),
      directory,
      startReceipt,
    };
  }
  return {
    observation: observationAttached(directory),
    startReceipt,
  };
}
