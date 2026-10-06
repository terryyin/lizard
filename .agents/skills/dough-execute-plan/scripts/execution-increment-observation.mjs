// Establish or reuse matching CI observation for managed delivery: live
// mailbox match, host-bridge readiness, start, and coordinator binding.
// Codex is observed only by the yielded stream its coordinator armed at
// execution start. Resume recovers only an unambiguous live owner; it never
// starts a replacement.
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

// Managed delivery never starts a Codex observer: the coordinator's own
// yielded stream is the one it reuses.
function codexStreamMissingReason({ repo, branch }) {
  return `no live Codex yielded stream observes ${repo} ${branch}; arm \`ci-mailbox.mjs stream --execution ${repo} ${branch}\` in a yielded cell as references/ci-notify-codex.md describes, and later deliveries reuse it`;
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

  if (host === "codex") {
    return {
      observation: coverageGap(codexStreamMissingReason({ repo, branch })),
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
