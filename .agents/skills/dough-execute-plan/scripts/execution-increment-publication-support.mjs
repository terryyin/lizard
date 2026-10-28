// Transport-timeout wrapping, remote fetch/push/tip steps, and accepted-receipt
// packaging for publishExecutionIncrement. Mutable attempt state stays with
// the publisher; this module only stages remote work and builds returns.
import { stopped } from "./applicable-candidate-proof.mjs";
import { captureAcceptedLanding } from "./dashboard-landing.mjs";
import {
  fetchedTarget,
  git,
  inspectDefaultCheckoutMaintenance,
  lsRemoteSha,
  tryPushExactRef,
} from "./publication-git.mjs";

// Runs one remote transport step. A step that outlasts the bound becomes a
// `transport-timeout` stop naming that stage with this attempt's facts;
// other failures throw as before.
export function boundPublicationTransport(attemptFacts) {
  return async (stage, operation) => {
    try {
      return { value: await operation() };
    } catch (error) {
      if (error?.code !== "transport-timeout") throw error;
      return {
        stop: stopped("transport-timeout", {
          stage,
          ...attemptFacts(),
          boundMs: error.boundMs,
        }),
      };
    }
  };
}

// Registers the accepted candidate and inspects the default checkout.
export async function acceptPublishedExecutionIncrement({
  workspace,
  candidate,
  targetRef,
  suffixBase,
  landingContext,
  remote,
  register,
  defaultCheckout,
  preRebaseSha,
  reconciliations,
  acceptedTip,
  classification,
}) {
  const receipt = { sha: candidate, target: targetRef };
  const landing = landingContext
    ? await captureAcceptedLanding(landingContext, {
        base: suffixBase,
        revision: candidate,
        remote,
        target: targetRef,
      })
    : undefined;
  register?.(receipt);
  const maintenance = await inspectDefaultCheckoutMaintenance(
    workspace,
    defaultCheckout,
    remote,
    targetRef,
  );
  return {
    ok: true,
    publication: "accepted",
    ...(landing === undefined ? {} : { landing }),
    ...(classification && { classification }),
    receipt,
    preRebaseSha,
    remoteTip: acceptedTip,
    suffixBase,
    reconciliations,
    maintenance,
  };
}

// Fetch, push, tip-read, and accept helpers closed over one mutable attempt.
export function executionIncrementRemoteOps({
  workspace,
  targetRef,
  remote,
  landingContext,
  register,
  defaultCheckout,
  preRebaseSha,
  attempt,
}) {
  // Mutable publication attempt; rename so property updates are not param reassigns.
  const state = attempt;
  const transport = boundPublicationTransport(() => ({
    pushIssued: state.pushIssued,
    remote,
    target: targetRef,
    candidate: state.candidate,
    preRebaseSha,
    previouslyPublishedBase: state.previouslyPublishedBase,
    suffixBase: state.suffixBase,
    remoteTip: state.remoteTip,
    reconciliations: state.reconciliations,
  }));
  return {
    fetchTarget: async (stage) => {
      const fetched = await transport(stage, () =>
        git(workspace, "fetch", remote),
      );
      if (!fetched.stop) {
        state.remoteTip = await fetchedTarget(workspace, targetRef, remote);
      }
      return fetched;
    },
    push: async (stage) => {
      state.pushIssued = true;
      return transport(stage, () =>
        tryPushExactRef(workspace, state.candidate, remote, targetRef),
      );
    },
    readTip: (stage) =>
      transport(stage, () => lsRemoteSha(remote, targetRef, workspace)),
    accept: (acceptedTip, classification) =>
      acceptPublishedExecutionIncrement({
        workspace,
        candidate: state.candidate,
        targetRef,
        suffixBase: state.suffixBase,
        landingContext,
        remote,
        register,
        defaultCheckout,
        preRebaseSha,
        reconciliations: state.reconciliations,
        acceptedTip,
        classification,
      }),
  };
}
