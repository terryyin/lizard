import { captureAcceptedLanding } from "./dashboard-landing.mjs";
import { inspectDefaultCheckoutMaintenance } from "./publication-git.mjs";

// Reports the accepted candidate and inspects the default checkout.
export async function acceptPublishedExecutionIncrement({
  workspace,
  candidate,
  targetRef,
  suffixBase,
  landingContext,
  remote,
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
  const maintenance = await inspectDefaultCheckoutMaintenance(
    workspace,
    defaultCheckout,
    remote,
    targetRef,
    acceptedTip,
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
