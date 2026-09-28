// Git mechanics for Trunk Mode wrap-up closure. Each owned closure commit
// is published through publishExecutionIncrement, or classified through
// resumeInterruptedPublication when it is already the retained candidate.
// The remote execution branch stays unpublished and undeleted. Current-branch
// closure stays in the recorded checkout and follows the caller's publication
// authority through deliverRecordedCheckout. Installed guidance is the agent's
// contract.
import { deliverRecordedCheckout } from "../../dough-execute-plan/scripts/current-branch-publication.mjs";
import { publishExecutionIncrement } from "../../dough-execute-plan/scripts/execution-increment-publication.mjs";
import { refreshDefaultCheckout } from "../../dough-execute-plan/scripts/maintain-default-checkout.mjs";
import {
  registerClosureReceipt,
  settleClosureCandidate,
  trunkTarget,
} from "./closure-candidate-settlement.mjs";
import { removeExecutionResources } from "./closure-resources.mjs";

export { removeExecutionResources };

async function refreshAfterAcceptance({
  defaultCheckout,
  declaredOwner,
  requester,
}) {
  return refreshDefaultCheckout({
    checkout: defaultCheckout,
    declaredOwner,
    requester,
    integrationBranch: "main",
  });
}

function unpublishedClosure(published) {
  return {
    ok: false,
    publication: published.publication,
    status: published.status,
    receipt: null,
    candidate: published.candidate,
    preRebaseSha: published.preRebaseSha,
    remoteTip: published.remoteTip,
    previouslyPublishedBase: published.previouslyPublishedBase,
    suffixBase: published.suffixBase,
    maintenance: null,
  };
}

export async function publishTrunkClosureRevision({
  workspace,
  branch,
  previouslyPublishedBase,
  observer,
  defaultCheckout,
  declaredOwner,
  requester,
  validate,
  validatedCandidate,
  backlogPath,
}) {
  const published = await publishExecutionIncrement({
    workspace,
    branch,
    previouslyPublishedBase,
    targetRef: trunkTarget,
    register: registerClosureReceipt(observer),
    validate,
    validatedCandidate,
    backlogPath,
  });
  if (!published.ok) {
    return unpublishedClosure(published);
  }
  const maintenance = await refreshAfterAcceptance({
    defaultCheckout,
    declaredOwner,
    requester,
  });
  return {
    ok: true,
    receipt: published.receipt,
    preRebaseSha: published.preRebaseSha,
    maintenance,
  };
}

async function finishObligation(result, input) {
  if (result.stopped === true) {
    return { ...result, cleanup: "not-performed" };
  }
  const refresh = await refreshAfterAcceptance(input);
  return { ...result, refresh, cleanup: "not-performed" };
}

export async function resumeTrunkClosure(input) {
  const beforeResult = await settleClosureCandidate({
    ...input,
    sha: input.beforeCleanupSha,
    previouslyPublishedBase: input.previouslyPublishedBase,
  });
  if (beforeResult.completedObligation !== "none") {
    return finishObligation(beforeResult, input);
  }
  if (!input.finalClosureSha) {
    return {
      completedObligation: "final-closure-not-committed",
      pushCount: 0,
      cleanup: "not-performed",
      acceptedSha: beforeResult.acceptedSha,
    };
  }
  const finalResult = await settleClosureCandidate({
    ...input,
    sha: input.finalClosureSha,
    previouslyPublishedBase: input.beforeCleanupSha,
  });
  if (finalResult.completedObligation !== "none") {
    return finishObligation(finalResult, input);
  }
  if (!input.observer.stopped) {
    input.observer.stop();
    return {
      completedObligation: "stop-observer",
      pushCount: 0,
      cleanup: "not-performed",
      acceptedSha: input.finalClosureSha,
    };
  }
  const cleanup = await removeExecutionResources({
    integration: input.integration,
    execution: input.ownedWorkspace,
    branch: input.branch,
    observer: input.observer,
    sessionOwned: input.sessionOwned,
    closureShas: [input.beforeCleanupSha, input.finalClosureSha],
  });
  return {
    completedObligation: "remove-resources",
    pushCount: 0,
    cleanup,
    acceptedSha: input.finalClosureSha,
  };
}

export async function deliverCurrentBranchClosure(request) {
  return deliverRecordedCheckout(request);
}
