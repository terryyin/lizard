// Settle one unpublished Trunk closure SHA: resume when already on remote,
// publish through publishExecutionIncrement when the owned tip still holds it,
// or stop with recoverable context. Resume orchestration stays in
// closure-publication.mjs.
import { existsSync } from "node:fs";
import { publishExecutionIncrement } from "../../dough-execute-plan/scripts/execution-increment-publication.mjs";
import {
  git,
  originTrackingRef,
  revParse,
} from "../../dough-execute-plan/scripts/publication-test-fixtures.mjs";
import { resumeInterruptedPublication } from "../../dough-execute-plan/scripts/publication-resume.mjs";
import { findWorktree, isAncestor, trunkTarget } from "./closure-resources.mjs";

function registerClosureReceipt(observer) {
  return (receipt) => {
    observer.register(receipt.sha, receipt.target);
  };
}

function stoppedClosurePublish(published) {
  return {
    classification: "stopped",
    stopped: true,
    completedObligation: "publish",
    pushCount: 0,
    acceptedSha: null,
    preRebaseSha: published.preRebaseSha,
    candidate: published.candidate,
    status: published.status,
    cleanup: "not-performed",
    reason: published.status,
  };
}

async function ownedWorkspaceAvailable(integration, ownedWorkspace) {
  if (await findWorktree(integration, ownedWorkspace)) {
    return true;
  }
  return existsSync(ownedWorkspace);
}

export async function settleClosureCandidate({
  ownedWorkspace,
  integration,
  defaultCheckout,
  branch,
  sha,
  previouslyPublishedBase,
  supersededShas,
  publishedRevisions,
  observer,
  validate,
  validatedCandidate,
  backlogPath,
}) {
  const workspaceReady = await ownedWorkspaceAvailable(
    integration,
    ownedWorkspace,
  );
  const inspectionWorkspace = workspaceReady ? ownedWorkspace : integration;
  await git(inspectionWorkspace, "fetch", "origin");
  const tracking = originTrackingRef(trunkTarget);
  const remoteTip = await revParse(inspectionWorkspace, tracking);
  const onRemote = await isAncestor(inspectionWorkspace, sha, tracking);
  if (!workspaceReady && !onRemote) {
    return {
      classification: "stopped",
      stopped: true,
      completedObligation: "publish",
      pushCount: 0,
      acceptedSha: null,
      cleanup: "not-performed",
      reason: "execution worktree is absent before closure is on remote trunk",
    };
  }
  const canFastForward =
    onRemote || (await isAncestor(inspectionWorkspace, remoteTip, sha));
  if (canFastForward) {
    return resumeInterruptedPublication({
      ownedWorkspace: workspaceReady ? ownedWorkspace : integration,
      defaultCheckout,
      candidateSha: sha,
      supersededShas,
      publishedRevisions,
      observer,
      targetRef: trunkTarget,
    });
  }
  const tip = await revParse(ownedWorkspace, branch);
  if (tip !== sha) {
    return {
      classification: "stopped",
      stopped: true,
      completedObligation: "publish",
      pushCount: 0,
      acceptedSha: null,
      cleanup: "not-performed",
      reason: "unpublished closure needs rebase and is not the branch tip",
    };
  }
  const published = await publishExecutionIncrement({
    workspace: ownedWorkspace,
    branch,
    previouslyPublishedBase,
    targetRef: trunkTarget,
    register: registerClosureReceipt(observer),
    validate,
    validatedCandidate,
    backlogPath,
  });
  if (!published.ok) {
    return stoppedClosurePublish(published);
  }
  if (!publishedRevisions.includes(published.receipt.sha)) {
    publishedRevisions.push(published.receipt.sha);
  }
  return {
    classification: "not-on-remote",
    completedObligation: "publish",
    pushCount: 1,
    acceptedSha: published.receipt.sha,
    preRebaseSha: published.preRebaseSha,
    receipt: published.receipt,
    acceptedPublicationCount: publishedRevisions.length,
    cleanup: "not-performed",
  };
}

export { registerClosureReceipt, trunkTarget };
