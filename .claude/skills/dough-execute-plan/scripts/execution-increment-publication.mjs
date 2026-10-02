// Git mechanics for one validated execution increment or owned repair.
// The caller supplies the owned workspace, the owned unpublished suffix,
// the authorized remote target, and how the accepted result is registered.
// A previously published base that no fetched remote ref holds stops at once.
// `onFetchedTarget` may stop on each fetched target tip before anything is
// rewritten. When another writer advances the target, only that owned suffix
// is reconciled; a changed candidate requires applicable proof before any
// push. One reconciliation retry recovers a racing push; conflict or a second
// rejection preserves recoverable Git state. Stash, checkout refresh, and
// observer startup stay with their own owners, managed observation with
// execution-increment-delivery.mjs. Installed guidance is the agent's contract.
import {
  ensureApplicableProof,
  fetchedTargetStop,
  proofGateResult,
  stopped,
} from "./applicable-candidate-proof.mjs";
import {
  defaultBacklogPath,
  reconcileOwnedSuffix,
} from "./owned-suffix-reconciliation.mjs";
import {
  fetchedTarget,
  git,
  inspectDefaultCheckoutMaintenance,
  lsRemoteSha,
  remoteHolds,
  revParse,
  tryPushExactRef,
} from "./publication-git.mjs";

// Reconcile the owned suffix onto onto, then require applicable proof before
// any push. Shared by the initial remote-advance path and the one retry.
async function reconcileAndRequireProof({
  workspace,
  onto,
  upstream,
  branch,
  backlogPath,
  candidateFallback,
  preRebaseSha,
  previouslyPublishedBase,
  priorSuffixBase,
  validate,
  validatedCandidate,
  reconciliations,
  retry = false,
}) {
  const replay = await reconcileOwnedSuffix({
    workspace,
    onto,
    upstream,
    branch,
    backlogPath,
  });
  const nextReconciliations = reconciliations + 1;
  if (!replay.ok) {
    return {
      ok: false,
      result: stopped("conflict", {
        candidate: await revParse(workspace, branch).catch(
          () => candidateFallback,
        ),
        preRebaseSha,
        remoteTip: onto,
        previouslyPublishedBase,
        ...(retry ? { suffixBase: priorSuffixBase } : {}),
        reconciliations: nextReconciliations,
        replay,
      }),
    };
  }
  const candidate = await revParse(workspace, branch);
  if (!retry && candidate === preRebaseSha && !validatedCandidate) {
    throw new Error("rebase left the pre-rebase SHA as the candidate");
  }
  // Held proof is judged only after rewrite; a further remote advance needs
  // renewed applicable proof even when a prior validatedCandidate was supplied.
  const gate = await ensureApplicableProof({
    validate,
    candidate,
    context: {
      preRebaseSha,
      remoteTip: onto,
      previouslyPublishedBase,
      suffixBase: onto,
      ...(retry ? { retry: true } : {}),
    },
    proofAlreadyHeld:
      !retry && Boolean(validatedCandidate) && candidate === validatedCandidate,
  });
  if (!gate.ok) {
    return {
      ok: false,
      result: proofGateResult(gate, {
        candidate,
        preRebaseSha,
        remoteTip: onto,
        previouslyPublishedBase,
        suffixBase: onto,
        reconciliations: nextReconciliations,
      }),
    };
  }
  return {
    ok: true,
    candidate,
    suffixBase: onto,
    reconciliations: nextReconciliations,
  };
}

export async function publishExecutionIncrement({
  workspace,
  branch,
  previouslyPublishedBase,
  targetRef,
  register,
  remote = "origin",
  validate,
  validatedCandidate,
  defaultCheckout,
  backlogPath = defaultBacklogPath,
  beforeRetryPush,
  beforePush,
  onFetchedTarget,
}) {
  await git(workspace, "fetch", remote);
  let remoteTip = await fetchedTarget(workspace, targetRef, remote);
  const preRebaseSha = await revParse(workspace, branch);
  // Commits under a base the remote does not hold are not this suffix: pushing
  // would publish them and reconciling would rebase them off the branch.
  if (!(await remoteHolds(workspace, previouslyPublishedBase, remote))) {
    return stopped("unpublished-base", {
      candidate: preRebaseSha,
      preRebaseSha,
      remoteTip,
      previouslyPublishedBase,
    });
  }
  let candidate = preRebaseSha;
  let suffixBase = previouslyPublishedBase;
  let reconciliations = 0;
  const held = (attempt) =>
    fetchedTargetStop(onFetchedTarget, attempt, {
      candidate,
      preRebaseSha,
      remoteTip,
      previouslyPublishedBase,
      suffixBase,
      reconciliations,
    });
  // Reconciles, advancing this attempt's state, or returns the stop.
  const reconcileOnto = async (onto, upstream, retry = false) => {
    const rewritten = await reconcileAndRequireProof({
      workspace,
      onto,
      upstream,
      branch,
      backlogPath,
      candidateFallback: candidate,
      preRebaseSha,
      previouslyPublishedBase,
      priorSuffixBase: suffixBase,
      validate,
      validatedCandidate,
      reconciliations,
      retry,
    });
    if (!rewritten.ok) return rewritten.result;
    ({ candidate, suffixBase, reconciliations } = rewritten);
    return null;
  };
  if (validatedCandidate && preRebaseSha !== validatedCandidate) {
    return stopped("candidate-mismatch", {
      candidate: preRebaseSha,
      validatedCandidate,
      preRebaseSha,
      remoteTip,
      previouslyPublishedBase,
    });
  }
  if (validatedCandidate) {
    candidate = validatedCandidate;
  }

  const heldFirst = await held(0);
  if (heldFirst) return heldFirst;
  // Validated resume supplies previouslyPublishedBase as the tip the candidate
  // already extends. Only a further remote advance rewrites again.
  if (remoteTip && remoteTip !== previouslyPublishedBase) {
    const stop = await reconcileOnto(remoteTip, previouslyPublishedBase);
    if (stop) return stop;
  }

  if (beforePush) {
    await beforePush({ attempt: 0, candidate });
  }
  let push = await tryPushExactRef(workspace, candidate, remote, targetRef);
  if (push.rejected) {
    await git(workspace, "fetch", remote);
    remoteTip = await fetchedTarget(workspace, targetRef, remote);
    const heldRetry = await held(1);
    if (heldRetry) return heldRetry;
    const stop = await reconcileOnto(remoteTip, suffixBase, true);
    if (stop) return stop;

    if (beforeRetryPush) {
      await beforeRetryPush();
    }
    if (beforePush) {
      await beforePush({ attempt: 1, candidate });
    }
    push = await tryPushExactRef(workspace, candidate, remote, targetRef);
    if (push.rejected) {
      return stopped("persistent-contention", {
        candidate,
        preRebaseSha,
        remoteTip: await lsRemoteSha(remote, targetRef, workspace),
        previouslyPublishedBase,
        suffixBase,
        reconciliations,
      });
    }
  }

  await git(workspace, "fetch", remote);
  const acceptedTip = await lsRemoteSha(remote, targetRef, workspace);
  if (acceptedTip !== candidate) {
    throw new Error("remote did not accept the candidate");
  }
  const receipt = { sha: candidate, target: targetRef };
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
    receipt,
    preRebaseSha,
    remoteTip: acceptedTip,
    suffixBase,
    reconciliations,
    maintenance,
  };
}
