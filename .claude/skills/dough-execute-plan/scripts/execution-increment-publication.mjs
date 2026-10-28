// Git mechanics for a validated execution increment or owned repair: owned
// workspace/suffix, authorized target, and acceptance registration. Resolves
// the previously published base after fetch; refuses unresolvable or
// unpublished bases; accepts an already-held candidate; reconciles an owned
// suffix once on a racing tip with applicable proof; transport timeouts stop
// without rewrite. Reconciliation lives in
// execution-increment-reconciliation.mjs; remote/accept helpers in
// execution-increment-publication-support.mjs. Installed guidance is the
// agent's contract.
import { fetchedTargetStop, stopped } from "./applicable-candidate-proof.mjs";
import { reconcileAndRequireProof } from "./execution-increment-reconciliation.mjs";
import { retainLandingComparison } from "./dashboard-landing.mjs";
import { executionIncrementRemoteOps } from "./execution-increment-publication-support.mjs";
import { resolvePublicationLandingContext } from "./resolve-publication-landing-context.mjs";
import { defaultBacklogPath } from "./owned-suffix-reconciliation.mjs";
import { resolveCommitId, remoteHolds, revParse } from "./publication-git.mjs";
import { isAncestor } from "./workspace-publication-ownership.mjs";
import { completedIdentityStop } from "./publication-backlog-validation.mjs";

export async function publishExecutionIncrement({
  workspace,
  branch,
  previouslyPublishedBase: suppliedBase,
  targetRef,
  register,
  remote = "origin",
  validate,
  validatedCandidate,
  defaultCheckout,
  backlogPath = defaultBacklogPath,
  beforeRetryPush,
  beforePush,
  landingContext: suppliedLandingContext,
  onFetchedTarget,
}) {
  const landingContext = await resolvePublicationLandingContext({
    landingContext: suppliedLandingContext,
    workspace,
    branch,
  });
  const preRebaseSha = await revParse(workspace, branch);
  const attempt = {
    candidate: preRebaseSha,
    // The supplied base until the fetch settles, then its full commit ID.
    previouslyPublishedBase: suppliedBase,
    suffixBase: suppliedBase,
    reconciliations: 0,
    remoteTip: null,
    pushIssued: false,
  };
  const { fetchTarget, push, readTip, accept } = executionIncrementRemoteOps({
    workspace,
    targetRef,
    remote,
    landingContext,
    register,
    defaultCheckout,
    preRebaseSha,
    attempt,
  });

  const fetched = await fetchTarget("fetch");
  if (fetched.stop) return fetched.stop;
  // A full ID and its unique abbreviation must give one decision. Input that
  // names no single commit is refused before anything is compared or written.
  const resolved = await resolveCommitId(workspace, suppliedBase);
  if (resolved.problem) {
    return stopped("unresolvable-base", {
      problem: resolved.problem,
      candidate: preRebaseSha,
      preRebaseSha,
      remoteTip: attempt.remoteTip,
      previouslyPublishedBase: suppliedBase,
      error: `previously published base ${JSON.stringify(suppliedBase)} names no single commit (${resolved.problem}); rerun with the full commit ID from the delivery receipt`,
    });
  }
  attempt.previouslyPublishedBase = resolved.commit;
  attempt.suffixBase = resolved.commit;
  // Commits under a base the remote does not hold are not this suffix: pushing
  // would publish them and reconciling would rebase them off the branch.
  if (
    !(await remoteHolds(workspace, attempt.previouslyPublishedBase, remote))
  ) {
    return stopped("unpublished-base", {
      candidate: preRebaseSha,
      preRebaseSha,
      remoteTip: attempt.remoteTip,
      previouslyPublishedBase: attempt.previouslyPublishedBase,
    });
  }
  const held = (n) =>
    fetchedTargetStop(onFetchedTarget, n, {
      candidate: attempt.candidate,
      preRebaseSha,
      remoteTip: attempt.remoteTip,
      previouslyPublishedBase: attempt.previouslyPublishedBase,
      suffixBase: attempt.suffixBase,
      reconciliations: attempt.reconciliations,
    });
  // Reconciles, advancing this attempt's state, or returns the stop.
  const reconcileOnto = async (onto, upstream, retry = false) => {
    const rewritten = await reconcileAndRequireProof({
      workspace,
      onto,
      upstream,
      branch,
      backlogPath,
      candidateFallback: attempt.candidate,
      preRebaseSha,
      previouslyPublishedBase: attempt.previouslyPublishedBase,
      priorSuffixBase: attempt.suffixBase,
      validate,
      validatedCandidate,
      reconciliations: attempt.reconciliations,
      retry,
    });
    if (!rewritten.ok) return rewritten.result;
    attempt.candidate = rewritten.candidate;
    attempt.suffixBase = rewritten.suffixBase;
    attempt.reconciliations = rewritten.reconciliations;
    return null;
  };
  if (validatedCandidate && preRebaseSha !== validatedCandidate) {
    return stopped("candidate-mismatch", {
      candidate: preRebaseSha,
      validatedCandidate,
      preRebaseSha,
      remoteTip: attempt.remoteTip,
      previouslyPublishedBase: attempt.previouslyPublishedBase,
    });
  }
  if (validatedCandidate) {
    attempt.candidate = validatedCandidate;
  }

  // As resume classifies it: the remote already holds this candidate, so this
  // delivery only reports and registers that acceptance. An empty suffix has
  // nothing to deliver and still moves onto the fetched tip below.
  if (
    attempt.remoteTip &&
    attempt.candidate !== attempt.previouslyPublishedBase &&
    (await isAncestor(workspace, attempt.candidate, attempt.remoteTip))
  ) {
    return accept(attempt.remoteTip, "already-published");
  }

  const invalidCandidate = async () => {
    const invalid = await completedIdentityStop({
      workspace,
      candidate: attempt.candidate,
      backlogPath,
    });
    return invalid
      ? stopped(invalid.status, {
          preRebaseSha,
          remoteTip: attempt.remoteTip,
          previouslyPublishedBase: attempt.previouslyPublishedBase,
          suffixBase: attempt.suffixBase,
          reconciliations: attempt.reconciliations,
          ...invalid.fields,
        })
      : null;
  };
  const invalid = await invalidCandidate();
  if (invalid) return invalid;
  const heldFirst = await held(0);
  if (heldFirst) return heldFirst;
  // Validated resume supplies previouslyPublishedBase as the tip the candidate
  // already extends. Only a further remote advance rewrites again.
  if (
    attempt.remoteTip &&
    attempt.remoteTip !== attempt.previouslyPublishedBase
  ) {
    const stop = await reconcileOnto(
      attempt.remoteTip,
      attempt.previouslyPublishedBase,
    );
    if (stop) return stop;
  }

  const preparePush = async (n) => {
    const invalid = await invalidCandidate();
    if (invalid) return invalid;
    const comparison = {
      candidate: attempt.candidate,
      suffixBase: attempt.suffixBase,
    };
    if (landingContext)
      await retainLandingComparison(landingContext, comparison, {
        remote,
        targetRef,
      });
    await beforePush?.({ attempt: n, ...comparison });
  };
  const firstStop = await preparePush(0);
  if (firstStop) return firstStop;
  let pushed = await push("push");
  if (pushed.stop) return pushed.stop;
  if (pushed.value.rejected) {
    const refetched = await fetchTarget("fetch-after-rejection");
    if (refetched.stop) return refetched.stop;
    const heldRetry = await held(1);
    if (heldRetry) return heldRetry;
    const stop = await reconcileOnto(
      attempt.remoteTip,
      attempt.suffixBase,
      true,
    );
    if (stop) return stop;

    if (beforeRetryPush) {
      await beforeRetryPush();
    }
    const retryStop = await preparePush(1);
    if (retryStop) return retryStop;
    pushed = await push("retry-push");
    if (pushed.stop) return pushed.stop;
    if (pushed.value.rejected) {
      const contended = await readTip("contention-tip");
      if (contended.stop) return contended.stop;
      return stopped("persistent-contention", {
        candidate: attempt.candidate,
        preRebaseSha,
        remoteTip: contended.value,
        previouslyPublishedBase: attempt.previouslyPublishedBase,
        suffixBase: attempt.suffixBase,
        reconciliations: attempt.reconciliations,
      });
    }
  }

  const confirming = await fetchTarget("confirmation-fetch");
  if (confirming.stop) return confirming.stop;
  const confirmed = await readTip("confirmation-tip");
  if (confirmed.stop) return confirmed.stop;
  if (confirmed.value !== attempt.candidate) {
    throw new Error("remote did not accept the candidate");
  }
  return accept(confirmed.value);
}
