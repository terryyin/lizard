// Apply execution ownership and proof to the shared exact publication loop.
import { fetchedTargetStop, stopped } from "./applicable-candidate-proof.mjs";
import { reconcileAndRequireProof } from "./execution-increment-reconciliation.mjs";
import { retainLandingComparison } from "./dashboard-landing.mjs";
import { acceptPublishedExecutionIncrement } from "./execution-increment-publication-support.mjs";
import { resolvePublicationLandingContext } from "./resolve-publication-landing-context.mjs";
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import { resolveCommitId, remoteHolds, revParse } from "./publication-git.mjs";
import { isAncestor } from "./workspace-publication-ownership.mjs";
import { publishExactCandidate } from "./publication-loop.mjs";
import { doneRecordsStop } from "./publication-backlog-validation.mjs";

export async function publishExecutionIncrement({
  workspace,
  branch,
  previouslyPublishedBase: suppliedBase,
  targetRef,
  remote = "origin",
  validate,
  validatedCandidate,
  defaultCheckout,
  backlogPath = defaultBacklogPath,
  beforePush,
  landingContext: suppliedLandingContext,
  onFetchedTarget,
}) {
  const landingContext = await resolvePublicationLandingContext({
    landingContext: suppliedLandingContext,
    workspace,
    branch,
    remote,
    targetRef,
  });
  const preRebaseSha = await revParse(workspace, branch);
  const attempt = {
    candidate: preRebaseSha,
    // The supplied base until the fetch settles, then its full commit ID.
    previouslyPublishedBase: suppliedBase,
    suffixBase: suppliedBase,
    reconciliations: 0,
  };
  const facts = (remoteTip) => ({ ...attempt, preRebaseSha, remoteTip });
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
  const checkedCandidates = new Set();
  const invalidCandidate = async (remoteTip) => {
    const { candidate } = attempt;
    if (checkedCandidates.has(candidate)) return null;
    const invalid = await doneRecordsStop({
      workspace,
      candidate,
      backlogPath,
    });
    if (!invalid) {
      checkedCandidates.add(candidate);
      return null;
    }
    return stopped(invalid.status, {
      ...facts(remoteTip),
      ...invalid.fields,
    });
  };

  const checkInitialCandidate = async (remoteTip) => {
    // Only the fetched remote can establish the base of the owned suffix.
    const resolved = await resolveCommitId(workspace, suppliedBase);
    const baseFields = {
      candidate: preRebaseSha,
      preRebaseSha,
      remoteTip,
      previouslyPublishedBase: resolved.commit ?? suppliedBase,
    };
    if (resolved.problem) {
      return {
        stop: stopped("unresolvable-base", {
          ...baseFields,
          problem: resolved.problem,
          error: `previously published base ${JSON.stringify(suppliedBase)} names no single commit (${resolved.problem}); rerun with the full commit ID from the delivery receipt`,
        }),
      };
    }
    attempt.previouslyPublishedBase = resolved.commit;
    attempt.suffixBase = resolved.commit;
    if (!(await remoteHolds(workspace, resolved.commit, remote))) {
      return { stop: stopped("unpublished-base", baseFields) };
    }
    if (validatedCandidate && preRebaseSha !== validatedCandidate) {
      return {
        stop: stopped("candidate-mismatch", {
          ...baseFields,
          validatedCandidate,
        }),
      };
    }
    if (
      remoteTip &&
      preRebaseSha !== resolved.commit &&
      (await isAncestor(workspace, preRebaseSha, remoteTip))
    ) {
      return { alreadyPublished: true };
    }
    const stop = await invalidCandidate(remoteTip);
    if (stop) return { stop };
  };

  const { acceptedTip, contentionTip, pushCount, stop, timeout } =
    await publishExactCandidate({
      workspace,
      remote,
      targetRef,
      retryRejected: true,
      prepareCandidate: async ({ attempt: attemptNumber, remoteTip }) => {
        if (attemptNumber === 0) {
          const initial = await checkInitialCandidate(remoteTip);
          if (initial) return initial;
        }
        const held = await fetchedTargetStop(
          onFetchedTarget,
          attemptNumber,
          facts(remoteTip),
        );
        if (held) return { stop: held };
        const retry = attemptNumber > 0;
        if (
          retry ||
          (remoteTip && remoteTip !== attempt.previouslyPublishedBase)
        ) {
          const conflict = await reconcileOnto(
            remoteTip,
            attempt.suffixBase,
            retry,
          );
          if (conflict) return { stop: conflict };
        }

        const invalid = await invalidCandidate(remoteTip);
        if (invalid) return { stop: invalid };
        const comparison = {
          candidate: attempt.candidate,
          suffixBase: attempt.suffixBase,
        };
        if (landingContext)
          await retainLandingComparison(landingContext, comparison, {
            remote,
            targetRef,
          });
        await beforePush?.({ attempt: attemptNumber, ...comparison });
        return comparison;
      },
    });
  if (stop) return stop;
  if (timeout) {
    const { stage, remoteTip, pushIssued, error } = timeout;
    return stopped("transport-timeout", {
      ...facts(remoteTip),
      stage,
      pushIssued,
      boundMs: error.boundMs,
      remote,
      target: targetRef,
    });
  }
  if (contentionTip !== undefined) {
    return stopped("persistent-contention", facts(contentionTip));
  }
  if (!acceptedTip) throw new Error("remote did not accept the candidate");
  return acceptPublishedExecutionIncrement({
    ...attempt,
    workspace,
    remote,
    targetRef,
    landingContext,
    defaultCheckout,
    preRebaseSha,
    acceptedTip,
    classification: pushCount === 0 ? "already-published" : undefined,
  });
}
