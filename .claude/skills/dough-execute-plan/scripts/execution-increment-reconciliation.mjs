// Reconcile one publication attempt's owned suffix onto a newer target tip,
// then require applicable proof for the rewritten candidate before any push.
// Shared by execution-increment-publication.mjs's initial remote-advance path
// and its one retry.
import {
  ensureApplicableProof,
  proofGateResult,
  stopped,
} from "./applicable-candidate-proof.mjs";
import { reconcileOwnedSuffix } from "./owned-suffix-reconciliation.mjs";
import { revParse } from "./publication-git.mjs";

// Returns `{ ok: true, candidate, suffixBase, reconciliations }`, or
// `{ ok: false, result }` holding the conflict or proof stop.
export async function reconcileAndRequireProof({
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
