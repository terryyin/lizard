// Prepare the reviewed merge and retain its landing comparison. The shared
// loop owns publication; this caller preserves a stopped merge for recovery.
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import {
  captureAcceptedLanding,
  retainLandingComparison,
} from "./dashboard-landing.mjs";
import { resolvePublicationLandingContext } from "./resolve-publication-landing-context.mjs";
import {
  constructCandidate,
  mergeInProgress,
} from "./history-preserving-candidate.mjs";
import { git, originTrackingRef, revParse } from "./publication-git.mjs";
import { publishExactCandidate } from "./publication-loop.mjs";
import { isAncestor } from "./workspace-publication-ownership.mjs";
import { doneRecordsStop } from "./publication-backlog-validation.mjs";

export const trunkTarget = "refs/heads/main";

async function returnToBranch(workspace, branch) {
  if (!branch || (await mergeInProgress(workspace))) {
    return;
  }
  const status = (await git(workspace, "status", "--porcelain")).stdout;
  if (status !== "") {
    return;
  }
  await git(workspace, "checkout", branch);
}

export async function publishHistoryPreservingCandidate({
  ownedWorkspace,
  publishedTip,
  branch,
  targetRef = trunkTarget,
  remote = "origin",
  backlogPath = defaultBacklogPath,
  affectedCheck,
  onFetchedTarget,
  beforePush,
  landingContext: suppliedLandingContext,
}) {
  const landingContext = await resolvePublicationLandingContext({
    landingContext: suppliedLandingContext,
    workspace: ownedWorkspace,
    branch,
    remote,
    targetRef,
  });
  const adapterStatuses = [];
  let mergeCount = 0;
  let comparison;
  const {
    acceptedTip,
    stop,
    timeout,
    contended,
    pushCount,
    rejectedPushCount,
    supersededSha,
  } = await publishExactCandidate({
    workspace: ownedWorkspace,
    remote,
    targetRef,
    retryRejected: true,
    fetchAfterPush: true,
    prepareCandidate: async ({ attempt, remoteTip }) => {
      if (
        attempt === 0 &&
        remoteTip &&
        (await isAncestor(ownedWorkspace, publishedTip, remoteTip))
      ) {
        return { alreadyPublished: true };
      }
      const suffixBase =
        remoteTip ??
        (await revParse(ownedWorkspace, originTrackingRef(targetRef, remote)));
      const held = await onFetchedTarget?.({
        attempt,
        candidate: publishedTip,
        remoteTip: suffixBase,
      });
      if (held) return { stop: { reason: held.status, ...held.fields } };

      const prepared = await constructCandidate(
        ownedWorkspace,
        suffixBase,
        publishedTip,
        backlogPath,
      );
      if (prepared.mergeRan) mergeCount += 1;
      if (prepared.adapterStatus) adapterStatuses.push(prepared.adapterStatus);
      if (!prepared.ok) {
        return {
          stop: {
            reason: prepared.reason ?? "conflict",
            ...(prepared.error && { error: prepared.error }),
            ...(prepared.conflictedPaths && {
              conflictedPaths: prepared.conflictedPaths,
            }),
          },
        };
      }
      const invalid = await doneRecordsStop({
        workspace: ownedWorkspace,
        candidate: prepared.sha,
        backlogPath,
      });
      if (invalid)
        return { stop: { reason: invalid.status, ...invalid.fields } };

      if (affectedCheck) await affectedCheck(prepared.sha);
      comparison = { candidate: prepared.sha, suffixBase };
      if (landingContext) {
        try {
          await retainLandingComparison(landingContext, comparison, {
            remote,
            targetRef,
          });
        } catch (error) {
          // Keep a previously resolved merge that exists only at HEAD.
          if (prepared.mergeRan) await returnToBranch(ownedWorkspace, branch);
          throw error;
        }
      }
      await beforePush?.({ attempt, ...comparison });
      return comparison;
    },
  });

  if (timeout) throw timeout.error;
  const facts = {
    mergeCount,
    pushCount,
    rejectedPushCount,
    supersededSha,
    adapterStatuses,
  };
  if (stop || contended) {
    return {
      classification: "preserved",
      ...(stop ?? { reason: "persistent-contention" }),
      ...facts,
      receipt: null,
    };
  }
  if (!acceptedTip) throw new Error("remote did not accept the candidate");
  // A rerun after a lost reply still records the pair kept before its push.
  const landing = landingContext
    ? await captureAcceptedLanding(landingContext, {
        remote,
        target: targetRef,
        ...(pushCount && {
          base: comparison.suffixBase,
          revision: acceptedTip,
        }),
      })
    : undefined;
  await returnToBranch(ownedWorkspace, branch);
  return {
    classification: pushCount ? "published" : "already-accepted",
    ...(landingContext && { landing }),
    ...facts,
    acceptedSha: acceptedTip,
    receipt: pushCount ? { sha: acceptedTip, target: targetRef } : null,
  };
}
