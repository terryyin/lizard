// Git mechanics for publish-the-candidate.md "Resume an interrupted publication".
// One classification shared by execution and preparation. Installed guidance
// is the agent's contract for recovery.
import {
  isAncestor,
  validateRetainedComparison,
} from "./publication-comparison.mjs";
import {
  retainLandingComparison,
  captureAcceptedLanding,
} from "./dashboard-landing.mjs";
import { resolvePublicationLandingContext } from "./resolve-publication-landing-context.mjs";
import {
  git,
  inspectDefaultCheckoutMaintenance,
  originTrackingRef,
  pushExactRef,
  revParse,
} from "./publication-git.mjs";
import {
  assertOwnedCommitsPreserved,
  atStage,
  ownedCommitIdentity,
} from "./publication-resume-support.mjs";
import { doneRecordsStop } from "./publication-backlog-validation.mjs";

const defaultTargetRef = "refs/heads/main";

// Confirms publication of the retained candidate. The caller owns CI
// registration, checkout maintenance and cleanup. `candidateSha` is the SHA
// retained immediately before the push, including after a rewrite.
// `suffixBase`, when retained with it, is returned unchanged even when the
// accepted candidate is an ancestor of a newer remote tip. Its absence leaves
// the comparison unavailable; publication recovery still works as before.
// `supersededShas` are pre-rebase identities and are never pushed. Before a
// push, an `onFetchedTarget` stop (see applicable-candidate-proof.mjs) for
// the fetched target tip is reported as `held` instead. A fetch or push that
// outlasts the transport bound throws its `transport-timeout` error carrying
// the stage (`fetch`, `push`, `confirmation-fetch`) and that attempt's facts.
export async function resumeInterruptedPublication({
  ownedWorkspace,
  defaultCheckout,
  candidateSha,
  suffixBase,
  supersededShas = [],
  targetRef = defaultTargetRef,
  remote = "origin",
  onFetchedTarget,
  landingContext: suppliedLandingContext,
  branch,
  backlogPath,
}) {
  if (supersededShas.includes(candidateSha)) {
    throw new Error(
      "retained candidate must be the rewritten SHA, not a pre-rebase SHA",
    );
  }

  const landingContext = await resolvePublicationLandingContext({
    landingContext: suppliedLandingContext,
    workspace: ownedWorkspace,
    branch,
    remote,
    targetRef,
  });
  await validateRetainedComparison(ownedWorkspace, candidateSha, suffixBase);
  const comparison = suffixBase === undefined ? {} : { suffixBase };
  const captureLanding = () =>
    landingContext
      ? captureAcceptedLanding(landingContext, {
          base: suffixBase,
          revision: candidateSha,
          remote,
          target: targetRef,
        })
      : undefined;

  const remoteTarget = originTrackingRef(targetRef, remote);
  const preserved = await ownedCommitIdentity(ownedWorkspace);
  await atStage(
    "fetch",
    { classification: null, pushIssued: false, pushCount: 0, remoteTip: null },
    () => git(ownedWorkspace, "fetch", remote),
  );
  const accepted = await isAncestor(ownedWorkspace, candidateSha, remoteTarget);

  if (!accepted) {
    const remoteTip = await revParse(ownedWorkspace, remoteTarget).catch(
      () => null,
    );
    const held =
      (await onFetchedTarget?.({
        attempt: 0,
        candidate: candidateSha,
        remoteTip,
      })) ||
      (await doneRecordsStop({
        workspace: ownedWorkspace,
        candidate: candidateSha,
        backlogPath,
      }));
    if (held)
      return {
        classification: "not-on-remote",
        pushCount: 0,
        ...comparison,
        held,
      };
    if (landingContext)
      await retainLandingComparison(
        landingContext,
        { candidate: candidateSha, suffixBase },
        { remote, targetRef },
      );
    const pushAttempt = { classification: "not-on-remote", pushIssued: true };
    // An unanswered push leaves acceptance unknown; nothing is counted as
    // pushed until the push answers.
    await atStage("push", { ...pushAttempt, pushCount: 0, remoteTip }, () =>
      pushExactRef(ownedWorkspace, candidateSha, remote, targetRef),
    );
    await atStage(
      "confirmation-fetch",
      { ...pushAttempt, pushCount: 1, remoteTip },
      () => git(ownedWorkspace, "fetch", remote),
    );
    if (!(await isAncestor(ownedWorkspace, candidateSha, remoteTarget))) {
      throw new Error("push did not accept the retained candidate");
    }
  } else {
    for (const stale of supersededShas) {
      if (await isAncestor(ownedWorkspace, stale, remoteTarget)) {
        throw new Error("a pre-rebase SHA must not be the published candidate");
      }
    }
  }

  const landing = await captureLanding();
  assertOwnedCommitsPreserved(
    preserved,
    await ownedCommitIdentity(ownedWorkspace),
  );
  const maintenance = await inspectDefaultCheckoutMaintenance(
    ownedWorkspace,
    defaultCheckout,
    remote,
    targetRef,
  );
  return {
    ...(landing === undefined ? {} : { landing }),
    classification: accepted ? "already-published" : "not-on-remote",
    pushCount: accepted ? 0 : 1,
    acceptedSha: candidateSha,
    ...comparison,
    maintenance,
    cleanup: "not-performed",
    preservedHead: preserved.head,
    preservedCommitCount: preserved.count,
  };
}
