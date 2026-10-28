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
  appendIdentity,
  assertOwnedCommitsPreserved,
  atStage,
  hasReceipt,
  ownedCommitIdentity,
  registerIfBound,
  remainingAfter,
} from "./publication-resume-support.mjs";
import { completedIdentityStop } from "./publication-backlog-validation.mjs";

const defaultTargetRef = "refs/heads/main";

// Classifies the retained candidate and completes the first unfinished
// publication obligation. Does not commit, refresh the default checkout,
// or remove a workspace. `candidateSha` is the SHA retained immediately
// before the push; after a rewrite that is the rewritten SHA.
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
  publishedRevisions,
  observer = null,
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
    const held = await onFetchedTarget?.({
      attempt: 0,
      candidate: candidateSha,
      remoteTip,
    });
    if (held)
      return {
        classification: "not-on-remote",
        pushCount: 0,
        ...comparison,
        held,
      };
    const invalid = await completedIdentityStop({
      workspace: ownedWorkspace,
      candidate: candidateSha,
      backlogPath,
    });
    if (invalid)
      return {
        classification: "not-on-remote",
        pushCount: 0,
        ...comparison,
        held: invalid,
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
    const landing = await captureLanding();
    assertOwnedCommitsPreserved(
      preserved,
      await ownedCommitIdentity(ownedWorkspace),
    );
    appendIdentity(publishedRevisions, candidateSha);
    const registration = registerIfBound(observer, candidateSha, targetRef);
    const maintenance = await inspectDefaultCheckoutMaintenance(
      ownedWorkspace,
      defaultCheckout,
      remote,
      targetRef,
    );
    return {
      classification: "not-on-remote",
      ...(landing === undefined ? {} : { landing }),
      completedObligation: "publish",
      pushCount: 1,
      acceptedSha: candidateSha,
      ...comparison,
      acceptedPublicationCount: publishedRevisions.length,
      registration,
      maintenance,
      cleanup: "not-performed",
      preservedHead: preserved.head,
      preservedCommitCount: preserved.count,
      remaining: remainingAfter(observer, candidateSha, targetRef, maintenance),
    };
  }

  for (const stale of supersededShas) {
    if (await isAncestor(ownedWorkspace, stale, remoteTarget)) {
      throw new Error("a pre-rebase SHA must not be the published candidate");
    }
  }

  const landing = await captureLanding();
  assertOwnedCommitsPreserved(
    preserved,
    await ownedCommitIdentity(ownedWorkspace),
  );
  const identityAppended = appendIdentity(publishedRevisions, candidateSha);
  const maintenance = await inspectDefaultCheckoutMaintenance(
    ownedWorkspace,
    defaultCheckout,
    remote,
    targetRef,
  );
  const published = {
    ...(landing === undefined ? {} : { landing }),
    classification: "already-published",
    pushCount: 0,
    acceptedSha: candidateSha,
    ...comparison,
    acceptedPublicationCount: publishedRevisions.length,
    maintenance,
    cleanup: "not-performed",
    preservedHead: preserved.head,
    preservedCommitCount: preserved.count,
  };

  if (identityAppended) {
    return {
      ...published,
      completedObligation: "record-published-identity",
      registration: observer?.bound
        ? { attempted: false, reason: "not-this-obligation" }
        : { attempted: false, reason: "no-observer" },
      remaining: remainingAfter(observer, candidateSha, targetRef, maintenance),
    };
  }

  if (
    observer?.bound === true &&
    !hasReceipt(observer, candidateSha, targetRef)
  ) {
    return {
      ...published,
      completedObligation: "register",
      registration: registerIfBound(observer, candidateSha, targetRef),
      remaining: remainingAfter(observer, candidateSha, targetRef, maintenance),
    };
  }

  return {
    ...published,
    completedObligation: "none",
    registration: observer?.bound
      ? { attempted: false, reason: "already-recorded" }
      : { attempted: false, reason: "no-observer" },
    remaining: remainingAfter(observer, candidateSha, targetRef, maintenance),
  };
}
