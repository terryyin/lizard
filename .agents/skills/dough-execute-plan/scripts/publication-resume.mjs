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
import {
  git,
  inspectDefaultCheckoutMaintenance,
  originTrackingRef,
  pushExactRef,
  revParse,
} from "./publication-git.mjs";

const defaultTargetRef = "refs/heads/main";

// Runs one remote transport step. A step that outlasts the transport bound
// rethrows its `transport-timeout` error naming the resume `stage` and the
// attempt's facts, so a managed caller can report it as a stop while other
// callers see it as any other thrown Git failure.
async function atStage(stage, facts, operation) {
  try {
    return await operation();
  } catch (error) {
    if (error?.code === "transport-timeout") {
      Object.assign(error, { stage, ...facts });
    }
    throw error;
  }
}

function hasReceipt(observer, sha, targetRef) {
  return (
    observer?.bound === true &&
    observer.receipts.some(
      (receipt) => receipt.sha === sha && receipt.target === targetRef,
    )
  );
}

// A repository management context without a checked-out commit (a Git
// directory whose owned worktree is already retired) holds no owned commits.
async function ownedCommitIdentity(workspace) {
  try {
    await git(workspace, "rev-parse", "-q", "--verify", "HEAD");
  } catch (error) {
    if (error.code !== 1) throw error;
    return { head: null, count: 0 };
  }
  return {
    head: await revParse(workspace, "HEAD"),
    count: Number(
      (await git(workspace, "rev-list", "--count", "HEAD")).stdout.trim(),
    ),
  };
}

function appendIdentity(publishedRevisions, sha) {
  if (!publishedRevisions.includes(sha)) {
    publishedRevisions.push(sha);
    return true;
  }
  return false;
}

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
  landingContext,
}) {
  if (supersededShas.includes(candidateSha)) {
    throw new Error(
      "retained candidate must be the rewritten SHA, not a pre-rebase SHA",
    );
  }

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

function assertOwnedCommitsPreserved(before, after) {
  if (after.head !== before.head || after.count !== before.count) {
    throw new Error("resume duplicated or moved the owned commit");
  }
}

function registerIfBound(observer, sha, targetRef) {
  if (!observer?.bound) {
    return { attempted: false, reason: "no-observer" };
  }
  observer.register(sha, targetRef);
  return { attempted: true, sha, target: targetRef };
}

function remainingAfter(observer, sha, targetRef, maintenance) {
  return {
    registration: !observer?.bound
      ? "not-applicable"
      : hasReceipt(observer, sha, targetRef)
        ? "satisfied"
        : "remaining",
    maintenance,
    maintenancePerformed: false,
    cleanup: "not-performed",
  };
}
