// Git mechanics for publish-the-candidate.md "Resume an interrupted publication".
// One classification shared by execution and preparation. Installed guidance
// is the agent's contract for recovery.
import {
  git,
  inspectDefaultCheckoutMaintenance,
  originTrackingRef,
  pushExactRef,
  revParse,
} from "./publication-git.mjs";

const defaultTargetRef = "refs/heads/main";

async function isAncestor(workspace, ancestor, descendant) {
  try {
    await git(workspace, "merge-base", "--is-ancestor", ancestor, descendant);
    return true;
  } catch {
    return false;
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
// `supersededShas` are pre-rebase identities and are never pushed. Before a
// push, an `onFetchedTarget` stop (see applicable-candidate-proof.mjs) for
// the fetched target tip is reported as `held` instead.
export async function resumeInterruptedPublication({
  ownedWorkspace,
  defaultCheckout,
  candidateSha,
  supersededShas = [],
  publishedRevisions,
  observer = null,
  targetRef = defaultTargetRef,
  remote = "origin",
  onFetchedTarget,
}) {
  if (supersededShas.includes(candidateSha)) {
    throw new Error(
      "retained candidate must be the rewritten SHA, not a pre-rebase SHA",
    );
  }

  const remoteTarget = originTrackingRef(targetRef, remote);
  const preserved = await ownedCommitIdentity(ownedWorkspace);
  await git(ownedWorkspace, "fetch", remote);
  const accepted = await isAncestor(ownedWorkspace, candidateSha, remoteTarget);

  if (!accepted) {
    const held = await onFetchedTarget?.({
      attempt: 0,
      candidate: candidateSha,
      remoteTip: await revParse(ownedWorkspace, remoteTarget).catch(() => null),
    });
    if (held) return { classification: "not-on-remote", pushCount: 0, held };
    await pushExactRef(ownedWorkspace, candidateSha, remote, targetRef);
    await git(ownedWorkspace, "fetch", remote);
    if (!(await isAncestor(ownedWorkspace, candidateSha, remoteTarget))) {
      throw new Error("push did not accept the retained candidate");
    }
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
      completedObligation: "publish",
      pushCount: 1,
      acceptedSha: candidateSha,
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
    classification: "already-published",
    pushCount: 0,
    acceptedSha: candidateSha,
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
