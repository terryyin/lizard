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
  verifyRef,
} from "./publication-git.mjs";
import { publishExactCandidate } from "./publication-loop.mjs";
import { doneRecordsStop } from "./publication-backlog-validation.mjs";

const defaultTargetRef = "refs/heads/main";

// A management repository without HEAD has no owned commits to preserve.
async function ownedCommitIdentity(workspace) {
  const head = await verifyRef(workspace, "HEAD");
  if (!head.verified) return { head: null, count: 0 };
  const { stdout } = await git(workspace, "rev-list", "--count", "HEAD");
  return { head: head.stdout.trim(), count: Number(stdout.trim()) };
}

// Confirms publication of the retained candidate. The caller owns CI
// registration, checkout maintenance and cleanup. `candidateSha` is the SHA
// retained immediately before the push, including after a rewrite.
// `suffixBase` is required and is returned unchanged when the accepted
// candidate is an ancestor of a newer remote tip.
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

  await validateRetainedComparison(ownedWorkspace, candidateSha, suffixBase);
  const landingContext = await resolvePublicationLandingContext({
    landingContext: suppliedLandingContext,
    workspace: ownedWorkspace,
    branch,
    remote,
    targetRef,
  });
  const captureLanding = () =>
    landingContext
      ? captureAcceptedLanding(landingContext, {
          base: suffixBase,
          revision: candidateSha,
          remote,
          target: targetRef,
        })
      : undefined;

  const preserved = await ownedCommitIdentity(ownedWorkspace);
  const accepts = (candidate, tip) =>
    isAncestor(ownedWorkspace, candidate, tip);
  const classification = "not-on-remote";
  const {
    acceptedTip,
    pushCount,
    stop: held,
    timeout,
  } = await publishExactCandidate({
    workspace: ownedWorkspace,
    remote,
    targetRef,
    acceptsFetchedCandidate: accepts,
    prepareCandidate: async ({ remoteTip }) => {
      if (remoteTip && (await accepts(candidateSha, remoteTip))) {
        for (const stale of supersededShas) {
          if (await accepts(stale, remoteTip)) {
            throw new Error(
              "a pre-rebase SHA must not be the published candidate",
            );
          }
        }
        return { alreadyPublished: true };
      }

      const stop =
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
      if (stop) return { stop };
      if (landingContext)
        await retainLandingComparison(
          landingContext,
          { candidate: candidateSha, suffixBase },
          { remote, targetRef },
        );
      return { candidate: candidateSha };
    },
  });
  if (timeout) {
    const { error, ...facts } = timeout;
    throw Object.assign(error, facts, {
      classification: facts.pushIssued ? classification : null,
    });
  }
  if (held) return { classification, pushCount, suffixBase, held };
  if (!acceptedTip)
    throw new Error("push did not accept the retained candidate");

  const landing = await captureLanding();
  const current = await ownedCommitIdentity(ownedWorkspace);
  if (current.head !== preserved.head || current.count !== preserved.count) {
    throw new Error("resume duplicated or moved the owned commit");
  }
  const maintenance = await inspectDefaultCheckoutMaintenance(
    ownedWorkspace,
    defaultCheckout,
    remote,
    targetRef,
  );
  return {
    ...(landing === undefined ? {} : { landing }),
    classification: pushCount ? classification : "already-published",
    pushCount,
    acceptedSha: candidateSha,
    suffixBase,
    maintenance,
    cleanup: "not-performed",
    preservedHead: preserved.head,
    preservedCommitCount: preserved.count,
  };
}
