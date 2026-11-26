import {
  git,
  lsRemoteSha,
  observePublicationTarget,
  originTrackingRef,
  pushExactRef,
  readTrackingTip,
  tryPushExactRef,
} from "./publication-git.mjs";

// The caller prepares its candidate against each fetched tip, or reports
// a stop or prior acceptance. Exact confirmation reads the live ref; resume
// checks containment in the fetched tip. Integration keeps its tracking ref
// current after each answered push, even when both attempts are rejected.
// A committed claim supplies its first candidate and checks ownership after
// any first push error, including when the remote may have accepted the push.
export async function publishExactCandidate({
  workspace,
  remote,
  targetRef,
  prepareCandidate,
  initialCandidate,
  recoverPushFailure,
  acceptsFetchedCandidate,
  acceptsLiveCandidate,
  confirmationRemote,
  retryRejected = false,
  fetchAfterPush = false,
}) {
  const tracking = originTrackingRef(targetRef, remote);
  let remoteTip = null;
  let pushIssued = false;
  const result = { pushCount: 0, rejectedPushCount: 0, supersededSha: null };
  let timeout;
  const transport = async (stage, operation) => {
    try {
      return await operation();
    } catch (error) {
      if (error?.code === "transport-timeout") {
        timeout = {
          error,
          stage,
          remoteTip,
          pushIssued,
          pushCount: result.pushCount,
        };
      }
      throw error;
    }
  };
  const fetch = (stage) =>
    transport(stage, () => git(workspace, "fetch", remote));
  const liveTip = (stage) =>
    transport(stage, () =>
      confirmationRemote === undefined
        ? lsRemoteSha(remote, targetRef, workspace)
        : lsRemoteSha(confirmationRemote, targetRef),
    );
  const push = retryRejected ? tryPushExactRef : pushExactRef;
  const retries = retryRejected || recoverPushFailure ? 1 : 0;
  const prepare = async (attempt, failedPush) => {
    if (attempt === 0 && initialCandidate !== undefined)
      return { candidate: initialCandidate };
    try {
      remoteTip = await transport(
        attempt ? "fetch-after-rejection" : "fetch",
        () => observePublicationTarget(workspace, remote, tracking),
      );
    } catch (error) {
      if (!failedPush) throw error;
      return recoverPushFailure({ ...failedPush, verificationError: error });
    }
    return failedPush
      ? recoverPushFailure({ ...failedPush, remoteTip })
      : prepareCandidate({ attempt, remoteTip });
  };

  let failedPush;
  let confirming = false;
  try {
    for (let attempt = 0; attempt <= retries; attempt += 1) {
      const prepared = await prepare(attempt, failedPush);
      if (prepared.stop) return { ...result, stop: prepared.stop };
      if (prepared.alreadyPublished)
        return { ...result, acceptedTip: remoteTip };

      pushIssued = true;
      const { candidate } = prepared;
      const pushed = await transport(attempt ? "retry-push" : "push", () =>
        push(workspace, candidate, remote, targetRef),
      ).catch((error) => {
        if (!recoverPushFailure) throw error;
        return { error };
      });
      if (pushed?.error) {
        if (recoverPushFailure) {
          if (attempt) return { ...result, pushError: pushed.error };
          failedPush = { candidate, pushError: pushed.error };
        } else {
          result.rejectedPushCount += 1;
          result.supersededSha = candidate;
        }
        continue;
      }
      result.pushCount += 1;
      confirming = true;
      const accepts = acceptsFetchedCandidate ?? acceptsLiveCandidate;
      if (fetchAfterPush || accepts) await fetch("confirmation-fetch");
      const tip = acceptsFetchedCandidate
        ? await readTrackingTip(workspace, tracking)
        : await liveTip("confirmation-tip");
      const accepted = acceptsFetchedCandidate
        ? tip && (await acceptsFetchedCandidate(candidate, tip))
        : ((await acceptsLiveCandidate?.(candidate, tip)) ?? tip === candidate);
      return { ...result, acceptedTip: accepted ? tip : null };
    }
    if (fetchAfterPush) {
      await fetch("fetch-after-rejection");
      return { ...result, contended: true };
    }
    return {
      ...result,
      contended: true,
      contentionTip: await liveTip("contention-tip"),
    };
  } catch (error) {
    if (recoverPushFailure && confirming) return { confirmationError: error };
    if (timeout && timeout.error === error) return { timeout };
    throw error;
  }
}
