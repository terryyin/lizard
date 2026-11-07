// Integrated-state evidence for one registered accepted revision B, judged by
// the observer's revision coverage under the project's declared policy.
// A successful current attempt of the declared check at a same-target
// descendant C (Git ancestry B → C) validates the integrated state at C; it is
// recorded beside B's own exact state, never as a result of B. Every poll
// judges it afresh: nothing here is retained once contradicted or unreadable.
// A locally proved descendant's current pending attempt supplies progress
// beside an unresolved judgement; it never validates the integrated state.
// The policy comes from ci-integrated-state-policy.mjs.
import { GitHubAcquisitionHeld } from "./ci-github-actions.mjs";
import {
  ancestryBasis,
  candidateAncestry,
} from "./ci-integrated-state-ancestry.mjs";
import {
  currentAttemptFacts,
  currentAttemptIneligibility,
  successfulAttempt,
} from "./ci-integrated-state-policy.mjs";
import { isFullGitRevision } from "./ci-revisions.mjs";

// B's own exact states this evidence may complete: no attempt of B was
// observed, or B's attempt was cancelled (for example, replaced while
// waiting). An ignored-only (`not_required`) B qualifies only while its
// applicable ancestor's attempt has no verdict (cancelled or still pending).
// Pending, success, failure, and a `not_required` B with a proved or
// unresolved basis keep their own rules.
const integratedEligibleStates = new Set(["undiscovered", "incomplete"]);
const integratedEligibleBasisStates = new Set(["incomplete", "pending"]);
const integratedEligible = ({ state, basis }) =>
  integratedEligibleStates.has(state) ||
  (state === "not_required" && integratedEligibleBasisStates.has(basis?.state));

// Authoritative current-attempt reads per revision per poll.
const verificationBound = 3;
const consideredBound = 5;

const failureDetail = (error) => String(error).slice(0, 300);

// A directed wait, or the end of observation, is no evidence: it ends this
// judgement without recording one.
const lookupFailure = (error, signal) => {
  if (error instanceof GitHubAcquisitionHeld || signal?.aborted) throw error;
  return failureDetail(error);
};

const unresolvedIntegratedState = (policy, reason, extra = {}) => ({
  state: "unresolved",
  policy,
  reason,
  ...extra,
});

// No candidate listing under `policy`: no current evidence either way.
export const candidateLookupUnavailable = (policy, error) =>
  unresolvedIntegratedState(policy, "candidate_lookup_unavailable", {
    detail: failureDetail(error),
  });

// One poll's judge: a revision's record with `integrated` replaced, present
// only under a declared `policy` for an eligible own state. `listCandidates()`
// lists this poll's candidate runs; `readCurrentAttempt(run, { jobs })` reads
// one listed run's current facts with its `check` and `context`, once per poll.
// `signal` is the observation's own.
export function integratedStateJudge(policy, poll) {
  const reads = new Map();
  const ancestryOf = candidateAncestry(poll);
  return async (revision) => {
    const judged = { ...revision };
    delete judged.integrated;
    if (policy && integratedEligible(judged))
      judged.integrated = await observeIntegratedState({
        ...poll,
        policy,
        reads,
        ancestryOf,
        acceptedSha: judged.sha,
      });
    return judged;
  };
}

// Judges B's integrated state from this poll's candidate runs, in provider
// order.
async function observeIntegratedState({
  acceptedSha,
  branch,
  policy,
  repoDir,
  listCandidates,
  readCurrentAttempt,
  reads,
  ancestryOf,
  signal,
}) {
  const unresolved = (reason, extra) =>
    unresolvedIntegratedState(policy, reason, extra);
  if (!readCurrentAttempt || !repoDir)
    return unresolved("current_attempt_unavailable");
  let runs;
  try {
    runs = await listCandidates();
  } catch (error) {
    return candidateLookupUnavailable(policy, lookupFailure(error, signal));
  }

  const bySha = new Map();
  for (const run of runs) {
    const sha = run.headSha?.toLowerCase();
    if (!isFullGitRevision(sha) || sha === acceptedSha) continue;
    if (run.headBranch !== branch) continue;
    // A listing that names its check is a candidate only for the declared one.
    if (run.check !== undefined && run.check !== policy.check) continue;
    const listed = bySha.get(sha) ?? [];
    const sameListing = (other) =>
      other.databaseId === run.databaseId &&
      other.attempt === run.attempt &&
      other.status === run.status &&
      other.conclusion === run.conclusion;
    if (!listed.some(sameListing)) listed.push(run);
    bySha.set(sha, listed);
  }

  const considered = new Map();
  let progress;
  let verifications = 0;
  // Preserve successful candidates' verification opportunity before spending
  // the same bounded reads on pending progress. Diagnostics keep provider order.
  const candidates = [...bySha].sort(
    ([, left], [, right]) =>
      Number(right.every(successfulAttempt)) -
      Number(left.every(successfulAttempt)),
  );
  for (const [sha, listed] of candidates) {
    // Every listed attempt of C must be a current success: a pending or
    // failed rerun contradicts an older green of the same snapshot. Such a C
    // cannot validate whatever its ancestry, so it is never fetched.
    const current = listed.every(successfulAttempt);
    const pending = listed.every(({ status }) => status !== "completed");
    const ancestry = await ancestryOf(acceptedSha, sha, { fetch: current });
    if (ancestry === "not_ancestor") continue;
    const consider = (reason) => considered.set(sha, reason);
    if (!current && !(pending && ancestry === "ancestor")) {
      consider("not_current_success");
      continue;
    }
    if (ancestry === "unknown") {
      consider("ancestry_unknown");
      continue;
    }
    if (verifications >= verificationBound) {
      consider("verification_bound");
      continue;
    }
    verifications += 1;
    const runIds = [...new Set(listed.map(({ databaseId }) => databaseId))];
    let reason;
    let verified;
    const attempts = pending
      ? listed
      : runIds.map((runId) =>
          listed.find(({ databaseId }) => databaseId === runId),
        );
    for (const run of attempts) {
      const key = String(run.databaseId);
      if (!reads.has(key))
        reads.set(
          key,
          Promise.resolve()
            .then(() => readCurrentAttempt(run, { jobs: policy.jobs }))
            .catch((error) => ({ unavailable: lookupFailure(error, signal) })),
        );
      const current = await reads.get(key);
      if (current.unavailable) {
        reason = "current_attempt_unavailable";
        break;
      }
      reason = currentAttemptIneligibility(
        run,
        sha,
        current,
        { branch, policy },
        pending,
      );
      if (reason) break;
      verified ??= current;
    }
    if (reason) {
      consider(reason);
      continue;
    }
    const ancestryEvidence = {
      ancestor: acceptedSha,
      descendant: sha,
      basis: ancestryBasis,
    };
    if (pending) {
      progress ??= {
        ...currentAttemptFacts(sha, verified, policy, true),
        ancestry: ancestryEvidence,
      };
      consider("not_current_success");
      continue;
    }
    return {
      state: "success",
      policy,
      validated: currentAttemptFacts(sha, verified, policy),
      ancestry: ancestryEvidence,
    };
  }
  if (!considered.size) return unresolved("no_candidate");
  return unresolved("no_eligible_candidate", {
    considered: [...bySha.keys()]
      .filter((sha) => considered.has(sha))
      .slice(0, consideredBound)
      .map((sha) => ({ sha, reason: considered.get(sha) })),
    ...(progress ? { progress } : {}),
  });
}
