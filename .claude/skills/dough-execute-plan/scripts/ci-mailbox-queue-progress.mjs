import {
  ancestryBasis,
  candidateAncestry,
} from "./ci-integrated-state-ancestry.mjs";
import {
  effectiveRevisionState,
  revisionVerdicts,
} from "./ci-mailbox-revision-records.mjs";

// This poll's queue may explain pending work, never a completion verdict.
export function queueProgressReader(
  queue,
  { branch, integratedPolicy, repoDir, observedAt },
) {
  const ancestryOf = candidateAncestry({ repoDir });
  const eligible = queue.filter(
    (entry) =>
      entry.branch === branch &&
      (!integratedPolicy ||
        (entry.check === integratedPolicy.check &&
          entry.context === integratedPolicy.context)),
  );
  return async (revision) => {
    if (revisionVerdicts.has(effectiveRevisionState(revision))) return;
    const exact = eligible.find((entry) => entry.sha === revision.sha);
    if (exact) return { ...exact, observedAt };
    if (!integratedPolicy || !repoDir) return;
    for (const entry of eligible) {
      if (
        (await ancestryOf(revision.sha, entry.sha, { fetch: false })) ===
        "ancestor"
      )
        return {
          ...entry,
          observedAt,
          ancestry: {
            ancestor: revision.sha,
            descendant: entry.sha,
            basis: ancestryBasis,
          },
        };
    }
  };
}
