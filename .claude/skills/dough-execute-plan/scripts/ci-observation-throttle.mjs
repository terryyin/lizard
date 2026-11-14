import { GitHubAcquisitionHeld } from "./ci-github-actions.mjs";

// A wait GitHub directed suspends acquisition until it ends, or until the
// budget does: reported once per suspension, it is no observation error, and
// a registration cannot shorten it.
export function createAcquisitionSuspension({
  emit,
  repo,
  branch,
  workflow,
  now,
  sleep,
  maxDurationMs,
  startedAt,
  observationSignal,
}) {
  let suspensionReported = false;
  return {
    held: (error) => error instanceof GitHubAcquisitionHeld,
    clearReport: () => {
      suspensionReported = false;
    },
    suspend: async ({ waitSeconds }) => {
      const waitMs = waitSeconds * 1000;
      if (!suspensionReported) {
        suspensionReported = true;
        await emit({
          type: "CI_MONITOR_THROTTLED",
          repo,
          branch,
          workflow,
          waitSeconds,
          resumesAt: new Date(now() + waitMs).toISOString(),
        });
      }
      const remainingMs = maxDurationMs - (now() - startedAt);
      await sleep(Math.max(Math.min(waitMs, remainingMs), 0), undefined, {
        signal: observationSignal,
      });
    },
  };
}
