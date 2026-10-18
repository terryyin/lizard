import { isFullGitRevision } from "./ci-revisions.mjs";

// The provider's attempts for the requested `branch`, in provider order. An
// attempt naming another `branch` belongs to another target and is dropped;
// one naming none keeps the legacy label of the requested branch but never
// attests it (`providerBranch` stays absent).
export function targetCommandAttempts(attempts, branch) {
  return attempts
    .map((attempt) => normalizeAttempt(attempt, branch))
    .filter(
      ({ providerBranch }) =>
        providerBranch === undefined || providerBranch === branch,
    );
}

// One snapshot's check: its SHA plus any check and context the provider names,
// so a later attempt of one check never supersedes another check's attempt.
const checkKey = (run) =>
  JSON.stringify([run.headSha.toLowerCase(), run.check, run.context]);

// Comparable timestamps identify superseded attempts without interpreting
// provider identities. Missing timestamps keep that revision's existing
// selection semantics; equal timestamps do not invent an ordering either.
export function latestCommandAttempts(runs) {
  const latest = new Map();
  for (const run of runs) {
    const key = checkKey(run);
    latest.set(
      key,
      Math.max(latest.get(key) ?? -Infinity, Date.parse(run.createdAt)),
    );
  }
  return runs.filter((run) => {
    const time = latest.get(checkKey(run));
    return !Number.isFinite(time) || Date.parse(run.createdAt) === time;
  });
}

// Integrated-state facts for listed `run` from the same discovery's `polled`
// attempts (provider order, newest first). Its branch, check and context are
// only what the provider named. It is the current attempt of its check only
// when listed first and no other attempt's time is equal or later; otherwise
// the facts are contradicted, so an equal or missing time never resolves to an
// older green.
export function currentCommandAttempt(polled, run) {
  const [first, ...rest] = polled.filter(
    (each) => checkKey(each) === checkKey(run),
  );
  const time = Date.parse(first?.createdAt);
  const contradicted =
    first?.databaseId !== run.databaseId ||
    first.attempt !== run.attempt ||
    rest.some((other) => {
      const otherTime = Date.parse(other.createdAt);
      return Number.isFinite(otherTime) && !(time > otherTime);
    });
  const { providerBranch, ...facts } = run;
  return {
    ...facts,
    headBranch: providerBranch,
    ...(contradicted ? { contradicted } : {}),
  };
}

const optionalName = (value) =>
  value === undefined || (typeof value === "string" && value.length > 0);

function normalizeAttempt(attempt, branch) {
  if (
    !attempt ||
    !["string", "number"].includes(typeof attempt.runId) ||
    !["string", "number"].includes(typeof attempt.attemptId) ||
    !isFullGitRevision(attempt.sha) ||
    !["pending", "success", "failure", "incomplete"].includes(
      attempt.outcome,
    ) ||
    ![attempt.branch, attempt.check, attempt.context].every(optionalName)
  )
    throw new Error("CI adapter returned an invalid attempt");

  const completed = attempt.outcome !== "pending";
  return {
    databaseId: attempt.runId,
    attempt: attempt.attemptId,
    headSha: attempt.sha,
    headBranch: attempt.branch ?? branch,
    status: completed ? "completed" : "in_progress",
    conclusion:
      attempt.outcome === "incomplete"
        ? "cancelled"
        : completed
          ? attempt.outcome
          : null,
    ...(attempt.url === undefined ? {} : { url: attempt.url }),
    ...(attempt.time === undefined ? {} : { createdAt: attempt.time }),
    ...(attempt.branch === undefined ? {} : { providerBranch: attempt.branch }),
    ...(attempt.check === undefined ? {} : { check: attempt.check }),
    ...(attempt.context === undefined ? {} : { context: attempt.context }),
  };
}
