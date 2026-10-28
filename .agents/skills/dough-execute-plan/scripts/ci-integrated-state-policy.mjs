// The project's declared integrated-state completion policy
// (ci-integrated-state.mjs judges evidence under it).
const integratedStatePolicyKey = "integratedStateCompletion";

const nonemptyString = (value) => typeof value === "string" && value.length > 0;

// The declared policy, or undefined when the project declares none (exact
// completion), from the observer's startup configuration snapshot.
export function integratedStatePolicy(configuration) {
  const policy = configuration[integratedStatePolicyKey];
  if (policy === undefined) return;
  const { check, context, jobs } = policy ?? {};
  if (
    !nonemptyString(check) ||
    !nonemptyString(context) ||
    (jobs !== undefined &&
      !(Array.isArray(jobs) && jobs.length && jobs.every(nonemptyString)))
  )
    throw new Error(
      `${integratedStatePolicyKey} must be {"check": STRING, "context": STRING} with optional nonempty "jobs" string array`,
    );
  return { check, context, ...(jobs ? { jobs: [...jobs] } : {}) };
}

export const successfulAttempt = ({ status, conclusion }) =>
  status === "completed" && conclusion === "success";

// Why `current`, the provider's authoritative current attempt of the listed
// `run` at `sha`, cannot supply integrated evidence or progress, or undefined.
export function currentAttemptIneligibility(
  run,
  sha,
  current,
  { branch, policy },
  pending = false,
) {
  if (
    String(current.databaseId) !== String(run.databaseId) ||
    current.headSha?.toLowerCase() !== sha
  )
    return "facts_mismatch";
  if (current.attempt !== run.attempt || current.contradicted)
    return "attempt_changed";
  if (current.headBranch !== branch) return "different_target";
  if (current.check !== policy.check || current.context !== policy.context)
    return "context_incompatible";
  if (pending)
    return current.status !== "completed" ? undefined : "not_current_pending";
  if (!successfulAttempt(current)) return "not_current_success";
  for (const name of policy.jobs ?? []) {
    const jobs = (current.jobs ?? []).filter((job) => job.name === name);
    if (jobs.length !== 1) return "missing_check";
    if (!successfulAttempt(jobs[0])) return "check_not_successful";
  }
  return undefined;
}

export function currentAttemptFacts(sha, current, policy, pending = false) {
  return {
    sha,
    branch: current.headBranch,
    runId: current.databaseId,
    attempt: current.attempt,
    url: current.url,
    check: current.check,
    context: current.context,
    ...(!pending && policy.jobs
      ? {
          jobs: policy.jobs.map((name) => {
            const job = current.jobs.find((each) => each.name === name);
            return {
              name,
              jobId: job.databaseId,
              conclusion: job.conclusion,
              url: job.url,
            };
          }),
        }
      : {}),
  };
}
