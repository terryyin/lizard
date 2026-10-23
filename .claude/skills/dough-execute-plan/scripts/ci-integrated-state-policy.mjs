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
