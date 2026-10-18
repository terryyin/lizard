// The project's declared integrated-state completion policy
// (ci-integrated-state.mjs judges evidence under it).
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";

const configurationPath = ".planning/open-dough.json";
const integratedStatePolicyKey = "integratedStateCompletion";

const nonemptyString = (value) => typeof value === "string" && value.length > 0;

// The declared policy, or undefined when the project declares none (exact
// completion). Read once when the observer starts.
export function readIntegratedStatePolicy(root = process.cwd()) {
  const path = join(root, configurationPath);
  if (!existsSync(path)) return;
  const policy = JSON.parse(readFileSync(path, "utf8"))[
    integratedStatePolicyKey
  ];
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
