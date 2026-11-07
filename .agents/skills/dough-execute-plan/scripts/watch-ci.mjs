import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { executionBudgetMs, watchCiExecution } from "./watch-ci-execution.mjs";

export { watchCiExecution };

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  const [mode, repo, branch, budget] = process.argv.slice(2);
  const maxDurationMs = budget ? Number(budget) : executionBudgetMs;
  if (!(
    mode === "--execution" &&
    repo &&
    typeof branch === "string" &&
    branch.length > 0 &&
    Number.isFinite(maxDurationMs) &&
    maxDurationMs > 0
  )) {
    throw new Error(
      "Usage: node watch-ci.mjs --execution OWNER/REPO BRANCH [BUDGET_MS]",
    );
  }
  const controller = new AbortController();
  const abort = () => controller.abort();
  process.once("SIGINT", abort);
  process.once("SIGTERM", abort);
  try {
    await watchCiExecution({
      repo,
      branch,
      maxDurationMs,
      signal: controller.signal,
      emit: (event) => process.stdout.write(`${JSON.stringify(event)}\n`),
    });
  } finally {
    process.removeListener("SIGINT", abort);
    process.removeListener("SIGTERM", abort);
  }
}
