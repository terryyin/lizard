// One snapshot of the project's CI settings when observation starts.
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { integratedStatePolicy } from "./ci-integrated-state-policy.mjs";

const configurationPath = ".planning/open-dough.json";

export function readCiConfiguration(root = process.cwd()) {
  const path = join(root, configurationPath);
  let configuration = {};
  if (existsSync(path)) {
    try {
      configuration = JSON.parse(readFileSync(path, "utf8"));
    } catch (error) {
      throw new Error(
        `Could not read CI settings from ${path}: ${error.message}`,
        { cause: error },
      );
    }
    if (
      !configuration ||
      typeof configuration !== "object" ||
      Array.isArray(configuration)
    )
      throw new Error(`${path} must contain a JSON object of project settings`);
  }
  const command = configuration.ciAdapter;
  if (
    command !== undefined &&
    (!Array.isArray(command) ||
      command.some((argument) => typeof argument !== "string" || !argument))
  )
    throw new Error(
      `ciAdapter in ${path} must be an empty or nonempty string array`,
    );
  const adapter = command?.length ? command : undefined;
  const workflow = adapter ? undefined : configuration.ciWorkflow;
  if (
    workflow !== undefined &&
    (typeof workflow !== "string" || !workflow.trim())
  )
    throw new Error(
      `ciWorkflow in ${path} must be a nonempty string naming a GitHub Actions workflow filename or numeric ID`,
    );
  return {
    adapter,
    workflow: process.env.DOUGH_CI_WORKFLOW ?? workflow ?? "ci.yml",
    integratedPolicy: integratedStatePolicy(configuration),
  };
}
