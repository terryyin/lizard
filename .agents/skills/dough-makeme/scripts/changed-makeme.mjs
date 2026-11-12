// Public refactor scope: discover every change path, then apply the convention
// only to sources that currently exist. Old move paths remain context, never a
// reason to convert a deletion or an opted-out destination.
import { spawnSync } from "node:child_process";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import {
  currentSourcePaths,
  repositoryChangePaths,
} from "../../dough-execute-plan/scripts/git-changes.mjs";
import { isSafeRepositoryPath } from "./repository-path.mjs";
import { selectMakeMe } from "./select-makeme.mjs";

function git(root, args) {
  const result = spawnSync("git", ["-C", root, ...args], { encoding: "utf8" });
  if (result.error || result.status !== 0) {
    throw new Error(
      result.error?.message ??
        result.stderr.trim() ??
        "Git scope could not be read.",
    );
  }
  return result.stdout;
}

/** @param {string} projectPath
 * @param {{ committed?: string[], paths?: string[] }} [options]
 */
export function selectChangedMakeMe(
  projectPath,
  { committed = [], paths = [] } = {},
) {
  const project = resolve(projectPath);
  try {
    if (committed.some((range) => !range || range.startsWith("-")))
      throw new Error("Supply committed revision ranges, not Git options.");
    const layers = repositoryChangePaths(git, project, "HEAD", committed);
    const changedPaths = [
      ...new Set([...Object.values(layers).flat(), ...paths]),
    ].sort();
    if (changedPaths.some((path) => !isSafeRepositoryPath(path))) {
      return {
        status: "unresolved",
        reason: "invalid-test-path",
        project,
        message: "Scope contains an unsafe repository-relative path.",
      };
    }
    const currentPaths = currentSourcePaths(project, changedPaths);
    return {
      ...selectMakeMe(project, currentPaths, { allowEmpty: true }),
      changedPaths,
      currentPaths,
    };
  } catch (error) {
    return {
      status: "unresolved",
      reason: "changed-scope-unreadable",
      project,
      message: error.message,
    };
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const args = process.argv.slice(2);
  const options = { committed: [], paths: [] };
  let valid = args[0] === "--project" && Boolean(args[1]);
  for (let index = 2; index < args.length && valid; index += 2) {
    const key =
      args[index] === "--committed"
        ? "committed"
        : args[index] === "--path"
          ? "paths"
          : undefined;
    if (key === undefined || !args[index + 1]) valid = false;
    else options[key].push(args[index + 1]);
  }
  if (!valid) {
    process.stderr.write(
      "usage: changed-makeme.mjs --project PROJECT [--committed RANGE]... [--path TEST_SOURCE]...\n",
    );
    process.exitCode = 2;
  } else {
    const result = selectChangedMakeMe(args[1], options);
    process.stdout.write(`${JSON.stringify(result)}\n`);
    process.exitCode = result.status === "unresolved" ? 1 : 0;
  }
}
