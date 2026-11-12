#!/usr/bin/env node
// Local project configuration and the public shared test-rule boundary.
import { execFileSync } from "node:child_process";
import { parseArgs } from "node:util";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { changedForRefactoring, currentSourcePaths } from "./git-changes.mjs";
import {
  isSafeRepositoryPath,
  selectTestEntryRules,
} from "./test-entry-rules-runtime.mjs";
import { readTestEntrySettings } from "./test-entry-settings.mjs";

export { selectTestEntryRules } from "./test-entry-rules-runtime.mjs";

export async function testEntryRules(project, sourcePaths) {
  const settings = readTestEntrySettings(project);
  if (settings.kind === "absent") {
    return {
      ...selectTestEntryRules("{}", sourcePaths),
      configuration: "absent",
    };
  }
  if (settings.kind === "unreadable") {
    return { status: "unresolved", problem: "unreadable", sources: [] };
  }
  return selectTestEntryRules(settings.text, sourcePaths);
}

export async function changedTestEntryRules(
  project,
  committed = [],
  paths = [],
) {
  const git = (root, args) =>
    execFileSync("git", ["-C", root, ...args], {
      encoding: "utf8",
      stdio: ["ignore", "pipe", "pipe"],
    });
  const changed = [
    ...new Set([...changedForRefactoring(git, project, committed), ...paths]),
  ].sort();
  if (changed.some((path) => !isSafeRepositoryPath(path)))
    throw new Error("Expected project-relative test source paths");
  const sources = currentSourcePaths(project, changed);
  return testEntryRules(project, sources);
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const { values } = parseArgs({
      options: {
        project: { type: "string" },
        source: { type: "string", multiple: true },
        changed: { type: "boolean" },
        committed: { type: "string", multiple: true },
        path: { type: "string", multiple: true },
      },
    });
    if (
      !values.project ||
      (!values.changed && !values.source?.length) ||
      (values.changed && values.source?.length) ||
      ((values.committed?.length || values.path?.length) && !values.changed)
    ) {
      throw new Error(
        "usage: test-entry-rules.mjs --project <root> (--source <path>... | --changed [--committed <revision-or-range>...] [--path <source>...])",
      );
    }
    const result = values.changed
      ? await changedTestEntryRules(
          values.project,
          values.committed,
          values.path,
        )
      : await testEntryRules(values.project, values.source);
    console.log(JSON.stringify(result));
    if (result.status === "unresolved") process.exitCode = 1;
  } catch (error) {
    console.error(error.message);
    process.exitCode = 2;
  }
}
