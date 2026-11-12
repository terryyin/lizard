// Public installed entry selection. No target-project npm dependency is needed.
import { fileURLToPath } from "node:url";
import { readTestEntrySettings } from "../../dough-execute-plan/scripts/test-entry-settings.mjs";
import {
  isSafeRepositoryPath,
  selectMakeMeEntries,
} from "../../dough-execute-plan/scripts/test-entry-rules-runtime.mjs";

/** @param {string} project @param {readonly string[]} paths
 * @param {{ allowEmpty?: boolean }} [options] Changed scope can contain no current files.
 */
export function selectMakeMe(project, paths, { allowEmpty = false } = {}) {
  const settings = readTestEntrySettings(project);
  const { configuration } = settings;
  const invalid = paths.find((path) => !isSafeRepositoryPath(path));
  if ((!allowEmpty && paths.length === 0) || invalid !== undefined)
    return {
      status: "unresolved",
      reason: "invalid-test-path",
      configuration,
      message: "Supply repository-relative test source paths.",
    };
  if (settings.kind !== "text") {
    return settings.kind === "absent"
      ? {
          status: "not-selected",
          reason: "configuration-absent",
          configuration,
          files: [],
          entries: [],
        }
      : {
          status: "unresolved",
          reason: "configuration-unreadable",
          configuration,
          message: settings.message,
        };
  }
  const selection = selectMakeMeEntries(settings.text, paths);
  if (selection.status === "unresolved")
    return {
      status: "unresolved",
      reason: "configuration-malformed",
      configuration,
      message: "Configuration is not readable JSON.",
    };
  const { entries, files } = selection;
  return {
    status: files.length > 0 ? "selected" : "not-selected",
    configuration,
    files,
    entries,
    ...(files.length > 0
      ? {
          convention: fileURLToPath(
            new URL("../references/convention.md", import.meta.url),
          ),
        }
      : { reason: "no-enabled-source" }),
  };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const args = process.argv.slice(2);
  if (args[0] !== "--project" || !args[1] || args[2] !== "--") {
    process.stderr.write(
      "usage: select-makeme.mjs --project PROJECT -- TEST_SOURCE...\n",
    );
    process.exitCode = 2;
  } else {
    const result = selectMakeMe(args[1], args.slice(3));
    process.stdout.write(`${JSON.stringify(result)}\n`);
    process.exitCode = result.status === "unresolved" ? 1 : 0;
  }
}
