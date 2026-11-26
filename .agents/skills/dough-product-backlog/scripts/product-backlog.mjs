#!/usr/bin/env node
// Applies one explicitly selected product backlog change to the project's
// backlog file. It applies a decision; it never decides value, priority,
// prerequisites, completion, or who may execute the work.

import { parseArgs } from "node:util";
import {
  catalogDone,
  complete,
} from "./product-backlog-completion-command.mjs";
import {
  readConsumers,
  visitConsumers,
} from "./product-backlog-dependency-consumers.mjs";
import { resolveDependency } from "./product-backlog-dependency-resolution.mjs";
import { direction, refresh } from "./product-backlog-edit-command.mjs";
import { printIdentityEvidence } from "./product-backlog-identity-evidence.mjs";
import { add, place, take } from "./product-backlog-queue-command.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { options, resolvePath } from "./product-backlog-request.mjs";
import {
  readDependencies,
  updateDependency,
} from "./product-backlog-story-dependencies-command.mjs";
import {
  readState,
  recordState,
} from "./product-backlog-story-state-command.mjs";
import { preparationRefusal } from "./product-backlog-story-state-home.mjs";

const placement = "(--after <id> | --before <id> | --position first|last)";
const dependencyRequest =
  "--identity <id> --link <href> --dependency-file <path> --expect-dependencies <sha256>";
const acceptedSupplier =
  "--accepted-revision <sha> --remote <remote> --target <branch>";

// Operations on canonical story homes; their refusals say nothing was
// written rather than that the backlog was unchanged.
const preparationOperations = {
  "record-state": [
    recordState,
    "--identity <id> --link <href> --refinement not-refined|refined\n" +
      "    --approach unselected|planned|planless [--plan <home-relative-path>]\n" +
      "    [--assessment ready|not-ready --expect-document <sha256> [--expect-plan <sha256>] [--reason <text>...]]",
  ],
  "read-state": [readState, "--link <href>"],
  "read-dependencies": [readDependencies, "--link <href>"],
  "update-dependency": [updateDependency, dependencyRequest],
  "discover-consumers": [readConsumers, "--supplier-identity <id>"],
  "visit-consumers": [
    visitConsumers,
    `--supplier-identity <id>\n    ${acceptedSupplier}`,
  ],
  "resolve-dependency": [
    resolveDependency,
    `${dependencyRequest}\n    ${acceptedSupplier} [--correct-agreement]\n` +
      "    [--plan <backlog-relative-path>] [--planless-complete --completion-file <repo-relative-path>]",
  ],
};

const operations = {
  add: [add, `--identity <id> --title <title> --link <href>\n    ${placement}`],
  place: [place, `--identity <id> ${placement} [--return]`],
  take: [take, "--identity <id> (--plan <backlog-relative-path> | --no-plan)"],
  complete: [
    complete,
    "--identity <id> [--dropped | --intent-coverage <path> | --intent-coverage-unavailable <reason>]",
  ],
  "catalog-done": [catalogDone],
  refresh: [
    refresh,
    "--identity <id> [--title <title>] [--link <href>] [--plan <backlog-relative-path>]",
  ],
  direction: [
    direction,
    "(--text <text> | --clear) (--expect <text> | --expect-none)",
  ],
  "identity-evidence": [
    printIdentityEvidence,
    "[--identity <id>] [--seeds <path>]",
  ],
  ...preparationOperations,
};

const usage = [
  "Usage: product-backlog.mjs [--file <path>] <operation> [options]",
  "       product-backlog.mjs --help",
  "",
  ...Object.entries(operations).map(([name, [, form = ""]]) =>
    `  ${name} ${form}`.trimEnd(),
  ),
  "",
  "See dough-product-backlog/SKILL.md in the installed skills for procedures.",
].join("\n");

function unknownOperation(name) {
  return new BacklogError(`Unknown operation: ${name || "(none)"}\n\n${usage}`);
}

async function main(argv) {
  let parsed;
  try {
    parsed = parseArgs({ args: argv, options, allowPositionals: true });
  } catch (error) {
    const [command] = argv;
    if (
      command &&
      !command.startsWith("-") &&
      !Object.hasOwn(operations, command)
    ) {
      throw unknownOperation(command);
    }
    throw new BacklogError(`${error.message}\n\n${usage}`);
  }
  const { values, positionals } = parsed;

  if (values.help) {
    console.log(usage);
    return;
  }
  const named = positionals.length === 1 ? positionals[0] : "";
  if (!Object.hasOwn(operations, named)) {
    throw unknownOperation(positionals.join(" "));
  }
  const [run] = operations[named];
  await run(resolvePath(values.file), values);
}

try {
  await main(process.argv.slice(2));
} catch (error) {
  if (error instanceof BacklogError) {
    const refusal = Object.hasOwn(preparationOperations, process.argv[2])
      ? preparationRefusal(error)
      : error.refusal;
    console.error(refusal);
    process.exit(1);
  }
  throw error;
}
