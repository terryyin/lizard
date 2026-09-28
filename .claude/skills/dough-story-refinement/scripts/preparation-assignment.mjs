#!/usr/bin/env node
// Installed CLI for a queued story's preparation assignment. `start` publishes
// the Preparing announcement before substantive preparation, or continues the
// workspace's existing one; `release` stages removal of exactly that
// assignment beside the retained result so one landing publishes both;
// `abandon` publishes its end alone, leaving the draft in the workspace, or,
// addressed by profile and allocation from the integration checkout, ends a
// lost workspace's assignment once the developer confirms it abandoned.
import { isDirectCliEntry } from "../../dough-execute-plan/scripts/ci-direct-entry.mjs";
import { abandonPreparation } from "./preparation-assignment-abandon.mjs";
import { releasePreparation } from "./preparation-assignment-release.mjs";
import { startPreparation } from "./preparation-assignment-start.mjs";

const operations = {
  start: startPreparation,
  release: releasePreparation,
  abandon: abandonPreparation,
};

const usage =
  "usage: preparation-assignment.mjs start --integration PATH --workspace PATH --identity ID --remote NAME --target BRANCH --push-authorized [--host claude|codex|cursor] [--model TEXT] [--declared-owner ID --requester ID]\n" +
  "       preparation-assignment.mjs release --workspace PATH --identity ID --remote NAME --target BRANCH\n" +
  "       preparation-assignment.mjs abandon --integration PATH --workspace PATH --identity ID --remote NAME --target BRANCH --push-authorized [--declared-owner ID --requester ID]\n" +
  "       preparation-assignment.mjs abandon --integration PATH --profile PATH [--allocation SHA --confirmed-abandoned] --remote NAME --target BRANCH --push-authorized [--declared-owner ID --requester ID]";

// Flags that stand alone: authority and confirmation the developer supplied.
const switches = {
  "--push-authorized": "pushAuthorized",
  "--confirmed-abandoned": "confirmedAbandoned",
};

function argumentsOf(argv) {
  const [operation, ...rest] = argv;
  if (!Object.hasOwn(operations, operation)) throw new Error(usage);
  const values = {};
  for (let index = 0; index < rest.length; index += 1) {
    const flag = rest[index];
    if (Object.hasOwn(switches, flag)) {
      values[switches[flag]] = true;
      continue;
    }
    if (!flag.startsWith("--") || index + 1 >= rest.length)
      throw new Error(`invalid argument ${flag}\n${usage}`);
    const key = flag
      .slice(2)
      .replace(/-[a-z]/g, (match) => match[1].toUpperCase());
    values[key] = rest[++index];
  }
  return { operation, values };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const { operation, values } = argumentsOf(process.argv.slice(2));
    const run = operations[operation];
    const result = await run(values);
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
