#!/usr/bin/env node
// Installed CLI for a queued story's preparation assignment. `start` publishes
// the Preparing announcement before substantive preparation, or continues the
// workspace's existing one, first creating a missing workspace on `--branch`
// at fetched trunk; `release` stages removal of exactly that
// assignment beside the retained result so one landing publishes both;
// `abandon` publishes its end alone, leaving the draft in the workspace, or,
// addressed by profile and allocation from the integration checkout, ends a
// lost workspace's assignment once the developer confirms it abandoned.
// `continue` is for a retained start: verify the existing workspace/branch and
// allocation, optionally against its saved receipt, without ever announcing.
// `start --one-shot` establishes the workspace, or with `--default-main` takes
// the default checkout as it is, and publishes nothing; `recheck` checks on
// fetched trunk that no other owner has taken up its story before its result
// is pushed.
import { isDirectCliEntry } from "../../dough-execute-plan/scripts/ci-direct-entry.mjs";
import {
  sessionPolicy,
  sessionPolicyToggles,
} from "../../dough-execute-plan/scripts/session-policy.mjs";
import { abandonPreparation } from "./preparation-assignment-abandon.mjs";
import { releasePreparation } from "./preparation-assignment-release.mjs";
import { startPreparation } from "./preparation-assignment-start.mjs";
import { recheckOneShotPreparation } from "./preparation-one-shot-recheck.mjs";
import { startOneShotPreparation } from "./preparation-one-shot-start.mjs";

const operations = {
  start: (input) =>
    sessionPolicy(input).tracking === "one-shot"
      ? startOneShotPreparation(input)
      : startPreparation(input),
  continue: (input) => startPreparation({ ...input, continueOnly: true }),
  release: releasePreparation,
  abandon: abandonPreparation,
  recheck: recheckOneShotPreparation,
};

const usage =
  "usage: preparation-assignment.mjs start [--integration PATH] [--repository PATH] --workspace PATH [--branch NAME] --identity ID --remote NAME --target BRANCH --push-authorized [--host claude|codex|cursor] [--model TEXT]\n" +
  "       preparation-assignment.mjs continue --workspace PATH --branch NAME --identity ID --remote NAME --target BRANCH --push-authorized [--expected-agent NAME] [--expected-allocation SHA]\n" +
  "       preparation-assignment.mjs start --one-shot [--default-main] [--integration PATH] [--repository PATH] --workspace PATH [--branch NAME] --identity ID --remote NAME --target BRANCH [--auto-land --push-authorized]\n" +
  "       preparation-assignment.mjs recheck --workspace PATH --identity ID --remote NAME --target BRANCH\n" +
  "       preparation-assignment.mjs release --workspace PATH --identity ID --remote NAME --target BRANCH\n" +
  "       preparation-assignment.mjs abandon [--integration PATH] --workspace PATH --identity ID --remote NAME --target BRANCH --push-authorized\n" +
  "       preparation-assignment.mjs abandon --integration PATH --profile PATH [--allocation SHA --confirmed-abandoned] --remote NAME --target BRANCH --push-authorized";

// Flags that stand alone: authority, confirmation, and session choices the
// developer supplied.
const switches = {
  "--push-authorized": "pushAuthorized",
  "--confirmed-abandoned": "confirmedAbandoned",
  ...sessionPolicyToggles,
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
