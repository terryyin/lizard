#!/usr/bin/env node
// Installed CLI for authorized queued startup, admission of accepted work
// that no backlog list holds yet, or preparation of one-shot work.
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { startExecution } from "./execution-start-operation.mjs";

export { startExecution } from "./execution-start-operation.mjs";

function argumentsOf(argv) {
  if (argv[0] !== "start")
    throw new Error(
      "usage: execution-start.mjs start --integration PATH --workspace PATH --branch NAME [--identity ID] [--publisher-id ID] --mode trunk|story-branch --remote NAME --target BRANCH --push-authorized --workspace-authorized [--admit --link HREF --title TEXT [--carry] | --one-shot] [--plan PATH] [--host claude|codex|cursor] [--model TEXT] [--declared-owner ID --requester ID] [--starting-revision SHA --candidate-sha SHA]",
    );
  const result = {};
  for (let index = 1; index < argv.length; index += 1) {
    const flag = argv[index];
    const toggle = {
      "--push-authorized": "pushAuthorized",
      "--workspace-authorized": "workspaceAuthorized",
      "--admit": "admit",
      "--carry": "carry",
      "--one-shot": "oneShot",
    }[flag];
    if (toggle) {
      result[toggle] = true;
      continue;
    }
    if (!flag.startsWith("--") || index + 1 >= argv.length)
      throw new Error(`invalid argument ${flag}`);
    const key = flag
      .slice(2)
      .replace(/-[a-z]/g, (match) => match[1].toUpperCase());
    result[key] = argv[++index];
  }
  return result;
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const result = await startExecution(argumentsOf(process.argv.slice(2)));
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
