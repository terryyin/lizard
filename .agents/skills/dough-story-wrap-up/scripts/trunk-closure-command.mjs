// Adapt the `trunk-closure.mjs finish` command line to finishTrunkClosure.
import { resolve } from "node:path";
import {
  rejectRetiredObserverFlags,
  withExplicitSession,
} from "../../dough-execute-plan/scripts/ci-host-bridge.mjs";
import { targetBranchName } from "../../dough-execute-plan/scripts/publication-git.mjs";

const required = [
  "workspace",
  "branch",
  "beforeCleanup",
  "final",
  "previouslyPublishedBase",
  "targetRef",
  "repo",
  "host",
];
const usage =
  "usage: trunk-closure.mjs finish --workspace PATH --branch NAME --before-cleanup SHA --final SHA --previously-published-base SHA --target-ref REF --repo OWNER/REPO --host cursor|claude|codex [--remote NAME] [--repository PATH] [--identity WORK] [--created-for-work] [--admitted-launch-context PATH] [--session-json JSON] [--coordinator VALUE --observer-directory PATH] [--default-checkout PATH] [--supplier-identity IDENTITY [--visit-settled SHA] [--backlog-path PATH]]";

function finishArguments(argv) {
  if (argv[0] !== "finish") {
    throw new Error(usage);
  }
  const result = {};
  for (let index = 1; index < argv.length; index += 1) {
    const flag = argv[index];
    if (flag === "--created-for-work") {
      result.createdForWork = true;
      continue;
    }
    if (!flag.startsWith("--") || index + 1 >= argv.length) {
      throw new Error(`invalid argument ${flag}\n${usage}`);
    }
    const key = flag
      .slice(2)
      .replace(/-[a-z]/g, (match) => match[1].toUpperCase());
    result[key] = argv[++index];
  }
  rejectRetiredObserverFlags(result, `\n${usage}`);
  for (const field of required) {
    if (!result[field]) {
      throw new Error(`missing ${field}\n${usage}`);
    }
  }
  if (result.visitSettled && !result.supplierIdentity) {
    throw new Error(`--visit-settled needs --supplier-identity\n${usage}`);
  }
  targetBranchName(result.targetRef);
  return withExplicitSession(result);
}

export async function runFinishCommand(argv, finish, onFailure) {
  let args;
  try {
    args = finishArguments(argv);
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
    return;
  }

  let result;
  try {
    result = await finish({
      ...args,
      workspace: resolve(args.workspace),
      repository: args.repository ? resolve(args.repository) : undefined,
      defaultCheckout: args.defaultCheckout
        ? resolve(args.defaultCheckout)
        : undefined,
    });
  } catch (error) {
    result = onFailure(error);
  }
  if (result.startReceipt) process.stdout.write(result.startReceipt);
  process.stdout.write(`${JSON.stringify(result)}\n`);
  if (!result.ok) process.exitCode = 1;
}
