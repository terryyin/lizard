// The `trunk-closure.mjs finish` command line: its flags as camel-cased
// `finishTrunkClosure` inputs, with the session made explicit. A usage error
// throws with the usage text.
import { withExplicitSession } from "../../dough-execute-plan/scripts/ci-host-bridge.mjs";
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
  "usage: trunk-closure.mjs finish --workspace PATH --branch NAME --before-cleanup SHA --final SHA --previously-published-base SHA --target-ref REF --repo OWNER/REPO --host cursor|claude|codex [--remote NAME] [--repository PATH] [--identity WORK] [--created-for-work] [--session-json JSON] [--coordinator VALUE --observer-directory PATH] [--preferred-alias .agents|.claude] [--default-checkout PATH] [--supplier-identity IDENTITY [--visit-settled SHA] [--backlog-path PATH]]";

export function finishArguments(argv) {
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
