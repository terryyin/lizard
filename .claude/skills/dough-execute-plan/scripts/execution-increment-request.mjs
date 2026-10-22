// Resolve fresh/delivery CLI inputs and validate the managed delivery contract.
// Existing-candidate consumers need no fresh preparation or commit resources.
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { withExplicitSession } from "./ci-host-bridge.mjs";
import { flagValues } from "./ci-direct-entry.mjs";

// Why a delivery request is refused before anything is fetched, pushed, or
// observed: a missing field, or a Story Branch increment aimed anywhere but its
// execution branch without declaring a one-shot landing on trunk.
export function managedIncrementRequestRefusal(request) {
  for (const field of [
    "workspace",
    "branch",
    "previouslyPublishedBase",
    "targetRef",
    "repo",
  ]) {
    if (!request[field]) return `missing ${field}`;
  }
  const { mode, tracking, branch, targetRef } = request;
  const executionTarget = `refs/heads/${branch}`;
  if (
    mode === "story-branch" &&
    tracking !== "one-shot" &&
    targetRef !== executionTarget
  ) {
    return `Story Branch Mode delivers only to its execution branch: use --target-ref ${executionTarget}, not ${targetRef}; a one-shot landing declares --tracking one-shot`;
  }
  return null;
}

const usage =
  "usage: execution-increment-delivery.mjs fresh --mode trunk|story-branch|current-branch --workspace PATH --branch NAME --authority publish|local-only --preparation-file JSON --message-file FILE\n" +
  "       execution-increment-delivery.mjs deliver --mode trunk|story-branch --workspace PATH --branch NAME --previously-published-base COMMIT_ID --target-ref refs/heads/<branch> --repo OWNER/REPO [--tracking one-shot] [--host cursor|claude|codex] [--preferred-alias .agents|.claude] [--authority publish|local-only] [--session-json JSON] [--coordinator VALUE --observer-directory PATH] [--max-duration-ms MS] [--validated-candidate SHA] [--default-checkout PATH] [--one-shot-identity ID] [--landing-context PATH]\n" +
  "       COMMIT_ID is a full or unique abbreviated commit ID, not a ref name; input naming no single commit stops as unresolvable-base";

function argumentsOf(argv) {
  if (!["deliver", "fresh"].includes(argv[0])) throw new Error(usage);
  const result = flagValues(argv.slice(1), { authority: "publish" });
  if (
    ![
      "trunk",
      "story-branch",
      ...(argv[0] === "fresh" ? ["current-branch"] : []),
    ].includes(result.mode)
  )
    throw new Error(usage);
  if (result.maxDurationMs !== undefined) {
    result.maxDurationMs = Number(result.maxDurationMs);
  }
  return withExplicitSession(result);
}

export async function readIncrementCliRequest(argv) {
  const args = argumentsOf(argv);
  const fresh = argv[0] === "fresh";
  if (fresh && (!args.workspace || !args.preparationFile || !args.messageFile))
    throw new Error(
      "fresh requires --workspace, --preparation-file, and --message-file",
    );
  const local = fresh
    ? {
        preparation: JSON.parse(
          await readFile(resolve(args.workspace, args.preparationFile), "utf8"),
        ),
        message: await readFile(
          resolve(args.workspace, args.messageFile),
          "utf8",
        ),
      }
    : {};
  return {
    fresh,
    request: {
      ...args,
      ...local,
      workspace: resolve(args.workspace),
      defaultCheckout: args.defaultCheckout
        ? resolve(args.defaultCheckout)
        : undefined,
    },
  };
}
