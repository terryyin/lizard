// A preparation-assignment operation's request: the fields each operation
// needs, resolved paths, the session choices a start may take, and the
// publication authority and separate workspace that publishing requires.
import { basename, dirname, join, resolve } from "node:path";
import {
  agentProfileDirectory,
  agentReportError,
  profileAgentName,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import {
  needsPublicationAuthority,
  oneShotOnlyChoices,
  sessionPolicy,
  sessionPolicyChoices,
} from "../../dough-execute-plan/scripts/session-policy.mjs";
import { backlogPath } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import { defaultCheckoutRequest } from "../../dough-execute-plan/scripts/workspace-publication-select.mjs";
import { stop } from "./preparation-assignment-ownership.mjs";

// Operations that publish to trunk need authority and a separate workspace.
const publishing = new Set(["start", "abandon"]);

// Why the request's session choices cannot apply to `operation`, or
// undefined when they can: only `start` takes them, and the default checkout
// and automatic landing apply to one-shot preparation.
function sessionPolicyError(operation, policy) {
  const chosen = Object.entries(sessionPolicyChoices).filter(
    ([choice, { values }]) => policy[choice] !== values[0],
  );
  if (chosen.length === 0) return undefined;
  if (operation !== "start")
    return `session options (${chosen.map(([, { flag }]) => flag).join(", ")}) apply only to start`;
  const unsupported = oneShotOnlyChoices(policy)[0];
  if (unsupported)
    return `${sessionPolicyChoices[unsupported].flag} applies to one-shot preparation; ${
      unsupported === "workspace"
        ? "an assigned preparation publishes its announcement from a separate owned workspace"
        : "an assigned preparation lands through its keep"
    }`;
  return undefined;
}

// Why an abandonment addressed by profile path cannot be read as one, or
// undefined when it can: a lost workspace's assignment is named by its
// profile beside the backlog and its allocation, not by workspace or story.
function addressingError(input) {
  if (input.workspace !== undefined || input.identity !== undefined)
    return "--profile addresses an assignment by its allocation; do not also name --workspace or --identity";
  const directory = join(dirname(backlogPath), agentProfileDirectory);
  if (
    dirname(input.profile) !== directory ||
    !profileAgentName(basename(input.profile))
  )
    return `--profile must name an agent profile under ${directory}/`;
  return undefined;
}

// The validated request for an operation, or a stop saying why not.
export function requestOf(operation, input) {
  const addressed = operation === "abandon" && input.profile !== undefined;
  // A lost workspace's assignment is ended from the integration checkout; an
  // existing owned workspace supplies its own repository access, a supplied
  // repository context (an owned worktree or the common Git directory) only
  // gives Git access, and a supplied integration checkout also gets a local
  // refresh.
  const required = addressed
    ? ["profile", "target", "integration"]
    : ["workspace", "identity", "target"];
  for (const field of required)
    if (!input[field])
      return stop("invalid-request", { error: `missing ${field}` });
  const invalid = addressed && addressingError(input);
  if (invalid) return stop("invalid-request", { error: invalid });
  const request = {
    ...input,
    remote: input.remote ?? "origin",
    ...(input.workspace ? { workspace: resolve(input.workspace) } : {}),
    ...(input.integration ? { integration: resolve(input.integration) } : {}),
    ...(input.repository ? { repository: resolve(input.repository) } : {}),
  };
  const reportError = agentReportError(request);
  if (reportError) return stop("invalid-request", { error: reportError });
  const policy = sessionPolicy(request);
  const policyError = sessionPolicyError(operation, policy);
  if (policyError) return stop("invalid-request", { error: policyError });
  // A one-shot start publishes nothing, so it needs no publication authority
  // unless its result lands automatically.
  const publishes =
    publishing.has(operation) && needsPublicationAuthority(policy);
  if (publishes && request.pushAuthorized !== true)
    return stop("authority-required", {
      error: "trunk publication authority must be established",
    });
  if (policy.workspace === "default-checkout") {
    const located = defaultCheckoutRequest(request);
    if (located.error) return stop("invalid-request", { error: located.error });
    return { ok: true, request: located.request };
  }
  if (!publishing.has(operation)) return { ok: true, request };
  if (request.integration === request.workspace)
    return stop("invalid-request", {
      error: "preparation requires a separate owned workspace",
    });
  return { ok: true, request };
}
