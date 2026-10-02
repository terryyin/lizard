// Validates and normalizes a queued-start, admission or one-shot request
// before any Git work.
import { existsSync } from "node:fs";
import { resolve } from "node:path";
import {
  agentModes,
  agentReportError,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import {
  needsPublicationAuthority,
  oneShotOnlyChoices,
  sessionPolicy,
  sessionPolicyChoices,
} from "./session-policy.mjs";
import { stopped } from "./workspace-publication-ownership.mjs";
import { defaultCheckoutRequest } from "./workspace-publication-select.mjs";

// The owned-workspace request, or `{ error }` saying why not: separate from
// any supplied integration checkout, and existing already when nothing else
// supplies the repository.
function ownedWorkspaceRequest(requestInput) {
  const workspace = resolve(requestInput.workspace);
  const integration = requestInput.integration
    ? resolve(requestInput.integration)
    : undefined;
  const context = requestInput.repository
    ? resolve(requestInput.repository)
    : undefined;
  if (integration === workspace)
    return { error: "queued work requires a separate owned workspace" };
  if (!integration && !context && !existsSync(workspace))
    return {
      error:
        "without --integration or --repository, --workspace must name an existing owned worktree of the repository",
    };
  return {
    request: {
      ...requestInput,
      integration,
      workspace,
      repository: integration ?? context ?? workspace,
    },
  };
}

// The normalized request, or the stop that refuses it. A one-shot start
// publishes no claim, so it needs no publisher: its result waits for review in
// the owned workspace, or in the default checkout when `--default-main`
// selects it, and needs no publication authority. With `--auto-land` the
// verified result lands without that review, so the start requires the
// publication authority up front. An unlisted request has no
// identity; a supplied identity is checked against fetched trunk. The Git
// `repository` the start reads and selects from is the supplied integration
// (default) checkout, else the supplied repository context (an owned worktree
// or the common Git directory), else the owned workspace, which must then
// already exist. Only a supplied integration checkout gets a local refresh or
// supplies drafts.
export function startRequest(requestInput) {
  const policy = sessionPolicy(requestInput);
  const oneShot = policy.tracking === "one-shot";
  const defaultCheckout = policy.workspace === "default-checkout";
  const unsupported = oneShotOnlyChoices(policy)[0];
  if (unsupported)
    return stopped("invalid-request", {
      error: `${sessionPolicyChoices[unsupported].flag} applies to one-shot work; ${
        unsupported === "workspace"
          ? "a tracked start publishes its claim from a separate owned workspace"
          : "tracked work publishes through its own lifecycle"
      }`,
    });
  const required = [
    "workspace",
    ...(defaultCheckout ? [] : ["branch"]),
    ...(oneShot ? [] : ["identity", "publisherId"]),
    "mode",
    "target",
  ];
  for (const field of required)
    if (!requestInput[field])
      return stopped("invalid-request", { error: `missing ${field}` });
  const located = defaultCheckout
    ? defaultCheckoutRequest(requestInput)
    : ownedWorkspaceRequest(requestInput);
  if (located.error)
    return stopped("invalid-request", { error: located.error });
  const { request } = located;
  if (oneShot && request.admit === true)
    return stopped("invalid-request", {
      error:
        "one-shot work publishes no admission; choose --one-shot or --admit",
    });
  if (oneShot && (request.startingRevision || request.candidateSha))
    return stopped("invalid-request", {
      error:
        "a one-shot start publishes no claim to resume; resume its delivery instead",
    });
  if (request.startingRevision || request.candidateSha) {
    if (!request.startingRevision || !request.candidateSha)
      return stopped("invalid-request", {
        error: "resume requires starting revision and candidate SHA",
      });
    request.retained = {
      workspace: request.workspace,
      branch: request.branch,
      startingRevision: request.startingRevision,
      candidateSha: request.candidateSha,
    };
  }
  if (request.carry === true && request.admit !== true)
    return stopped("invalid-request", {
      error: "--carry carries a one-shot attempt's edits into --admit",
    });
  if (request.admit === true)
    for (const field of ["link", "title"])
      if (!request[field])
        return stopped("invalid-request", {
          error: `admission requires ${field}`,
        });
  const reportError = agentReportError(request);
  if (reportError) return stopped("invalid-request", { error: reportError });
  // Permission to work is separate from permission to publish: only a start
  // that publishes a claim, or a one-shot start whose result lands
  // automatically, needs trunk publication authority.
  const publishes = needsPublicationAuthority(policy);
  if (
    !agentModes.includes(request.mode) ||
    request.workspaceAuthorized !== true ||
    (publishes && request.pushAuthorized !== true)
  ) {
    return stopped("authority-required", {
      error: publishes
        ? "mode, workspace and trunk publication authority must be established"
        : "mode and workspace authority must be established",
    });
  }
  return { ok: true, request };
}
