// Validates and normalizes a queued-start, admission or one-shot request
// before any Git work.
import { resolve } from "node:path";
import {
  agentModes,
  agentReportError,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { stopped } from "./workspace-publication-ownership.mjs";

// The normalized request, or the stop that refuses it. A one-shot start
// publishes no claim, so it needs no publisher, and an unlisted request has no
// identity; a supplied identity is checked against fetched trunk.
export function startRequest(requestInput) {
  const oneShot = requestInput.oneShot === true;
  const required = [
    "integration",
    "workspace",
    "branch",
    ...(oneShot ? [] : ["identity", "publisherId"]),
    "mode",
    "target",
  ];
  for (const field of required)
    if (!requestInput[field])
      return stopped("invalid-request", { error: `missing ${field}` });
  const request = {
    ...requestInput,
    integration: resolve(requestInput.integration),
    workspace: resolve(requestInput.workspace),
  };
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
  if (
    !agentModes.includes(request.mode) ||
    request.pushAuthorized !== true ||
    request.workspaceAuthorized !== true
  ) {
    return stopped("authority-required", {
      error:
        "mode, workspace and trunk publication authority must be established",
    });
  }
  if (request.integration === request.workspace)
    return stopped("invalid-request", {
      error: "queued work requires a separate owned workspace",
    });
  return { ok: true, request };
}
