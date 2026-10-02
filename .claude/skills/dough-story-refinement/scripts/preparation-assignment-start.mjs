// Publishes a queued story's preparation announcement before substantive
// preparation, or continues the workspace's existing one, first creating a
// missing workspace at fetched trunk. The announcement commit adds only the
// assignment profile; the draft stays in the workspace.
import { agentIdentity } from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { selectAgent } from "../../dough-execute-plan/scripts/agent-assignments.mjs";
import { maintenance } from "../../dough-execute-plan/scripts/execution-start-maintenance.mjs";
import { fastForwardToFetchedTrunk } from "../../dough-execute-plan/scripts/maintain-default-checkout.mjs";
import {
  git,
  revParse,
  tryPushExactRef,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import {
  configureAgentAuthorship,
  DeveloperIdentityRefused,
} from "../../dough-execute-plan/scripts/workspace-agent-authorship.mjs";
import {
  backlogPath,
  remoteRef,
} from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import {
  assignedElsewhereError,
  assignmentFields,
  errorText,
  recordedAllocation,
  restoreAllocation,
  stop,
  workspaceAssignment,
} from "./preparation-assignment-ownership.mjs";
import { requestOf } from "./preparation-assignment-request.mjs";
import {
  fetchQueuedTrunk,
  selectPreparationWorkspace,
} from "./preparation-assignment-trunk.mjs";

import {
  commitAnnouncement,
  acceptedOnRemote,
} from "./preparation-announcement.mjs";

// Drops an announcement a stopped earlier run left in the workspace that
// trunk never took, restoring the workspace it was built from.
async function dropUnconfirmed(workspace, found) {
  const { allocation } = found.own;
  if ((await revParse(workspace, "HEAD")) !== allocation) return;
  if ((await git(workspace, "status", "--porcelain")).stdout !== "") return;
  await git(workspace, "reset", "--keep", "--quiet", `${allocation}^`);
  await restoreAllocation(workspace, undefined);
}

export async function startPreparation(input) {
  const requested = requestOf("start", input);
  if (!requested.ok) return requested;
  const { request } = requested;
  const selected = await selectPreparationWorkspace(request);
  if (!selected.ok) return selected;
  const { selection } = selected;
  const result = await announce(request);
  return selection ? { ...result, selection } : result;
}

async function announce(request) {
  const { workspace, remote, target } = request;
  const ref = remoteRef(request);
  const trunk = await fetchQueuedTrunk(workspace, request);
  if (!trunk.ok) return trunk;
  let base = trunk.fetched;
  const recorded = recordedAllocation(workspace);
  const found = await workspaceAssignment(request, ref, recorded);
  if (request.continueOnly) {
    const facts = found.own && assignmentFields(request, found.own);
    if (
      found.state !== "held" ||
      (request.expectedAllocation !== undefined &&
        request.expectedAllocation !== facts?.allocation) ||
      (request.expectedAgent !== undefined &&
        request.expectedAgent !== facts?.agent)
    )
      return stop("continuation-refused", {
        workspace,
        error:
          "the retained preparation's exact local allocation and published ownership could not be verified; no announcement was made",
      });
  }
  if (found.state === "held") {
    await configureAgentAuthorship(workspace, agentIdentity(found.own.name));
    return {
      ok: true,
      status: "continued",
      ...assignmentFields(request, found.own),
      workspace,
      fetched: base,
    };
  }
  if (found.assigned)
    return stop("workspace-assigned-elsewhere", {
      workspace,
      fetched: base,
      ...assignmentFields(found.assigned.profile, found.assigned),
      error: assignedElsewhereError,
    });
  // Only dropping an unconfirmed announcement changes the record.
  let previous = await recorded;
  if (found.state === "unconfirmed") {
    await dropUnconfirmed(workspace, found);
    previous = await recordedAllocation(workspace);
  }
  const startHead = await revParse(workspace, "HEAD");
  // Refresh's eligibility on the workspace's own branch: only a fast-forward
  // to fetched trunk, or already being there, continues.
  const { reason } = await fastForwardToFetchedTrunk(workspace, base);
  if (reason)
    return stop("workspace-not-isolated", {
      workspace,
      fetched: base,
      error: `a new announcement needs a workspace that fast-forwards to fetched trunk (${reason}); nothing was published`,
    });
  // Stops with the workspace back where it started, however it moved since.
  const unannounced = async (status, fields) => {
    await git(workspace, "reset", "--keep", "--quiet", startHead);
    await restoreAllocation(workspace, previous);
    return stop(status, {
      workspace,
      fetched: base,
      unannounced: true,
      ...fields,
    });
  };
  let agent, announced;
  for (let attempt = 1; ; attempt += 1) {
    const chosen = await selectAgent(
      { ...request, cwd: workspace },
      base,
      backlogPath,
    );
    if (!chosen.ok) {
      const { status, error, occupied } = chosen;
      return unannounced(status, { error, occupied });
    }
    agent = chosen.agent;
    try {
      announced = await commitAnnouncement(request, agent);
    } catch (error) {
      if (!(error instanceof DeveloperIdentityRefused)) throw error;
      return unannounced("developer-identity-refused", {
        error: error.message,
      });
    }
    let rejected = false;
    try {
      ({ rejected } = await tryPushExactRef(
        workspace,
        announced.sha,
        remote,
        `refs/heads/${target}`,
      ));
    } catch {
      // The response is lost or refused; the remote itself decides below.
    }
    if (!rejected) break;
    await git(workspace, "fetch", "--quiet", remote);
    const moved = await revParse(workspace, ref);
    if (moved === base || attempt === 2) break;
    // Trunk moved: rebuild the isolated announcement on it, choosing again.
    await git(workspace, "reset", "--keep", "--quiet", moved);
    base = moved;
  }
  let accepted;
  try {
    accepted = await acceptedOnRemote(
      request,
      ref,
      announced.sha,
      announced.path,
    );
  } catch (error) {
    return stop("unpublished", {
      workspace,
      candidateSha: announced.sha,
      error: `announcement acceptance is unconfirmed: ${errorText(error)}`,
    });
  }
  // Remote trunk has not taken the announcement: leave the workspace as it
  // was found, with no coordination commit to mistake for a published one.
  if (!accepted)
    return unannounced("unpublished", {
      error: "remote trunk did not accept the preparation announcement",
    });
  const authorship = await configureAgentAuthorship(
    workspace,
    agentIdentity(agent.name),
  );
  const refresh = await maintenance(request);
  return {
    ok: true,
    status: "announced",
    ...assignmentFields(request, {
      name: agent.name,
      path: announced.path,
      allocation: announced.sha,
      profile: agent,
    }),
    publishedSha: announced.sha,
    workspace,
    workspaceAuthorship: authorship,
    refresh,
  };
}
