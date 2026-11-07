// The isolated preparation announcement commit and confirmation from origin.
import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import {
  agentIdentity,
  renderAgentProfile,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { profileAllocation } from "../../dough-execute-plan/scripts/agent-assignments.mjs";
import {
  git,
  lsRemoteSha,
  revParse,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import { creditDeveloper } from "../../dough-execute-plan/scripts/workspace-agent-authorship.mjs";
import { isAncestor } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import {
  profilePathOf,
  recordAllocation,
} from "./preparation-assignment-ownership.mjs";

// Commits only the new profile on top of fetched trunk, from a clean
// workspace already at that trunk; nothing else is staged.
// The agent authors it and the developer committing it is credited; an
// unusable developer throws DeveloperIdentityRefused before anything changes.
export async function commitAnnouncement(request, agent) {
  const { workspace } = request;
  const identity = agentIdentity(agent.name);
  const message = await creditDeveloper(
    workspace,
    `Announce preparation: ${request.identity}\n\nPreparation-Identity: ${request.identity}\n`,
    identity,
  );
  const path = profilePathOf(agent.name);
  mkdirSync(dirname(join(workspace, path)), { recursive: true });
  writeFileSync(
    join(workspace, path),
    renderAgentProfile({
      name: agent.name,
      identity: request.identity,
      activity: "preparation",
      host: agent.host,
      model: agent.model,
    }),
  );
  await git(workspace, "add", "--", path);
  await git(
    workspace,
    "commit",
    "--quiet",
    `--author=${identity.agent} <${identity.email}>`,
    "-m",
    message,
  );
  const sha = await revParse(workspace, "HEAD");
  // Recorded before the push, so a rerun after a lost response recognizes
  // its own announcement instead of making a second one.
  await recordAllocation(workspace, sha);
  return { path, sha };
}

// Whether remote trunk contains `sha` and still records it as the profile's
// allocation, read from a fresh fetch and the remote's own tip.
export async function acceptedOnRemote(request, ref, sha, path) {
  const { workspace, remote, target } = request;
  await git(workspace, "fetch", "--quiet", remote);
  const url = (await git(workspace, "remote", "get-url", remote)).stdout.trim();
  const tip = await lsRemoteSha(url, `refs/heads/${target}`);
  if (!tip || !(await isAncestor(workspace, sha, tip))) return false;
  return (await profileAllocation(workspace, ref, path)) === sha;
}
