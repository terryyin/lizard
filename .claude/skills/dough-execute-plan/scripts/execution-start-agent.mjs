// The agent a Take names, beyond the shared rotation reading in
// agent-assignments.mjs: chosen again when a rival publishes the same name
// first, read back from the claim commit when an existing claim resumes, and
// reported with the workspace's authorship on the receipt.
import { agentIdentity } from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import {
  addedProfile,
  agentUnavailable,
  nextAgentName,
} from "./agent-assignments.mjs";
import { git, revParse } from "./publication-git.mjs";
import { commitWorkspaceClaim } from "./workspace-publication-claim.mjs";
import { isAncestor, stopped } from "./workspace-publication-ownership.mjs";
import {
  configureAgentAuthorship,
  workspaceAuthorship,
} from "./workspace-agent-authorship.mjs";

// The execution profile the claim commit `claimSha` added for `identity`; a
// preparation profile naming the same identity is never the claim's agent.
async function claimProfile(workspace, claimSha, identity, backlogPath) {
  const added = await addedProfile(
    workspace,
    claimSha,
    backlogPath,
    (profile) =>
      profile.activity === "execution" && profile.identity === identity,
  );
  return added?.profile;
}

// publishClaimSha's reselection hook. When the rejected push's trunk already
// holds the selected name, rebuild the still-isolated Take on that trunk under
// the rotation's next name instead of replaying it. An admission, given its
// `admission()` reconciled onto that trunk, is always rebuilt so its content
// lands per section rather than by line replay; a resumed one keeps the agent
// its candidate named. Otherwise leave the replay alone. `onAgent` learns the
// name that was actually committed.
export function reselectClaimAgent(claimRequest, chosen, onAgent, admission) {
  const { workspace, backlogPath, identity } = claimRequest;
  return async ({ onto, candidateSha }) => {
    const agent =
      chosen ??
      (await claimProfile(workspace, candidateSha, identity, backlogPath));
    if (!agent) return undefined;
    const { name, held, error } = await nextAgentName(
      workspace,
      onto,
      backlogPath,
    );
    if (error) return stopped("agent-setting-invalid", { error });
    const collides = held.includes(agent.name);
    if (!collides && !admission) return undefined;
    const isolated =
      (await revParse(workspace, "HEAD")) === candidateSha &&
      (await isAncestor(workspace, `${candidateSha}^`, onto)) &&
      (await git(workspace, "status", "--porcelain")).stdout === "";
    if (!isolated) return undefined;
    if (collides && !name)
      return agentUnavailable(workspace, onto, backlogPath);
    await git(workspace, "reset", "--hard", onto);
    const next = collides ? { ...agent, name } : agent;
    const recreated = await commitWorkspaceClaim({
      ...claimRequest,
      startingRevision: onto,
      agent: next,
      ...(admission ? { admission: admission() } : {}),
    });
    if (recreated.ok) onAgent(next);
    return recreated;
  };
}

// A resumed claim keeps the agent its claim commit named. Restores that
// agent's authorship in the reused workspace and returns its name with that
// workspace's authorship, or undefined for a claim made without a profile. No
// new name is chosen and no profile is written.
async function resumeClaimAgent(workspace, claimSha, identity, backlogPath) {
  const profile = await claimProfile(
    workspace,
    claimSha,
    identity,
    backlogPath,
  );
  if (!profile) return undefined;
  const agent = agentIdentity(profile.name);
  return {
    agent: agent.agent,
    workspaceAuthorship: await configureAgentAuthorship(workspace, agent),
  };
}

// The receipt's agent for the claim at `claimSha`: the agent this Take chose,
// or else the one the claim commit names, with whether its workspace authors
// ordinary commits as that agent; nothing for a claim made without an agent.
export async function claimReceiptAgent(claimRequest, chosen, claimSha) {
  const { workspace, identity, backlogPath } = claimRequest;
  if (!chosen)
    return (
      (await resumeClaimAgent(workspace, claimSha, identity, backlogPath)) ?? {}
    );
  const agent = agentIdentity(chosen.name).agent;
  return { agent, workspaceAuthorship: await workspaceAuthorship(workspace) };
}
