// Commit the Taken claim in the owned execution workspace: a queued entry's
// Take, or an admission that also carries the accepted story's reconciled
// canonical content. An agent's claim adds its profile, is authored by the
// agent, and credits the committing developer. Publication of that SHA is a
// separate step.
import { dirname, join } from "node:path";
import { mkdirSync, writeFileSync } from "node:fs";
import {
  agentIdentity,
  renderAgentProfile,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { applyToBacklog } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import {
  admitEntry,
  takeEntry,
} from "../../dough-product-backlog/scripts/product-backlog-take.mjs";
import { git, revParse } from "./publication-git.mjs";
import {
  claimCommitMessage,
  pathOf,
  remoteRef,
  stopped,
  trailers,
} from "./workspace-publication-ownership.mjs";
import {
  configureAgentAuthorship,
  creditDeveloper,
  DeveloperIdentityRefused,
} from "./workspace-agent-authorship.mjs";

function agentProfileOf(request, file) {
  const identity = agentIdentity(request.agent.name);
  return { identity, profile: join(dirname(file), identity.path) };
}

// Adds the agent's profile beside the backlog. Trunk Mode names remote trunk
// as its branch context; Story Branch Mode names the owned branch.
async function stageAgentProfile(request, { identity, profile }) {
  const { workspace, agent } = request;
  mkdirSync(dirname(join(workspace, profile)), { recursive: true });
  writeFileSync(
    join(workspace, profile),
    renderAgentProfile({
      name: agent.name,
      identity: request.identity,
      mode: request.mode,
      branch: request.mode === "trunk" ? remoteRef(request) : request.branch,
      host: agent.host,
      model: agent.model,
    }),
  );
  await configureAgentAuthorship(workspace, identity);
  await git(workspace, "add", "--", profile);
}

export async function commitWorkspaceClaim(request) {
  const { workspace, identity, publisherId, startingRevision } = request;
  const file = pathOf(request);
  const message = (await git(workspace, "log", "-1", "--format=%B")).stdout;
  const owned = trailers(message);
  const head = await revParse(workspace, "HEAD");
  if (
    head !== startingRevision &&
    owned.publisher === publisherId &&
    owned.identity === identity
  ) {
    return { ok: true, candidateSha: head, committed: false };
  }
  if (
    head !== startingRevision ||
    (await git(workspace, "status", "--porcelain")).stdout !== ""
  ) {
    return stopped("setup-failed", {
      recovery: {
        workspace,
        branch: request.branch,
        error: "claim workspace has unpublished commits or pending changes",
      },
    });
  }
  // The agent was chosen on this starting revision, so its profile is free.
  const agentProfile = request.agent && agentProfileOf(request, file);
  const { admission } = request;
  let claimMessage = claimCommitMessage(
    identity,
    publisherId,
    Boolean(admission),
  );
  // An agent's Take credits the developer committing it, or changes nothing.
  if (agentProfile) {
    try {
      claimMessage = await creditDeveloper(
        workspace,
        claimMessage,
        agentProfile.identity,
      );
    } catch (error) {
      if (!(error instanceof DeveloperIdentityRefused)) throw error;
      return stopped("developer-identity-refused", {
        workspace,
        branch: request.branch,
        error: error.message,
      });
    }
  }
  // Admitted content lands before the entry, whose home and plan must resolve.
  for (const { path, content } of admission?.files ?? []) {
    mkdirSync(dirname(join(workspace, path)), { recursive: true });
    writeFileSync(join(workspace, path), content);
    await git(workspace, "add", "--", path);
  }
  let outcome;
  await applyToBacklog(join(workspace, file), (source) => {
    const entryRequest = {
      identity,
      ...(request.plan === undefined ? {} : { plan: request.plan }),
      backlogDirectory: dirname(join(workspace, file)),
    };
    // A queued story admitted with its one-shot edits moves its entry.
    outcome =
      admission && !admission.queued
        ? admitEntry(source, {
            ...entryRequest,
            title: admission.title,
            href: admission.href,
          })
        : takeEntry(source, entryRequest);
    return outcome.source;
  });
  if (outcome.result === "unchanged") {
    return stopped("unchanged", { workspace, branch: request.branch });
  }
  if (agentProfile) await stageAgentProfile(request, agentProfile);
  await git(workspace, "add", "--", file);
  // The agent authors the Take commit even where workspace authorship could
  // not be configured.
  const { agent, email } = agentProfile?.identity ?? {};
  await git(
    workspace,
    "commit",
    ...(agentProfile ? [`--author=${agent} <${email}>`] : []),
    "-m",
    claimMessage,
  );
  return {
    ok: true,
    candidateSha: await revParse(workspace, "HEAD"),
    committed: true,
  };
}
