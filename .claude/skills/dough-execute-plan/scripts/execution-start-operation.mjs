// Startup for queued, admitted (with carried edits when requested), or one-shot
// work. The CLI adapter stays in execution-start.mjs.
import { git, lsRemoteSha, revParse } from "./publication-git.mjs";
import {
  maintenance,
  reportedMaintenance,
} from "./execution-start-maintenance.mjs";
import { recovery, retainedCandidate } from "./execution-start-recovery.mjs";
import { finishCarry, parkCarriedEdits } from "./execution-start-carry.mjs";
import {
  existingClaim,
  prepareOneShot,
  startSource,
} from "./execution-start-source.mjs";
import { acceptedReceipt } from "./execution-start-receipt.mjs";
import { startRequest } from "./execution-start-request.mjs";
import { selectAgent } from "./agent-assignments.mjs";
import {
  claimReceiptAgent,
  reselectClaimAgent,
} from "./execution-start-agent.mjs";
import { commitWorkspaceClaim } from "./workspace-publication-claim.mjs";
import {
  mainWorktreeError,
  selectOwnedWorkspace,
} from "./workspace-publication-select.mjs";
import {
  claimMembership,
  publishClaimSha,
  publishStoryBranch,
} from "./workspace-publication-push.mjs";
import {
  backlogPath,
  claimProvenance,
  isAncestor,
  remoteOf,
  remoteRef,
  sourceStopped,
  stopped,
} from "./workspace-publication-ownership.mjs";

export async function startExecution(requestInput) {
  const started = startRequest(requestInput);
  if (!started.ok) return started;
  const result = await startRequested(started.request);
  return started.request.carry ? finishCarry(started.request, result) : result;
}
async function startRequested(request) {
  if (!request.retained && !request.defaultMain) {
    const error = await mainWorktreeError(request.workspace);
    if (error) return stopped("invalid-request", { error });
  }
  const remote = remoteOf(request);
  const ref = remoteRef(request);
  const source = startSource(request);
  let selectedSource, fetched, origin;
  try {
    origin = (
      await git(request.repository, "remote", "get-url", remote)
    ).stdout.trim();
    await git(request.repository, "fetch", remote);
    fetched = await revParse(request.repository, ref);
    selectedSource = await source.read(request, ref);
    if (request.retained) await source.retainedBasis?.(request, selectedSource);
  } catch (error) {
    return sourceStopped(error, { error: error.stderr || error.message });
  }
  if (source.oneShot) return prepareOneShot(request, origin, fetched);
  if (selectedSource.existing && !request.retained)
    return existingClaim(request, ref, selectedSource);
  // The rotation is read in the repository, which fetched trunk.
  const selection = { ...request, cwd: request.repository };
  let agent;
  if (!request.retained) {
    const chosen = await selectAgent(selection, ref, backlogPath, { fetched });
    if (!chosen.ok) return chosen;
    agent = chosen.agent;
  }
  const beforeMaintenance = await maintenance(request);
  // Stops report only this compact local outcome; acceptance reports both.
  const stopMaintenance = reportedMaintenance(beforeMaintenance);
  // A carried park, its reset and the workspace selection share one trunk.
  const parked = request.carry && (await parkCarriedEdits(request, fetched));
  if (parked && !parked.ok) return { ...parked, ...stopMaintenance };
  const base = parked ? fetched : undefined;
  const selected = await selectOwnedWorkspace({ ...request, origin, base });
  if (!selected.ok) return { ...selected, fetched, ...stopMaintenance };
  // Trunk can move between the source fetch and the workspace's base; the
  // claim names the rotation's next agent on the trunk it is built on.
  if (agent && selected.startingRevision !== fetched) {
    const { startingRevision: base, workspace, branch } = selected;
    const stop = { fetched, workspace, branch, ...stopMaintenance };
    const chosen = await selectAgent(selection, base, backlogPath, stop);
    if (!chosen.ok) return chosen;
    agent = chosen.agent;
  }
  const claimRequest = {
    ...request,
    ...selected,
    origin,
    plan: selectedSource.planTarget,
    admission: selectedSource.admission,
    backlogPath,
    candidateSha: request.retained?.candidateSha,
  };
  let checked;
  try {
    checked = await claimMembership(claimRequest);
    if (checked.ownership === "owned" && request.retained)
      await publishStoryBranch(claimRequest, checked.provenance.sha);
  } catch (error) {
    return stopped("unpublished", {
      workspace: selected.workspace,
      branch: selected.branch,
      ...stopMaintenance,
      error: error.stderr || error.message,
    });
  }
  if (checked.ownership === "owned" && request.retained) {
    const afterMaintenance = await maintenance(request);
    return acceptedReceipt(
      request,
      selected,
      selectedSource,
      {
        status: "resumed",
        publishedSha: checked.provenance.sha,
        candidateSha: request.retained.candidateSha,
        created: false,
        ...(await claimReceiptAgent(
          claimRequest,
          agent,
          checked.provenance.sha,
        )),
      },
      beforeMaintenance,
      afterMaintenance,
    );
  }
  if (checked.ownership !== "absent") {
    return stopped("conflict", {
      ownership: checked.ownership,
      workspace: selected.workspace,
      branch: selected.branch,
      ...stopMaintenance,
      provenance: checked.provenance,
      recovery: {
        ...recovery(request, selected, request.retained?.candidateSha),
        provenance: checked.provenance,
      },
    });
  }
  let committed;
  try {
    committed = request.retained
      ? await retainedCandidate(request, selected)
      : await commitWorkspaceClaim({ ...claimRequest, agent });
  } catch (error) {
    return stopped("claim-failed", {
      workspace: selected.workspace,
      branch: selected.branch,
      ...stopMaintenance,
      error: error.stderr || error.message,
    });
  }
  if (!committed.ok) return { ...committed, ...stopMaintenance };
  const published = await publishClaimSha({
    ...claimRequest,
    candidateSha: committed.candidateSha,
    async recheckSource({ candidateSha }) {
      await git(request.repository, "fetch", remote);
      const refreshed = await source.read(request, ref, candidateSha);
      if (source.changed(refreshed, selectedSource)) {
        throw new Error(
          "selected published source changed during claim publication",
        );
      }
      // The candidate's admission, reconciled again onto newer trunk.
      if (source.admitting) selectedSource = refreshed;
    },
    reselectClaim:
      (agent || source.admitting) &&
      reselectClaimAgent(
        claimRequest,
        agent,
        (next) => {
          agent = next;
        },
        source.admitting && (() => selectedSource.admission),
      ),
  });
  if (!published.ok)
    return {
      ...published,
      ...stopMaintenance,
      recovery: {
        ...recovery(
          request,
          selected,
          published.recovery?.candidateSha ?? committed.candidateSha,
        ),
        ...published.recovery,
      },
    };
  try {
    await git(selected.workspace, "fetch", remote);
    const remoteTip = await lsRemoteSha(origin, `refs/heads/${request.target}`);
    const contained = await isAncestor(
      selected.workspace,
      published.publishedSha,
      remoteTip,
    );
    const provenance = await claimProvenance(
      selected.workspace,
      ref,
      request.identity,
      backlogPath,
    );
    if (
      !contained ||
      provenance?.publisher !== request.publisherId ||
      provenance?.identity !== request.identity
    )
      throw new Error("remote containment or claim ownership is unconfirmed");
    await publishStoryBranch(claimRequest, published.publishedSha);
  } catch (error) {
    return stopped("unpublished", {
      workspace: selected.workspace,
      candidateSha: published.candidateSha,
      recovery: recovery(request, selected, published.candidateSha),
      ...stopMaintenance,
      error: error.stderr || error.message,
    });
  }
  const afterMaintenance = await maintenance(request);
  return acceptedReceipt(
    request,
    selected,
    selectedSource,
    {
      ...published,
      created: selected.created,
      ...(await claimReceiptAgent(claimRequest, agent, published.publishedSha)),
    },
    beforeMaintenance,
    afterMaintenance,
  );
}
