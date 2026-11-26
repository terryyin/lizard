import { realpath } from "node:fs/promises";
import { posix } from "node:path";
import { agentProfileDirectory } from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { ongoingOperation } from "./maintain-default-checkout.mjs";
import {
  git,
  inspectCheckout,
  lsRemoteSha,
  managementContext,
  revParse,
  verifyRef,
} from "./publication-git.mjs";
import { mainWorktreeError } from "./workspace-publication-select.mjs";
import {
  backlogPath,
  isAncestor,
  remoteOf,
  stopped,
  trailers,
} from "./workspace-publication-ownership.mjs";

export function recovery(request, selected, candidateSha) {
  return {
    workspace: selected.workspace,
    branch: selected.branch,
    startingRevision: selected.startingRevision,
    candidateSha,
    identity: request.identity,
    publisherId: request.publisherId,
  };
}

export function sameSelectedSource(a, b) {
  return (
    a.selectedSource === b.selectedSource &&
    a.planSource === b.planSource &&
    a.planTarget === b.planTarget
  );
}

export async function retainedCandidate(
  request,
  selected,
  expectedHead = request.retained.candidateSha,
) {
  const sha = request.retained.candidateSha;
  if (!sha)
    return stopped("invalid-request", {
      error: "retained candidate SHA is required",
    });
  const message = (
    await git(selected.workspace, "log", "-1", "--format=%B", sha)
  ).stdout;
  const claim = trailers(message);
  const { head, status } = await inspectCheckout(
    selected.workspace,
    undefined,
    ["--untracked-files=all"],
  );
  if (
    claim.identity !== request.identity ||
    claim.publisher !== request.publisherId ||
    head !== expectedHead ||
    status !== ""
  ) {
    return stopped("setup-failed", {
      recovery: recovery(request, selected, sha),
      error: "retained candidate or workspace is not the isolated owned claim",
    });
  }
  return { ok: true, candidateSha: sha };
}

// A caller may draft again only after this exact losing local Take has a
// recoverable ref and its original checkout can continue from trunk.
export async function stoppedClaim(request, checked) {
  const { ownership, provenance } = checked;
  const recoveryFacts = {
    ...recovery(request, request, request.retained?.candidateSha),
    provenance,
  };
  const conflict = stopped("conflict", {
    ownership,
    workspace: request.workspace,
    branch: request.branch,
    provenance,
    recovery: recoveryFacts,
  });
  if (
    !request.retryAdmission ||
    ownership !== "other" ||
    checked.candidateIsAncestor ||
    provenance?.identity !== request.identity ||
    !provenance.sha
  )
    return conflict;
  try {
    const preserved = await retireLosingCandidate(request, checked);
    return preserved
      ? stopped("retry-admission", {
          recovery: { ...recoveryFacts, preserved },
        })
      : conflict;
  } catch (error) {
    return {
      ...conflict,
      recovery: { ...recoveryFacts, error: error.stderr || error.message },
    };
  }
}

async function retireLosingCandidate(request, { tip, provenance }) {
  const { workspace, branch, startingRevision, candidateSha } = request;
  if (
    branch === request.target ||
    (await mainWorktreeError(workspace)) ||
    (await realpath(await revParse(workspace, "--show-toplevel"))) !==
      (await realpath(workspace)) ||
    (await realpath(await managementContext(workspace))) !==
      (await realpath(await managementContext(request.repository))) ||
    (
      await git(workspace, "remote", "get-url", remoteOf(request))
    ).stdout.trim() !== request.origin
  )
    return undefined;

  const [commit, parent, ...extraParents] = (
    await git(workspace, "rev-list", "--parents", "-1", candidateSha)
  ).stdout
    .trim()
    .split(" ");
  if (
    commit !== candidateSha ||
    !parent ||
    extraParents.length > 0 ||
    !(await isAncestor(workspace, startingRevision, parent)) ||
    !(await isAncestor(workspace, parent, provenance.sha)) ||
    !(await changesOnlyAdmission(request, parent, candidateSha)) ||
    tip !==
      (await lsRemoteSha(request.origin, `refs/heads/${request.target}`)) ||
    (await git(workspace, "branch", "--show-current")).stdout.trim() !==
      branch ||
    (await ongoingOperation(workspace))
  )
    return undefined;

  const preserved = `refs/worktree/dough/rejected-admission/${candidateSha}`;
  const saved = await verifyRef(workspace, preserved);
  const prior = saved.verified ? saved.stdout.trim() : "";
  if (prior && prior !== candidateSha) return undefined;
  if (
    prior === candidateSha &&
    (await retainedCandidate(request, request, parent)).ok
  )
    return preserved;

  if (!(await retainedCandidate(request, request)).ok) return undefined;
  await git(
    workspace,
    "update-ref",
    preserved,
    candidateSha,
    prior || "0".repeat(candidateSha.length),
  );
  await git(workspace, "reset", "--keep", parent);
  return preserved;
}

async function changesOnlyAdmission({ workspace, link }, base, candidate) {
  const directory = posix.dirname(backlogPath);
  const seed = posix.join(directory, link.split("#")[0]);
  const { stdout } = await git(
    workspace,
    "diff",
    "--no-renames",
    "--name-only",
    "-z",
    base,
    candidate,
    "--",
  );
  const paths = new Set(stdout.split("\0").filter(Boolean));
  if (!paths.delete(backlogPath) || !paths.delete(seed)) return false;
  const [profile] = paths;
  return (
    paths.size === 0 ||
    (paths.size === 1 &&
      posix.dirname(profile) === posix.join(directory, agentProfileDirectory))
  );
}
