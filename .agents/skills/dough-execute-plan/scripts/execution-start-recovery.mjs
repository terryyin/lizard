import { git, revParse } from "./publication-git.mjs";
import { stopped, trailers } from "./workspace-publication-ownership.mjs";

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

export async function retainedCandidate(request, selected) {
  const sha = request.retained.candidateSha;
  if (!sha)
    return stopped("invalid-request", {
      error: "retained candidate SHA is required",
    });
  const message = (
    await git(selected.workspace, "log", "-1", "--format=%B", sha)
  ).stdout;
  const claim = trailers(message);
  const head = await revParse(selected.workspace, "HEAD");
  if (
    claim.identity !== request.identity ||
    claim.publisher !== request.publisherId ||
    sha !== head ||
    (await git(selected.workspace, "status", "--porcelain")).stdout !== ""
  ) {
    return stopped("setup-failed", {
      recovery: recovery(request, selected, sha),
      error: "retained candidate or workspace is not the isolated owned claim",
    });
  }
  return { ok: true, candidateSha: sha };
}
