// Apply startup claim ownership and replay rules to exact publication.
import { execFile } from "node:child_process";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";
import {
  git,
  lsRemoteSha,
  pushExactRef,
  revParse,
} from "./publication-git.mjs";
import { publishExactCandidate } from "./publication-loop.mjs";
import {
  claimProvenance,
  classifyOwnership,
  isAncestor,
  pathOf,
  remoteOf,
  remoteRef,
  sourceStopped,
  stopped,
  targetOf,
} from "./workspace-publication-ownership.mjs";

const exec = promisify(execFile);
const rebaseCli = fileURLToPath(
  new URL(
    "../../dough-product-backlog/scripts/product-backlog-git-rebase.mjs",
    import.meta.url,
  ),
);

async function replaySuffix(request, onto) {
  try {
    const { stdout, stderr } = await exec(process.execPath, [
      rebaseCli,
      "rebase",
      "--onto",
      onto,
      "--ref",
      request.startingRevision,
      "--branch",
      request.branch,
      "--cwd",
      request.workspace,
      "--file",
      pathOf(request),
    ]);
    return { code: 0, stdout, stderr };
  } catch (error) {
    return {
      code: error.code ?? 1,
      stdout: error.stdout ?? "",
      stderr: error.stderr ?? "",
    };
  }
}

export async function claimMembership(request) {
  await git(request.workspace, "fetch", remoteOf(request));
  return claimMembershipAt(
    request,
    await revParse(request.workspace, remoteRef(request)),
  );
}

async function claimMembershipAt(request, tip) {
  const provenance = await claimProvenance(
    request.workspace,
    tip,
    request.identity,
    pathOf(request),
  );
  const candidateSha = request.candidateSha;
  const candidateIsAncestor = candidateSha
    ? await isAncestor(request.workspace, candidateSha, tip)
    : false;
  return {
    tip,
    provenance,
    candidateIsAncestor,
    ownership: classifyOwnership({
      publisherId: request.publisherId,
      candidateSha,
      candidateIsAncestor,
      provenance,
    }),
  };
}

export function conflictResult(request, ownership, provenance, candidateSha) {
  return stopped("conflict", {
    ownership,
    workspace: request.workspace,
    branch: request.branch,
    candidateSha,
    recovery: {
      workspace: request.workspace,
      branch: request.branch,
      startingRevision: request.startingRevision,
      candidateSha,
      provenance,
    },
  });
}

export async function publishClaimSha(request) {
  let sha = request.candidateSha;
  // Every stop leaves the current candidate in this workspace for resume.
  const currentClaim = () => ({
    workspace: request.workspace,
    branch: request.branch,
    candidateSha: sha,
  });
  const unpublished = (error) =>
    stopped("unpublished", {
      recovery: {
        ...currentClaim(),
        ...(error === undefined ? {} : { error }),
      },
    });
  const gitErrorText = (error) => `${error.message}\n${error.stderr ?? ""}`;
  let resumed;
  const result = await publishExactCandidate({
    workspace: request.workspace,
    remote: remoteOf(request),
    targetRef: `refs/heads/${targetOf(request)}`,
    initialCandidate: sha,
    confirmationRemote: request.origin,
    acceptsLiveCandidate: (candidate, tip) =>
      candidate === tip || isAncestor(request.workspace, candidate, tip),
    async recoverPushFailure({
      candidate,
      pushError,
      remoteTip,
      verificationError,
    }) {
      let checked;
      try {
        if (verificationError) throw verificationError;
        checked = await claimMembershipAt(
          { ...request, candidateSha: candidate },
          remoteTip ?? (await revParse(request.workspace, remoteRef(request))),
        );
      } catch (error) {
        return {
          stop: unpublished(
            `push and verification failed: ${pushError.message}; ${error.message}`,
          ),
        };
      }
      if (checked.ownership === "owned") {
        resumed = checked.provenance.sha;
        return { alreadyPublished: true };
      }
      if (checked.ownership !== "absent") {
        return {
          stop: conflictResult(
            request,
            checked.ownership,
            checked.provenance,
            candidate,
          ),
        };
      }
      const text = gitErrorText(pushError);
      if (!/rejected|non-fast-forward/.test(text))
        return { stop: unpublished(text) };
      try {
        if (request.recheckSource)
          await request.recheckSource({ candidateSha: candidate });
      } catch (error) {
        return {
          stop: sourceStopped(error, {
            recovery: {
              ...currentClaim(),
              error: error.stderr || error.message,
            },
          }),
        };
      }
      // Source recheck can fetch again before rebuilding or replaying the Take.
      const onto = await revParse(request.workspace, remoteRef(request));
      let reselected;
      try {
        if (request.reselectClaim)
          reselected = await request.reselectClaim({
            onto,
            candidateSha: candidate,
          });
      } catch (error) {
        return { stop: unpublished(error.stderr || error.message) };
      }
      if (reselected && !reselected.ok) {
        return {
          stop: {
            ...reselected,
            recovery: { ...currentClaim(), ...reselected.recovery },
          },
        };
      }
      if (!reselected) {
        const replay = await replaySuffix(request, onto);
        if (replay.code !== 0) {
          return {
            stop: stopped("conflict", {
              ownership: "replay-failed",
              recovery: { ...currentClaim(), replay },
            }),
          };
        }
      }
      sha = await revParse(request.workspace, "HEAD");
      return { candidate: sha };
    },
  });
  if (result.stop) return result.stop;
  if (result.pushError) return unpublished(gitErrorText(result.pushError));
  if (result.confirmationError)
    return unpublished(
      `push result could not be verified: ${result.confirmationError.message}`,
    );
  if (!resumed && !result.acceptedTip) return unpublished();
  return {
    ok: true,
    status: resumed ? "resumed" : "published",
    ...(resumed ? { ownership: "owned" } : {}),
    publishedSha: resumed ?? sha,
    ...currentClaim(),
  };
}

// The published profile names a Story Branch Mode execution's branch, so the
// accepted claim also publishes that branch; one already published stays as is.
export async function publishStoryBranch(request, sha) {
  if (request.mode !== "story-branch") return;
  const ref = `refs/heads/${request.branch}`;
  if (await lsRemoteSha(request.origin, ref)) return;
  await pushExactRef(request.workspace, sha, remoteOf(request), ref);
}
