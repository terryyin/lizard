// Publish one claim SHA from the owned workspace. A rejected push rechecks
// remote membership before any replay. Another publisher, or missing
// provenance, stays a conflict. Implementation is not started here.
import { execFile } from "node:child_process";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";
import {
  git,
  lsRemoteSha,
  pushExactRef,
  revParse,
} from "./publication-git.mjs";
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
  const ref = remoteRef(request);
  const provenance = await claimProvenance(
    request.workspace,
    ref,
    request.identity,
    pathOf(request),
  );
  const candidateSha = request.candidateSha;
  const candidateIsAncestor = candidateSha
    ? await isAncestor(request.workspace, candidateSha, ref)
    : false;
  return {
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
  const recoveryAt = (details) => ({
    workspace: request.workspace,
    branch: request.branch,
    candidateSha: sha,
    ...details,
  });
  let pushError;
  try {
    await pushExactRef(
      request.workspace,
      sha,
      remoteOf(request),
      `refs/heads/${targetOf(request)}`,
    );
  } catch (error) {
    pushError = error;
  }
  if (pushError) {
    let checked;
    try {
      checked = await claimMembership({ ...request, candidateSha: sha });
    } catch (error) {
      return stopped("unpublished", {
        recovery: recoveryAt({
          error: `push and verification failed: ${pushError.message}; ${error.message}`,
        }),
      });
    }
    if (checked.ownership === "owned") {
      return {
        ok: true,
        status: "resumed",
        ownership: checked.ownership,
        publishedSha: checked.provenance.sha,
        candidateSha: sha,
        workspace: request.workspace,
        branch: request.branch,
      };
    }
    if (checked.ownership !== "absent") {
      return conflictResult(
        request,
        checked.ownership,
        checked.provenance,
        sha,
      );
    }
    const text = `${pushError.message}\n${pushError.stderr ?? ""}`;
    if (!/rejected|non-fast-forward/.test(text)) {
      return stopped("unpublished", {
        recovery: recoveryAt({ error: text }),
      });
    }
    if (request.recheckSource) {
      try {
        await request.recheckSource({ candidateSha: sha });
      } catch (error) {
        return sourceStopped(error, {
          recovery: recoveryAt({ error: error.stderr || error.message }),
        });
      }
    }
    const onto = await revParse(request.workspace, remoteRef(request));
    // Startup may rebuild its Take on newer trunk instead of replaying it,
    // for example when a rival now holds the agent name it selected.
    let reselected;
    if (request.reselectClaim) {
      try {
        reselected = await request.reselectClaim({ onto, candidateSha: sha });
      } catch (error) {
        reselected = stopped("unpublished", {
          recovery: { error: error.stderr || error.message },
        });
      }
      if (reselected && !reselected.ok) {
        return { ...reselected, recovery: recoveryAt(reselected.recovery) };
      }
    }
    const replay = reselected ? { code: 0 } : await replaySuffix(request, onto);
    if (replay.code !== 0) {
      return stopped("conflict", {
        ownership: "replay-failed",
        recovery: recoveryAt({ replay }),
      });
    }
    sha = await revParse(request.workspace, "HEAD");
    try {
      await pushExactRef(
        request.workspace,
        sha,
        remoteOf(request),
        `refs/heads/${targetOf(request)}`,
      );
    } catch (error) {
      // A second race is outside the one-retry bound. The rewritten candidate
      // stays in this workspace for a later explicit resume.
      return stopped("unpublished", {
        recovery: recoveryAt({
          error: `${error.message}\n${error.stderr ?? ""}`,
        }),
      });
    }
  }
  let present;
  try {
    await git(request.workspace, "fetch", remoteOf(request));
    const tip = await lsRemoteSha(
      request.origin,
      `refs/heads/${targetOf(request)}`,
    );
    present = sha === tip || (await isAncestor(request.workspace, sha, tip));
  } catch (error) {
    return stopped("unpublished", {
      recovery: recoveryAt({
        error: `push result could not be verified: ${error.message}`,
      }),
    });
  }
  if (!present) {
    return stopped("unpublished", { recovery: recoveryAt() });
  }
  return {
    ok: true,
    status: "published",
    publishedSha: sha,
    workspace: request.workspace,
    branch: request.branch,
    candidateSha: sha,
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
