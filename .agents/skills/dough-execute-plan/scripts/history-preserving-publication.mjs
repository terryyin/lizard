// Git mechanics for publish-the-candidate.md "Preserve published history".
// One owned workspace merges an already-published tip onto fetched trunk,
// pushes that candidate SHA through pushExactRef, and recomputes the merge
// once after a rejected push. Installed guidance is the agent's contract.
import { existsSync } from "node:fs";
import { isAbsolute, join } from "node:path";
import { fileURLToPath } from "node:url";
import { doneDirectoryBeside } from "../../dough-product-backlog/scripts/product-backlog-git-done-catalog.mjs";
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import {
  creditMergeInProgress,
  DeveloperIdentityRefused,
} from "./workspace-agent-authorship.mjs";
import {
  exec,
  git,
  lsRemoteSha,
  originTrackingRef,
  pushExactRef,
  revParse,
} from "./publication-git.mjs";

const mergeCli = fileURLToPath(
  new URL(
    "../../dough-product-backlog/scripts/product-backlog-git-merge.mjs",
    import.meta.url,
  ),
);

const trunkTarget = "refs/heads/main";

async function isAncestor(workspace, ancestor, descendant) {
  try {
    await git(workspace, "merge-base", "--is-ancestor", ancestor, descendant);
    return true;
  } catch (error) {
    if (error.code === 1) {
      return false;
    }
    throw error;
  }
}

async function pathExists(workspace, name) {
  const printed = (
    await git(workspace, "rev-parse", "--git-path", name)
  ).stdout.trim();
  const path = isAbsolute(printed) ? printed : join(workspace, printed);
  return existsSync(path);
}

// Whether either side changed the backlog or the done directory beside it.
async function backlogTouched(workspace, ref, file) {
  const base = (await git(workspace, "merge-base", "HEAD", ref)).stdout.trim();
  const paths = ["--", file, doneDirectoryBeside(file)];
  const onTrunk = (
    await git(workspace, "diff", "--name-only", base, "HEAD", ...paths)
  ).stdout.trim();
  const onStory = (
    await git(workspace, "diff", "--name-only", base, ref, ...paths)
  ).stdout.trim();
  return onTrunk !== "" || onStory !== "";
}

async function mergeThroughAdapter(workspace, ref, file) {
  try {
    const { stdout, stderr } = await exec(process.execPath, [
      mergeCli,
      "merge",
      "--ref",
      ref,
      "--file",
      file,
      "--cwd",
      workspace,
    ]);
    return { code: 0, status: stdout.trim(), stdout, stderr };
  } catch (error) {
    return {
      code: error.code ?? 1,
      status: `${error.stdout ?? ""}${error.stderr ?? ""}`.trim(),
      stdout: error.stdout ?? "",
      stderr: error.stderr ?? "",
    };
  }
}

async function historyPreservingMerge(workspace, ref) {
  const head = await revParse(workspace, "HEAD");
  const base = (await git(workspace, "merge-base", "HEAD", ref)).stdout.trim();
  if (base === head) {
    await git(workspace, "merge", "--ff-only", ref);
    return;
  }
  await git(workspace, "merge", "--no-ff", "--no-commit", ref);
  await creditMergeInProgress(workspace);
  await git(workspace, "commit", "--no-edit");
}

async function constructCandidate(workspace, trunkRef, publishedTip, file) {
  await git(workspace, "checkout", "--detach", trunkRef);
  if (await backlogTouched(workspace, publishedTip, file)) {
    const merged = await mergeThroughAdapter(workspace, publishedTip, file);
    if (merged.code !== 0) {
      return { ok: false, adapterStatus: merged.status, merged };
    }
    return {
      ok: true,
      sha: await revParse(workspace, "HEAD"),
      adapterStatus: merged.status,
    };
  }
  try {
    await historyPreservingMerge(workspace, publishedTip);
  } catch (error) {
    if (!(error instanceof DeveloperIdentityRefused)) throw error;
    return {
      ok: false,
      reason: "developer-identity-refused",
      error: error.message,
    };
  }
  return {
    ok: true,
    sha: await revParse(workspace, "HEAD"),
    adapterStatus: null,
  };
}

async function pushRejected(workspace, sha, remote, targetRef) {
  try {
    await pushExactRef(workspace, sha, remote, targetRef);
    return false;
  } catch (error) {
    const text = `${error.message}\n${error.stderr ?? ""}`;
    if (!/rejected|non-fast-forward|fetch first/i.test(text)) {
      throw error;
    }
    return true;
  }
}

async function returnToBranch(workspace, branch) {
  if (!branch || (await pathExists(workspace, "MERGE_HEAD"))) {
    return;
  }
  const status = (await git(workspace, "status", "--porcelain")).stdout;
  if (status !== "") {
    return;
  }
  await git(workspace, "checkout", branch);
}

export async function publishHistoryPreservingCandidate({
  ownedWorkspace,
  publishedTip,
  branch,
  targetRef = trunkTarget,
  remote = "origin",
  backlogPath = defaultBacklogPath,
  affectedCheck,
  beforePush,
  register,
}) {
  await git(ownedWorkspace, "fetch", remote);
  const tracking = originTrackingRef(targetRef, remote);
  if (await isAncestor(ownedWorkspace, publishedTip, tracking)) {
    return {
      classification: "already-accepted",
      mergeCount: 0,
      pushCount: 0,
      rejectedPushCount: 0,
      acceptedSha: await revParse(ownedWorkspace, tracking),
      receipt: null,
      supersededSha: null,
      adapterStatuses: [],
    };
  }

  const adapterStatuses = [];
  let supersededSha = null;
  let mergeCount = 0;
  let rejectedPushCount = 0;
  for (let attempt = 0; attempt < 2; attempt += 1) {
    const prepared = await constructCandidate(
      ownedWorkspace,
      tracking,
      publishedTip,
      backlogPath,
    );
    mergeCount += 1;
    if (prepared.adapterStatus) {
      adapterStatuses.push(prepared.adapterStatus);
    }
    if (!prepared.ok) {
      return {
        classification: "preserved",
        reason: prepared.reason ?? "conflict",
        ...(prepared.error && { error: prepared.error }),
        mergeCount,
        pushCount: 0,
        rejectedPushCount,
        supersededSha,
        adapterStatuses,
        receipt: null,
      };
    }
    if (affectedCheck) {
      await affectedCheck(prepared.sha);
    }
    if (attempt === 0 && beforePush) {
      await beforePush();
    }
    if (await pushRejected(ownedWorkspace, prepared.sha, remote, targetRef)) {
      rejectedPushCount += 1;
      supersededSha = prepared.sha;
      await git(ownedWorkspace, "fetch", remote);
      continue;
    }
    await git(ownedWorkspace, "fetch", remote);
    const acceptedTip = await lsRemoteSha(remote, targetRef, ownedWorkspace);
    if (acceptedTip !== prepared.sha) {
      throw new Error("remote did not accept the candidate");
    }
    const receipt = { sha: prepared.sha, target: targetRef };
    register?.(receipt);
    await returnToBranch(ownedWorkspace, branch);
    return {
      classification: "published",
      mergeCount,
      pushCount: 1,
      rejectedPushCount,
      receipt,
      acceptedSha: prepared.sha,
      supersededSha,
      adapterStatuses,
    };
  }

  return {
    classification: "preserved",
    reason: "persistent-contention",
    mergeCount,
    pushCount: 0,
    rejectedPushCount,
    supersededSha,
    adapterStatuses,
    receipt: null,
  };
}
