#!/usr/bin/env node
// Git mechanics for references/preserve-published-history.md.
// One owned workspace merges an already-published tip onto fetched trunk,
// pushes that candidate SHA through pushExactRef, and recomputes the merge
// once after a rejected push. A conflicted or uncommitted merge is preserved
// for the agent to resolve and commit; the next run publishes that commit as
// the candidate.
// With a launch's landing context, each attempt's candidate and the fetched
// tip it was built on are retained before its push, and the accepted pair is
// recorded as that launch's landing. Installed guidance is the agent's
// contract.
import { resolve } from "node:path";
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import { flagValues, isDirectCliEntry } from "./ci-direct-entry.mjs";
import {
  captureAcceptedLanding,
  retainLandingComparison,
} from "./dashboard-landing.mjs";
import {
  constructCandidate,
  mergeInProgress,
} from "./history-preserving-candidate.mjs";
import {
  git,
  lsRemoteSha,
  originTrackingRef,
  pushExactRef,
  revParse,
} from "./publication-git.mjs";

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
  if (!branch || (await mergeInProgress(workspace))) {
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
  landingContext,
  register,
}) {
  await git(ownedWorkspace, "fetch", remote);
  const tracking = originTrackingRef(targetRef, remote);
  // Records the pair retained before the accepted push; `expected` names what
  // this run knows of it.
  const captured = async (expected) =>
    landingContext
      ? {
          landing: await captureAcceptedLanding(landingContext, {
            ...expected,
            remote,
            target: targetRef,
          }),
        }
      : {};
  if (await isAncestor(ownedWorkspace, publishedTip, tracking)) {
    return {
      classification: "already-accepted",
      // A rerun after a push whose answer was lost still records its pair.
      ...(await captured({})),
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
    const suffixBase = await revParse(ownedWorkspace, tracking);
    const prepared = await constructCandidate(
      ownedWorkspace,
      tracking,
      publishedTip,
      backlogPath,
    );
    if (prepared.mergeRan) mergeCount += 1;
    if (prepared.adapterStatus) {
      adapterStatuses.push(prepared.adapterStatus);
    }
    if (!prepared.ok) {
      return {
        classification: "preserved",
        reason: prepared.reason ?? "conflict",
        ...(prepared.error && { error: prepared.error }),
        ...(prepared.conflictedPaths && {
          conflictedPaths: prepared.conflictedPaths,
        }),
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
    const comparison = { candidate: prepared.sha, suffixBase };
    if (landingContext) {
      try {
        await retainLandingComparison(landingContext, comparison, {
          remote,
          targetRef,
        });
      } catch (error) {
        // A resolved merge taken from the workspace is on no ref: the
        // workspace stays on it so the next run takes it again.
        if (prepared.mergeRan) await returnToBranch(ownedWorkspace, branch);
        throw error;
      }
    }
    await beforePush?.({ attempt, ...comparison });
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
    const landing = await captured({
      base: suffixBase,
      revision: prepared.sha,
    });
    register?.(receipt);
    await returnToBranch(ownedWorkspace, branch);
    return {
      classification: "published",
      ...landing,
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

const usage =
  "usage: history-preserving-publication.mjs integrate --workspace PATH --published-tip SHA --branch NAME [--target-ref refs/heads/<branch>] [--remote NAME] [--backlog-path PATH] [--landing-context PATH]";

function argumentsOf(argv) {
  if (argv[0] !== "integrate") throw new Error(usage);
  const result = flagValues(argv.slice(1));
  const { workspace, publishedTip, branch, ...rest } = result;
  const known = ["targetRef", "remote", "backlogPath", "landingContext"];
  if (
    !workspace ||
    !publishedTip ||
    !branch ||
    Object.keys(rest).some((key) => !known.includes(key))
  ) {
    throw new Error(usage);
  }
  return { ownedWorkspace: resolve(workspace), publishedTip, branch, ...rest };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const result = await publishHistoryPreservingCandidate(
      argumentsOf(process.argv.slice(2)),
    );
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (result.classification === "preserved") process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
