// Git mechanics for explicit current-branch work and an already-supported
// host-owned execution. Both stay in the recorded checkout: no worktree is
// created and the branch is not switched. Codex, Cursor, and Claude already
// supply that checkout and, when the caller has publication authority, the
// authorized target. Publish authority uses publishExecutionIncrement from
// this checkout. A local commit or local merge stays a local operation.
// Installed guidance is the agent's contract.
import { realpathSync } from "node:fs";
import { publishExecutionIncrement } from "./execution-increment-publication.mjs";
import {
  declaredOwnerRefusal,
  refreshDefaultCheckout,
} from "./maintain-default-checkout.mjs";
import {
  git,
  recordedCheckoutIdentity,
  revParse,
} from "./publication-test-fixtures.mjs";

function sameCheckout(left, right) {
  if (!left || !right) {
    return false;
  }
  return realpathSync(left) === realpathSync(right);
}

function preserved(reason) {
  return {
    ok: false,
    classification: "local",
    publication: "pending",
    report: "preserved",
    receipt: null,
    maintenance: { result: "deferred", reason },
  };
}

function ownerAccess(request) {
  const refusal = declaredOwnerRefusal(
    request.declaredOwner,
    request.requester,
  );
  if (refusal) {
    return preserved(refusal);
  }
  return { ok: true };
}

async function assertStayed(checkout, before) {
  const after = await recordedCheckoutIdentity(checkout);
  if (
    after.branch !== before.branch ||
    after.toplevel !== before.toplevel ||
    after.worktrees !== before.worktrees
  ) {
    throw new Error(
      "current-branch delivery left the recorded checkout or branch",
    );
  }
}

async function commitOwned(checkout, paths, message) {
  await git(checkout, "add", "--", ...paths);
  await git(checkout, "commit", "--only", "-m", message, "--", ...paths);
  return revParse(checkout, "HEAD");
}

async function mergeLocal(checkout, mergeRef, message) {
  await git(checkout, "merge", "--no-ff", "-m", message, mergeRef);
  return revParse(checkout, "HEAD");
}

function pending(operation, sha, checkout, branch) {
  return {
    ok: true,
    classification: "local",
    operation,
    publication: "pending",
    report: "committed",
    sha,
    checkout,
    branch,
    receipt: null,
    maintenance: null,
  };
}

export async function deliverRecordedCheckout(request) {
  const { checkout, operation } = request;
  const before = await recordedCheckoutIdentity(checkout);
  const onDefault = sameCheckout(checkout, request.defaultCheckout);

  if (onDefault) {
    const access = ownerAccess(request);
    if (!access.ok) {
      await assertStayed(checkout, before);
      return { ...access, checkout: before.toplevel, branch: before.branch };
    }
  }

  const sha =
    operation === "merge"
      ? await mergeLocal(checkout, request.mergeRef, request.message)
      : await commitOwned(checkout, request.paths, request.message);

  const publishes = operation === "publish" && request.authority === "publish";
  if (!publishes) {
    await assertStayed(checkout, before);
    return pending(operation, sha, before.toplevel, before.branch);
  }

  const published = await publishExecutionIncrement({
    workspace: checkout,
    branch: before.branch,
    previouslyPublishedBase: request.previouslyPublishedBase,
    targetRef: request.targetRef,
    register: request.register,
    validate: request.validate,
    validatedCandidate: request.validatedCandidate,
    backlogPath: request.backlogPath,
  });
  if (!published.ok) {
    await assertStayed(checkout, before);
    return {
      ok: false,
      classification: "remote",
      operation: "publish",
      publication: published.publication,
      status: published.status,
      report: published.status,
      sha: published.candidate ?? null,
      checkout: before.toplevel,
      branch: before.branch,
      receipt: null,
      candidate: published.candidate,
      preRebaseSha: published.preRebaseSha,
      remoteTip: published.remoteTip,
      previouslyPublishedBase: published.previouslyPublishedBase,
      suffixBase: published.suffixBase,
      maintenance: null,
    };
  }
  const maintenance = onDefault
    ? await refreshDefaultCheckout({
        checkout,
        declaredOwner: request.declaredOwner,
        requester: request.requester,
        integrationBranch: request.integrationBranch ?? "main",
      })
    : null;
  await assertStayed(checkout, before);
  return {
    ok: true,
    classification: "remote",
    operation: "publish",
    publication: "accepted",
    report: "accepted",
    sha: published.receipt.sha,
    checkout: before.toplevel,
    branch: before.branch,
    receipt: published.receipt,
    preRebaseSha: published.preRebaseSha,
    maintenance,
  };
}
