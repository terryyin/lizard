#!/usr/bin/env node
// Story wrap-up's Trunk Mode closure after the before-cleanup commit was
// accepted: confirm the target holds that commit, publish the committed final
// closure through managed delivery on this execution's own observer, run
// completion once for the accepted SHA there, attempt the optional
// default-checkout refresh, and only after a `success` receipt with confirmed
// shutdown retire the worktree and branch through Dough Land's retirement
// core. Any stop preserves the worktree, branch, and both closure commits and
// names the unfinished step.
// For a selected completed supplier (`--supplier-identity`), an accepted final
// closure first visits current consumers from the still-present worktree. A
// clean visit, with no consumers and no discovery problems, continues in the
// same invocation and reports the visit. Any other visit stops at
// `step: "visit"`, before completion, refresh or retirement. Only a rerun
// naming the last accepted SHA of that settled visit (`--visit-settled`),
// with no later publication registered on this closure's observers,
// completes that SHA and retires.
// A rerun continues from the first unfinished step: an accepted final closure
// is not pushed again, and once the worktree is gone the recorded management
// context (`--repository`) carries completion and cleanup.
import { existsSync } from "node:fs";
import { resolve } from "node:path";
import { completeRevision } from "../../dough-execute-plan/scripts/ci-mailbox-complete.mjs";
import { mailboxRoot } from "../../dough-execute-plan/scripts/ci-mailbox-location.mjs";
import { isDirectCliEntry } from "../../dough-execute-plan/scripts/ci-direct-entry.mjs";
import { refreshDefaultCheckout } from "../../dough-execute-plan/scripts/maintain-default-checkout.mjs";
import {
  git,
  originTrackingRef,
  resolveManagementContext,
  targetBranchName,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import { retireWorktree } from "../../dough-land/scripts/worktree-retirement.mjs";
import { isAncestor } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import { observerAccess } from "./trunk-closure-observer.mjs";
import { settleFinalClosure } from "./trunk-closure-settlement.mjs";
import { settledVisit } from "./trunk-closure-visit.mjs";
import { finishArguments } from "./trunk-closure-arguments.mjs";

import {
  completionRecovery,
  receiptPermitsRetirement,
  resourcesStay,
} from "./trunk-closure-completion.mjs";
const recoveries = {
  "before-cleanup":
    "publish the before-cleanup commit through deliver until it is accepted, then rerun finish",
  context:
    "rerun finish with --repository set to the management context an earlier finish result reported",
  observation: `follow observation.reason and rerun finish, or report the lost coverage; ${resourcesStay}`,
};

function stop(step, detail) {
  return {
    ok: false,
    step,
    cleanup: "not-performed",
    recovery: recoveries[step] ?? null,
    ...detail,
  };
}

export async function finishTrunkClosure({
  workspace,
  repository: recorded,
  branch,
  beforeCleanup,
  final,
  previouslyPublishedBase,
  targetRef,
  repo,
  host,
  remote = "origin",
  identity,
  createdForWork = false,
  session,
  coordinator,
  observerDirectory,
  preferredAlias,
  defaultCheckout,
  supplierIdentity,
  visitSettled,
  backlogPath,
  storage = mailboxRoot,
  env = process.env,
}) {
  // Recorded while the worktree exists, so a rerun can retire from it.
  const repository = await resolveManagementContext(recorded, workspace);
  const where = { repository, beforeCleanup, final };
  if (!repository) {
    return stop("context", {
      ...where,
      reason: "execution worktree and management context are both absent",
    });
  }
  const inspection = existsSync(workspace) ? workspace : repository;
  await git(inspection, "fetch", remote);
  const tracking = originTrackingRef(targetRef, remote);
  if (!(await isAncestor(inspection, beforeCleanup, tracking))) {
    return stop("before-cleanup", {
      ...where,
      publication: "not-attempted",
      reason: `${tracking} does not contain the before-cleanup commit`,
    });
  }
  const root = await observerAccess(workspace, repository);
  const published = await settleFinalClosure({
    workspace,
    inspection,
    tracking,
    branch,
    final,
    previouslyPublishedBase,
    targetRef,
    repo,
    host,
    remote,
    session,
    coordinator,
    observerDirectory,
    preferredAlias,
    root,
    storage,
    env,
  });
  if (published.stopped) {
    const { stopped, ...detail } = published;
    return stop(stopped, { ...where, ...detail });
  }
  const { acceptedSha, observation } = published;
  const accepted = {
    ...where,
    publication: "accepted",
    acceptedSha,
    target: targetRef,
    pushCount: published.pushCount,
    observation,
    startReceipt: published.startReceipt,
  };
  const visited = await settledVisit({
    workspace,
    inspection,
    tracking,
    acceptedSha,
    supplierIdentity,
    visitSettled,
    observerDirectories: published.observerDirectories,
    targetRef,
    remote,
    backlogPath,
  });
  if (visited.stopped) {
    const { stopped, ...detail } = visited;
    return stop(stopped, { ...accepted, ...detail });
  }
  if (visited.visit) accepted.visit = visited.visit;
  const completed = visited.finalSha ?? acceptedSha;
  let completion = null;
  if (observation?.directory && observation.state !== "unobserved") {
    try {
      completion = await completeRevision(observation.directory, completed, {
        root,
      });
    } catch (error) {
      completion = { error: String(error.message ?? error).slice(0, 600) };
    }
  }
  const refresh = await refreshDefaultCheckout({
    checkout: defaultCheckout,
    remote,
    integrationBranch: targetBranchName(targetRef),
  });
  if (!completion) {
    return stop("observation", { ...accepted, completion, refresh });
  }
  if (!receiptPermitsRetirement(completion)) {
    const recovery = completionRecovery(completion);
    return stop("completion", { ...accepted, completion, refresh, recovery });
  }
  const cleanup = await retireWorktree({
    repository,
    worktree: workspace,
    branch,
    remote,
    targetRef,
    identity,
    createdForWork,
    contained: [...new Set([beforeCleanup, acceptedSha, completed])],
  });
  return {
    ok: cleanup.removed === true,
    step: cleanup.removed === true ? "done" : "retire",
    ...accepted,
    ...(visited.finalSha && { visitSettled: visited.finalSha }),
    completion,
    refresh,
    cleanup,
  };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  let args;
  try {
    args = finishArguments(process.argv.slice(2));
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
  if (args) {
    let result;
    try {
      result = await finishTrunkClosure({
        ...args,
        workspace: resolve(args.workspace),
        repository: args.repository ? resolve(args.repository) : undefined,
        defaultCheckout: args.defaultCheckout
          ? resolve(args.defaultCheckout)
          : undefined,
      });
    } catch (error) {
      // A failed Git step stops closure; nothing after it ran.
      result = stop("git", { reason: "git error", error: error.message });
    }
    if (result.startReceipt) process.stdout.write(result.startReceipt);
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  }
}
