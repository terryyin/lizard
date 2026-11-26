// A Dashboard admission can use an owned claim checkout while the native
// session runs in a borrowed checkout. Transfer its accepted evidence only
// when finishing that claim, without changing the borrowed checkout.
import { existsSync, realpathSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { dirname, join } from "node:path";
import { ownershipHold } from "../../dough-land/scripts/retirement-checks.mjs";
import { parseBacklog } from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { doneRecordFileName } from "../../dough-product-backlog/scripts/product-backlog-done-record.mjs";
import {
  currentLandingInput,
  readLandingInput,
} from "../../dough-execute-plan/scripts/dashboard-landing.mjs";
import {
  readAcceptedLaunchEvidence,
  readSessionEvidence,
  retainAdmittedSession,
} from "../../dough-execute-plan/scripts/execution-session-evidence.mjs";
import { managementContext } from "../../dough-execute-plan/scripts/publication-git.mjs";
import { isSessionId } from "../../dough-execute-plan/scripts/session-record.mjs";
import {
  backlogPath as defaultBacklogPath,
  fileAt,
  isAncestor,
} from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";

const readJson = async (file) => JSON.parse(await readFile(file, "utf8"));

export function admittedRefreshCheckout(checkout, borrowed) {
  if (!checkout || !borrowed) return checkout;
  if (checkout === borrowed) return undefined;
  return existsSync(checkout) &&
    existsSync(borrowed) &&
    realpathSync(checkout) === realpathSync(borrowed)
    ? undefined
    : checkout;
}

async function firstLanding(file) {
  let pending;
  try {
    pending = await currentLandingInput(file);
  } catch (error) {
    if (error.code !== "ENOENT") throw error;
    return undefined;
  }
  return readLandingInput(pending);
}

export async function adoptAdmittedSession({
  admittedLaunchContext,
  workspace,
  branch,
  identity,
  repository,
  remote,
  targetRef,
  tracking,
  final,
  backlogPath = defaultBacklogPath,
}) {
  const file = admittedLaunchContext;
  const context = await readJson(file);
  const claim = await readJson(join(dirname(file), "admitted-claim.json"));
  if (
    !isSessionId(context.reference) ||
    !identity ||
    claim.identity !== identity ||
    context.identity !== identity ||
    context.reference !== claim.reference ||
    context.origin !== claim.origin ||
    context.source !== claim.source ||
    context.host !== claim.host ||
    context.remote !== remote ||
    context.target !== targetRef ||
    claim.remote !== remote ||
    claim.target !== targetRef ||
    claim.admission.workspace !== workspace ||
    claim.admission.branch !== branch ||
    claim.nativeWorkspace === workspace
  ) {
    throw new Error("The Dashboard admission does not name this owned claim.");
  }
  if (!existsSync(workspace)) return claim.nativeWorkspace;
  if (
    (await managementContext(claim.nativeWorkspace)) !== repository ||
    (await ownershipHold(workspace, identity, false)) ||
    !(await isAncestor(workspace, claim.admission.publishedSha, tracking)) ||
    !(await isAncestor(workspace, claim.admission.publishedSha, branch))
  ) {
    throw new Error(
      "The published claim or its checkout ownership could not be verified.",
    );
  }
  const donePath = join(
    dirname(backlogPath),
    "done",
    doneRecordFileName(identity),
  );
  const done = await fileAt(workspace, final, donePath);
  const backlog = await fileAt(workspace, final, backlogPath);
  if (
    !done ||
    JSON.parse(done).identity !== identity ||
    !backlog ||
    parseBacklog(backlog).entries.some((entry) => entry.identity === identity)
  ) {
    throw new Error(
      "The final closure must record the admitted story as Done.",
    );
  }

  const kept = readSessionEvidence(workspace);
  if (kept?.sessionId === context.reference && kept.story === identity)
    return claim.nativeWorkspace;
  if (kept?.story !== identity || kept.closedAt || kept.ranges.length !== 0)
    throw new Error(
      "This checkout has different session evidence to preserve.",
    );

  const landed = await firstLanding(file);
  const exact = readAcceptedLaunchEvidence(
    claim.nativeWorkspace,
    context.reference,
  );
  const native = exact ?? readSessionEvidence(claim.nativeWorkspace);
  const matchesLaunch =
    native?.sessionId === context.reference && native.story === identity;
  const ranges = matchesLaunch ? native.ranges : [];
  if (
    (exact !== undefined && !matchesLaunch) ||
    (landed &&
      (landed.reference !== context.reference ||
        !ranges.some(
          ({ base, candidate }) =>
            base === landed.base && candidate === landed.revision,
        )))
  ) {
    throw new Error(
      "The native landing has no matching accepted session range.",
    );
  }
  for (const { base, candidate } of ranges) {
    if (
      !(await isAncestor(workspace, base, candidate)) ||
      !(await isAncestor(workspace, candidate, tracking))
    ) {
      throw new Error("A native comparison is outside published trunk.");
    }
  }
  retainAdmittedSession(workspace, {
    sessionId: context.reference,
    story: identity,
    ranges,
  });
  return claim.nativeWorkspace;
}
