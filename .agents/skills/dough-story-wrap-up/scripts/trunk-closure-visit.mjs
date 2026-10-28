// The selected supplier's current-consumer visit inside Trunk Mode `finish`.
// After the final closure is accepted, `finish` visits from the execution
// worktree. A clean visit, with no consumers and no discovery problems, lets
// completion and retirement continue for the accepted final closure in the
// same `finish`. Otherwise `finish` stops with that visit for the agent's
// judgment, keeping the worktree, branch and observer. Only `--visit-settled`,
// the last accepted SHA once the visit's owned publications finished or its
// unresolved work was retained, lets completion and retirement continue, for
// that SHA; a later revision this closure's own observers registered on the
// target refuses it, so completion cannot skip a consumer publication.
import { existsSync } from "node:fs";
import { visitSelectedSupplier } from "../../dough-execute-plan/scripts/supplier-integration.mjs";
import { isAncestor } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import { listRegisteredRevisions } from "../../dough-execute-plan/scripts/ci-mailbox-revision-records.mjs";
import { commitOf } from "./trunk-closure-settlement.mjs";

const keep = "the worktree, branch and observer stay";

// The latest revision `observerDirectories` registered that `tracking` holds
// and that strictly descends from `settled`, or null when none does. A target
// revision another writer published is never registered there.
async function laterRegistered(inspection, tracking, settled, directories) {
  let latest = null;
  for (const revision of new Set(
    directories.flatMap(listRegisteredRevisions),
  )) {
    if (revision === settled) continue;
    // An observer may have registered a revision this repository lacks.
    if (!(await commitOf(inspection, revision))) continue;
    if (!(await isAncestor(inspection, settled, revision))) continue;
    if (!(await isAncestor(inspection, revision, tracking))) continue;
    if (!latest || (await isAncestor(inspection, latest, revision)))
      latest = revision;
  }
  return latest;
}

// `{}` without a supplier; `{ finalSha }` for a settled visit; `{ visit }` for
// a clean visit; otherwise `stopped: "visit"` with the visit or the reason
// none could run or settle.
export async function settledVisit({
  workspace,
  inspection,
  tracking,
  acceptedSha,
  supplierIdentity,
  visitSettled,
  observerDirectories = [],
  targetRef,
  remote,
  backlogPath,
}) {
  if (!supplierIdentity) return {};
  if (visitSettled) {
    const finalSha = await commitOf(inspection, visitSettled);
    if (
      finalSha &&
      (await isAncestor(inspection, acceptedSha, finalSha)) &&
      (await isAncestor(inspection, finalSha, tracking))
    ) {
      const later = await laterRegistered(
        inspection,
        tracking,
        finalSha,
        observerDirectories,
      );
      if (!later) return { finalSha };
      return {
        stopped: "visit",
        reason: `${tracking} holds ${later}, a later publication this closure's observers registered after --visit-settled ${visitSettled}`,
        recovery: `name ${later}, or a later accepted SHA of this visit's publications, as --visit-settled and rerun finish; ${keep}`,
      };
    }
    return {
      stopped: "visit",
      reason: `${tracking} holds no --visit-settled ${visitSettled} that contains the accepted final closure`,
      recovery: `name the last accepted SHA of this visit's publications, or the accepted final closure when it published none, as --visit-settled and rerun finish; ${keep}`,
    };
  }
  if (inspection !== workspace || !existsSync(workspace)) {
    return {
      stopped: "visit",
      reason:
        "execution worktree is absent, so no owned checkout can visit current consumers",
      recovery: `if a clean visit with no consumers and no problems was printed by an earlier finish, rerun finish with --final ${acceptedSha} and --visit-settled ${acceptedSha}; otherwise report the unfinished consumer visit with the accepted final closure, since no owned checkout can establish its current consumers`,
    };
  }
  const visit = visitSelectedSupplier({
    supplierIdentity,
    ownedWorkspace: workspace,
    acceptedSha,
    targetRef,
    remote,
    backlogPath,
  });
  if (
    visit.status === "visited" &&
    visit.consumers.length === 0 &&
    visit.problems.length === 0
  )
    return { visit };
  const resume = `rerun finish with --final ${acceptedSha} and --visit-settled set to the last accepted SHA: a consumer publication, or ${acceptedSha} when none`;
  return {
    stopped: "visit",
    visit,
    recovery:
      visit.status === "visited"
        ? `judge each visited consumer under the supplier dependency procedure, publish owned consumer changes from this worktree through deliver on this coordinator's observer, and retain unresolved work in active context; then ${resume}; ${keep}`
        : `follow visit.recovery and rerun finish to visit again, or retain the unfinished visit and ${resume}; ${keep}`,
  };
}
