// Completion, shutdown and recovery remain distinct from publication and
// retirement. Pending and exhausted handoffs never release resources. When
// finish has no usable live or verdict-holding ended observer and the
// execution worktree still exists, completion recovery establishes or reuses
// this coordinator's observer and runs the ordinary receipt.
import { existsSync } from "node:fs";
import { completeRevision } from "../../dough-execute-plan/scripts/ci-mailbox-complete.mjs";
import { recoverExecutionCompletion } from "../../dough-execute-plan/scripts/execution-completion-recovery.mjs";

export const resourcesStay =
  "the worktree and branch stay until a success receipt confirms shutdown";

// Ownership kinds whose deliver-and-rerun gap finish replaces with the shared
// completion recovery while the execution worktree exists.
const recoverOwnership = new Set(["ended", "lost", "unavailable"]);

export function receiptPermitsRetirement(completion) {
  return (
    completion?.verdict === "success" &&
    completion?.shutdown?.status === "confirmed"
  );
}

export function completionRecovery(completion) {
  if (completion.unresolvedReason === "pending")
    return `retain the durable handoff and original deadline; resume with ${completion.handoff.command}, then rerun finish; ${resourcesStay}`;
  if (completion.unresolvedReason === "completion_expired")
    return `report the exhausted completion deadline and shutdown evidence; later green cannot reopen this handoff; ${resourcesStay}`;
  return `report the completion receipt; ${resourcesStay}. For a receipt with no verdict, rerun finish to repeat completion on this coordinator's observer, or report the closure's coverage as lost`;
}

// Completes `sha` on a selected observer, or through completion recovery when
// none is usable and the worktree exists. Returns the observation finish
// reports and the receipt, or `completion: null` when coverage stays lost.
export async function completeFinalClosure({
  observation,
  sha,
  workspace,
  root,
  targetRef,
  repo,
  host,
  session,
  coordinator,
  observerDirectory,
  preferredAlias,
  env,
  storage,
}) {
  if (observation?.directory && observation.state !== "unobserved") {
    try {
      return {
        observation,
        completion: await completeRevision(observation.directory, sha, {
          root,
        }),
      };
    } catch (error) {
      return {
        observation,
        completion: { error: String(error.message ?? error).slice(0, 600) },
      };
    }
  }
  // Claude/Cursor establish a replacement; Codex completes only when a live
  // stream is already supplied, otherwise names arming a fresh one.
  if (existsSync(workspace) && recoverOwnership.has(observation?.ownership)) {
    const recovered = await recoverExecutionCompletion({
      targetRef,
      repo,
      host,
      session,
      coordinator,
      observerDirectory,
      preferredAlias,
      env,
      storage,
      workspace,
      acceptedSha: sha,
      root,
    });
    return {
      observation: recovered.observation ?? observation,
      completion: recovered.receipt,
    };
  }
  return { observation, completion: null };
}
