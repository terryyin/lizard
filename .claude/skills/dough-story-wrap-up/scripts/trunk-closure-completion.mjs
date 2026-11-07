// Completion, shutdown and recovery remain distinct from publication and
// retirement. Pending and exhausted handoffs never release resources.
export const resourcesStay =
  "the worktree and branch stay until a success receipt confirms shutdown";

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
