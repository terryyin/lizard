// A selected completed supplier's accepted publication, then the
// current-consumer visit from the same owned workspace against its accepted
// revision. Story Branch integration, Trunk Mode `finish` and a selected
// supplier's landing share this visit. The accepted receipt stays the
// publication's; the visit reports the checkout it discovered from, or why
// that checkout could not become current.
import { resolve } from "node:path";
import { visitCurrentConsumers } from "../../dough-product-backlog/scripts/product-backlog-dependency-consumers.mjs";
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import { BacklogError } from "../../dough-product-backlog/scripts/product-backlog-refusal.mjs";
import { publishExecutionIncrement } from "./execution-increment-publication.mjs";
import {
  publishHistoryPreservingCandidate,
  trunkTarget,
} from "./history-preserving-publication.mjs";

// The visit for `acceptedSha` from `ownedWorkspace`; its failures arrive as
// a stopped visit, since the accepted publication still stands.
export function visitSelectedSupplier({
  supplierIdentity,
  ownedWorkspace,
  acceptedSha,
  targetRef = trunkTarget,
  remote = "origin",
  backlogPath = defaultBacklogPath,
}) {
  try {
    return visitCurrentConsumers(resolve(ownedWorkspace, backlogPath), {
      "supplier-identity": supplierIdentity,
      "accepted-revision": acceptedSha,
      remote,
      target: targetRef,
    });
  } catch (error) {
    if (!(error instanceof BacklogError)) throw error;
    return {
      status: "stopped",
      reason: "error",
      error: error.message,
      recovery:
        "Clear the reported failure, then run product-backlog.mjs visit-consumers with this accepted revision.",
    };
  }
}

export async function integrateSelectedSupplier({
  supplierIdentity,
  ...publication
}) {
  const result = await publishHistoryPreservingCandidate(publication);
  if (result.classification === "preserved") return result;
  const visit = visitSelectedSupplier({
    ...publication,
    supplierIdentity,
    acceptedSha: result.acceptedSha,
  });
  return { ...result, visit };
}

// A selected supplier's landing of a wholly unpublished suffix: the ordinary
// increment publication, then the visit once its candidate is accepted.
export async function publishSelectedSupplier({
  supplierIdentity,
  ...publication
}) {
  const result = await publishExecutionIncrement(publication);
  if (!result.ok) return result;
  const visit = visitSelectedSupplier({
    ...publication,
    supplierIdentity,
    ownedWorkspace: publication.workspace,
    acceptedSha: result.receipt.sha,
  });
  return { ...result, visit };
}
