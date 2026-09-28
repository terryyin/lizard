// The selected source a start reads from fetched trunk: queued work,
// admission of accepted work that no backlog list holds yet, or one-shot work.
import { readAdmissionSource } from "./execution-admission-source.mjs";
import {
  maintenance,
  reportedMaintenance,
} from "./execution-start-maintenance.mjs";
import { readPublishedExecutionSource } from "./execution-source.mjs";
import { requireOneShotStart } from "./one-shot-ownership.mjs";
import { preparedReceipt } from "./execution-start-receipt.mjs";
import { sameSelectedSource } from "./execution-start-recovery.mjs";
import { selectOwnedWorkspace } from "./workspace-publication-select.mjs";
import {
  backlogPath,
  claimProvenance,
  stopped,
} from "./workspace-publication-ownership.mjs";

// A one-shot start claims nothing: it only checks how fetched trunk holds a
// supplied identity. An unlisted request, with no identity or one no backlog
// list holds, has nothing to check.
async function readOneShotSource({ integration, identity }, ref) {
  if (identity)
    await requireOneShotStart(integration, ref, identity, backlogPath);
  return {};
}

// The one-shot start: the owned workspace at fetched trunk, with nothing
// published. Its result goes through managed delivery.
export async function prepareOneShot(request, origin, fetched) {
  const maintained = await maintenance(request);
  const selected = await selectOwnedWorkspace({ ...request, origin });
  if (!selected.ok)
    return { ...selected, fetched, ...reportedMaintenance(maintained) };
  return preparedReceipt(request, selected, maintained);
}

// `read(request, ref, candidateSha)` reads it on fetched trunk; `changed`
// tells whether a source reread during claim publication no longer supports
// the claim. An admission's source is its claim candidate once one exists (a
// resumed start's retained candidate by default), reconciled again onto the
// trunk it reads rather than drafted anew, so it only changes when the work
// has meanwhile been listed. A one-shot source publishes no claim to recheck.
export function startSource(request) {
  if (request.oneShot === true)
    return { oneShot: true, read: readOneShotSource };
  if (request.admit === true)
    return {
      admitting: true,
      read: (reader, ref, candidateSha = request.retained?.candidateSha) =>
        readAdmissionSource(reader, ref, candidateSha),
      changed: (refreshed) => Boolean(refreshed.existing),
    };
  return {
    admitting: false,
    read: readPublishedExecutionSource,
    changed: (refreshed, selected) => !sameSelectedSource(refreshed, selected),
    // A resumed claim's source must still be the one its claim was built on.
    async retainedBasis(reader, selected) {
      const original = await readPublishedExecutionSource(
        reader,
        reader.retained.startingRevision,
      );
      if (!sameSelectedSource(original, selected))
        throw new Error(
          "selected published source changed since retained claim basis",
        );
    },
  };
}

// Work already Taken continues under the claim this publisher already holds:
// an admission of it, or an ordinary start once its published preparation
// supports execution. Any other claim is refused. Nothing is written. The
// source reports the claim when it already read that provenance.
export async function existingClaim(request, ref, source = {}) {
  const provenance =
    "claim" in source
      ? source.claim
      : await claimProvenance(
          request.integration,
          ref,
          request.identity,
          backlogPath,
        );
  if (provenance?.publisher !== request.publisherId)
    return stopped("conflict", {
      ownership: provenance?.publisher ? "other" : "ambiguous",
      provenance,
      error: "selected identity is already Taken under another claim",
    });
  return {
    ok: true,
    status: "existing",
    publishedSha: provenance.sha,
    created: false,
    ...(request.plan || !source.planTarget ? {} : { plan: source.planTarget }),
    ...reportedMaintenance(await maintenance(request)),
  };
}
