// The accepted start result: publication and recovery coordinates, what the
// command resolved beyond its invocation, and local maintenance reported
// separately. Unchanged invocation context is not echoed back.
import { reportedMaintenance } from "./execution-start-maintenance.mjs";
import { remoteOf } from "./workspace-publication-ownership.mjs";

export function acceptedReceipt(
  request,
  selected,
  source,
  publication,
  beforeMaintenance,
  afterMaintenance,
) {
  return {
    ok: true,
    status: publication.status,
    publishedSha: publication.publishedSha,
    // Resume flags take both SHAs, even when they match.
    startingRevision: selected.startingRevision,
    candidateSha: publication.candidateSha,
    ...(publication.agent ? { agent: publication.agent } : {}),
    ...(publication.workspaceAuthorship === "not-configured"
      ? { workspaceAuthorship: publication.workspaceAuthorship }
      : {}),
    ...(request.remote ? {} : { remote: remoteOf(request) }),
    ...(request.plan || !source.planTarget ? {} : { plan: source.planTarget }),
    // The drafted canonical files this admission published from the
    // originating checkout, whose own copies stay as they were.
    ...(source.admission
      ? { admitted: source.admission.files.map(({ path }) => path) }
      : {}),
    created: publication.created ?? selected.created,
    ...reportedMaintenance(afterMaintenance, beforeMaintenance),
  };
}

// A prepared one-shot start: the owned workspace's fetched-trunk base, which
// the result's managed delivery takes as its previously published base.
// Nothing was published.
export function preparedReceipt(request, selected, maintained) {
  return {
    ok: true,
    status: "prepared",
    startingRevision: selected.startingRevision,
    ...(request.remote ? {} : { remote: remoteOf(request) }),
    created: selected.created,
    ...reportedMaintenance(maintained),
  };
}

// A prepared one-shot start in the default checkout: the selected checkout's
// role, path, target branch, and actual HEAD as `startingRevision` (which may
// hold local commits and sits beside any uncommitted content), and the
// fetched trunk it was checked against. Nothing was created, refreshed, or
// published.
export function defaultCheckoutReceipt(request, selected, fetched) {
  return {
    ...selected,
    status: "prepared",
    fetched,
    ...(request.remote ? {} : { remote: remoteOf(request) }),
  };
}
