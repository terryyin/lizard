// Rechecks a one-shot preparation's story on freshly fetched trunk before its
// retained result is pushed: no Taken entry or agent profile (execution or
// preparation) names it, and it is still in the Backlog list, or the
// workspace's result is already on trunk. A recorded not-ready assessment
// does not stop it: the result may be what repairs it. Publishes nothing.
import { queuedOwnershipGuard } from "../../dough-execute-plan/scripts/one-shot-ownership.mjs";
import {
  git,
  revParse,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import { remoteRef } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import { errorText, stop } from "./preparation-assignment-ownership.mjs";
import { requestOf } from "./preparation-assignment-request.mjs";

export async function recheckOneShotPreparation(input) {
  const requested = requestOf("recheck", input);
  if (!requested.ok) return requested;
  const { request } = requested;
  const { workspace, identity, remote } = request;
  const ref = remoteRef(request);
  let fetched, candidate;
  try {
    await git(workspace, "fetch", "--quiet", remote);
    fetched = await revParse(workspace, ref);
    candidate = await revParse(workspace, "HEAD");
  } catch (error) {
    return stop("source-refused", { workspace, error: errorText(error) });
  }
  const guard = queuedOwnershipGuard({ workspace, identity });
  const changed = await guard({ candidate, remoteTip: fetched });
  if (changed)
    return stop(changed.status, { workspace, fetched, ...changed.fields });
  return {
    ok: true,
    status: "queued",
    identity,
    workspace,
    fetched,
    candidate,
  };
}
