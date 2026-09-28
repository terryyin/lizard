// Which preparation assignment a developer addresses when the workspace that
// announced it is lost: named by its profile path and allocation and read in
// the integration checkout, it is ended only once the developer confirms that
// exact allocation is abandoned, never a later allocation of the same name.
import { basename } from "node:path";
import {
  agentIdentity,
  parseAgentProfileFile,
  profileAgentName,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { profileAllocation } from "../../dough-execute-plan/scripts/agent-assignments.mjs";
import {
  fileAt,
  isAncestor,
} from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import {
  announcedAssignment,
  assignmentFields,
  commitOf,
  endedAssignment,
  stop,
} from "./preparation-assignment-ownership.mjs";

// What trunk `ref` holds at the addressed profile path, read in the
// integration checkout, for ending a lost workspace's assignment:
// - `held`: the request confirms the exact allocation trunk holds, which is a
//   preparation assignment;
// - `ended`: trunk already removed the addressed allocation; a later
//   allocation of the same name is its `successor` and stays;
// - `stopped`: nothing may be published; `receipt` says why and, while the
//   name is held, reports the assignment holding it and its allocation.
export async function addressedAssignment(request, ref) {
  const { integration: cwd, profile: path } = request;
  const text = await fileAt(cwd, ref, path);
  const current =
    text === null ? undefined : await profileAllocation(cwd, ref, path);
  const name = profileAgentName(basename(path));
  const addressed = await commitOf(cwd, request.allocation);
  if (
    addressed &&
    addressed !== current &&
    (await isAncestor(cwd, addressed, ref))
  ) {
    const own = await announcedAssignment(cwd, addressed, name);
    if (own) return endedAssignment(cwd, ref, own, current);
  }
  const refused = (status, fields, error) => ({
    state: "stopped",
    receipt: stop(status, {
      ...fields,
      error: `${error}; nothing was published`,
    }),
  });
  if (text === null)
    return refused(
      "no-assignment",
      { profile: path },
      `${ref} holds no assignment at ${path} that the addressed allocation added`,
    );
  const read = parseAgentProfileFile(basename(path), text);
  if (!read.ok)
    return refused(
      "not-preparation",
      { profile: path, allocation: current },
      `${path} on ${ref} is not a readable preparation assignment`,
    );
  const held = { name, path, allocation: current, profile: read.profile };
  if (read.profile.activity !== "preparation")
    return refused(
      "not-preparation",
      {
        activity: read.profile.activity,
        identity: read.profile.identity,
        agent: agentIdentity(name).agent,
        profile: path,
        allocation: current,
      },
      `${path} records ${read.profile.activity}, which only its own completion releases`,
    );
  const fields = assignmentFields(read.profile, held);
  if (request.allocation !== undefined && addressed !== current)
    return refused(
      "allocation-mismatch",
      { ...fields, requestedAllocation: request.allocation },
      `${ref} holds allocation ${current} of ${path}, not ${request.allocation}`,
    );
  if (request.allocation === undefined || request.confirmedAbandoned !== true)
    return refused(
      "confirmation-required",
      fields,
      "only the developer's confirmation that this exact allocation is abandoned releases it: rerun with --allocation <allocation> --confirmed-abandoned once confirmed",
    );
  return { state: "held", own: held };
}
