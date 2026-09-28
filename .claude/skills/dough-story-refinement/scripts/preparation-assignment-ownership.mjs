// Which preparation assignment a workspace owns, and what became of it. An
// assignment is identified by its profile path plus the commit that added it
// (its allocation), never by story identity, timestamps, or tool/model alone.
// The workspace remembers its own announcement commit under a per-worktree
// ref, recorded before the announcement is pushed. Trunk history alone cannot
// tell a workspace's own allocation from a later one of the same name that a
// fast-forward brought into the workspace, so only that record counts. When
// that workspace is lost, a developer addresses the assignment instead by its
// profile path and allocation (preparation-assignment-lost-workspace.mjs).
import { basename, dirname, join, resolve } from "node:path";
import {
  agentIdentity,
  agentProfileDirectory,
  agentReportError,
  profileAgentName,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import {
  addedProfile,
  profileAllocation,
} from "../../dough-execute-plan/scripts/agent-assignments.mjs";
import { parseBacklog } from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { git } from "../../dough-execute-plan/scripts/publication-git.mjs";
import {
  backlogPath,
  fileAt,
  isAncestor,
} from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";

export function stop(status, fields) {
  return { ok: false, status, ...fields };
}

export const errorText = (error) => error.stderr || error.message;

// Repository path of a rotation name's profile beside the backlog.
export const profilePathOf = (name) =>
  join(dirname(backlogPath), agentIdentity(name).path);

// Operations that publish to trunk need authority and a separate workspace.
const publishing = new Set(["start", "abandon"]);

// Why an abandonment addressed by profile path cannot be read as one, or
// undefined when it can: a lost workspace's assignment is named by its
// profile beside the backlog and its allocation, not by workspace or story.
function addressingError(input) {
  if (input.workspace !== undefined || input.identity !== undefined)
    return "--profile addresses an assignment by its allocation; do not also name --workspace or --identity";
  const directory = join(dirname(backlogPath), agentProfileDirectory);
  if (
    dirname(input.profile) !== directory ||
    !profileAgentName(basename(input.profile))
  )
    return `--profile must name an agent profile under ${directory}/`;
  return undefined;
}

// The validated request for an operation, or a stop saying why not.
export function requestOf(operation, input) {
  const addressed = operation === "abandon" && input.profile !== undefined;
  const required = addressed
    ? ["profile", "target"]
    : ["workspace", "identity", "target"];
  if (publishing.has(operation)) required.push("integration");
  for (const field of required)
    if (!input[field])
      return stop("invalid-request", { error: `missing ${field}` });
  const invalid = addressed && addressingError(input);
  if (invalid) return stop("invalid-request", { error: invalid });
  const request = {
    ...input,
    remote: input.remote ?? "origin",
    ...(input.workspace ? { workspace: resolve(input.workspace) } : {}),
    ...(input.integration ? { integration: resolve(input.integration) } : {}),
  };
  const reportError = agentReportError(request);
  if (reportError) return stop("invalid-request", { error: reportError });
  if (publishing.has(operation)) {
    if (request.pushAuthorized !== true)
      return stop("authority-required", {
        error: "trunk publication authority must be established",
      });
    if (request.integration === request.workspace)
      return stop("invalid-request", {
        error: "preparation requires a separate owned workspace",
      });
  }
  return { ok: true, request };
}

// The backlog list holding story `identity` on trunk `ref` ("Backlog list"
// or "Taken"), or undefined when the backlog there lists it nowhere.
export async function storyListAt(cwd, ref, identity) {
  const text = await fileAt(cwd, ref, backlogPath);
  if (text === null) return undefined;
  return parseBacklog(text).entries.find((entry) => entry.identity === identity)
    ?.list;
}

const recordRef = "refs/worktree/dough/preparation-assignment";

// Remembers `sha` as this workspace's own announcement commit.
export async function recordAllocation(workspace, sha) {
  await git(workspace, "update-ref", recordRef, sha);
}

// Puts back the record a workspace held before (`sha`), or none.
export async function restoreAllocation(workspace, sha) {
  if (sha) await recordAllocation(workspace, sha);
  else await git(workspace, "update-ref", "-d", recordRef);
}

export const recordedAllocation = (workspace) => commitOf(workspace, recordRef);

// The preparation profile the announcement commit `sha` added, as an
// assignment: its name, path, allocation, and recorded facts. `only` narrows
// it to the profile of that rotation name.
export async function announcedAssignment(cwd, sha, only) {
  const added = await addedProfile(
    cwd,
    sha,
    backlogPath,
    (profile, name) =>
      profile.activity === "preparation" &&
      profile.name === name &&
      (only === undefined || name === only),
  );
  return added && { ...added, allocation: sha };
}

// What became of the assignment this workspace announced, read from the
// fetched trunk `ref`:
// - `held`: trunk still records that exact allocation;
// - `ended`: trunk history removed it, naming the removing commit and any
//   later allocation of the same name, which is never this workspace's;
// - `unconfirmed`: trunk never took the recorded announcement;
// - `none`: this workspace recorded no announcement for `identity`.
// `assigned` names a still-held assignment the workspace records for another
// story than the request names. A caller that also needs the workspace's
// record passes its pending read as `recorded`.
export async function workspaceAssignment(
  request,
  ref,
  recorded = recordedAllocation(request.workspace),
) {
  const { workspace, identity } = request;
  const sha = await recorded;
  const own = sha && (await announcedAssignment(workspace, sha));
  if (!own) return { state: "none" };
  const named = own.profile.identity === identity;
  if (!(await isAncestor(workspace, sha, ref)))
    return named ? { state: "unconfirmed", own } : { state: "none" };
  const current =
    (await fileAt(workspace, ref, own.path)) === null
      ? undefined
      : await profileAllocation(workspace, ref, own.path);
  if (!named)
    return current === sha
      ? { state: "none", assigned: own }
      : { state: "none" };
  if (current === sha) return { state: "held", own };
  return endedAssignment(workspace, ref, own, current);
}

// An allocation `own` that trunk `ref` no longer holds: the commit that
// removed it, and `current`, any later allocation of the same name.
export async function endedAssignment(cwd, ref, own, current) {
  const { stdout } = await git(
    cwd,
    "log",
    "--reverse",
    "--diff-filter=D",
    "--format=%H",
    `${own.allocation}..${ref}`,
    "--",
    own.path,
  );
  return {
    state: "ended",
    own,
    endedBy: stdout.split("\n")[0] || undefined,
    ...(current === undefined ? {} : { successor: current }),
  };
}

// The receipt fields of a preparation assignment of the story `identity`.
export function assignmentFields(
  { identity },
  { name, path, allocation, profile },
) {
  return {
    activity: "preparation",
    identity,
    agent: agentIdentity(name).agent,
    profile: path,
    allocation,
    ...(profile.host === undefined ? {} : { host: profile.host }),
    ...(profile.model === undefined ? {} : { model: profile.model }),
  };
}

// The stop for a workspace with no assignment of the identity to act on.
export function noAssignment(request, ref, found, action) {
  const reason =
    found.state === "unconfirmed"
      ? `its announcement ${found.own.allocation} never reached ${ref}`
      : `${ref} holds no preparation assignment of ${request.identity} announced from this workspace`;
  return stop("no-assignment", {
    workspace: request.workspace,
    error: `${reason}; nothing was ${action}`,
  });
}

// The receipt for an assignment trunk already ended: nothing more to do.
export function alreadyReleased(request, found) {
  return {
    ok: true,
    status: "already-released",
    ...assignmentFields(found.own.profile, found.own),
    endedBy: found.endedBy,
    ...(found.successor ? { successor: found.successor } : {}),
    workspace: request.workspace,
  };
}

// The commit `rev` names in `cwd`, or undefined when it names none.
export async function commitOf(cwd, rev) {
  if (rev === undefined) return undefined;
  try {
    const { stdout } = await git(
      cwd,
      "rev-parse",
      "--verify",
      "-q",
      `${rev}^{commit}`,
    );
    return stdout.trim();
  } catch {
    return undefined;
  }
}
