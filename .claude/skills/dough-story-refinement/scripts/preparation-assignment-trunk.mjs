// Fetched remote trunk as a queued story's preparation starting point: the
// story must be queued there, and a workspace that does not exist yet is
// created at it, never at the integration checkout's local revision.
import { existsSync, realpathSync } from "node:fs";
import { queueHeading } from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import {
  git,
  revParse,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import { remoteRef } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import {
  mainWorktreeError,
  selectOwnedWorkspace,
} from "../../dough-execute-plan/scripts/workspace-publication-select.mjs";
import {
  errorText,
  stop,
  storyListAt,
} from "./preparation-assignment-ownership.mjs";

// Fetches the target from `cwd` and returns its revision when the story is
// queued there, or the stop that refuses the request.
export async function fetchQueuedTrunk(cwd, request) {
  const { workspace, remote, identity } = request;
  const ref = remoteRef(request);
  let fetched;
  try {
    await git(cwd, "fetch", "--quiet", remote);
    fetched = await revParse(cwd, ref);
  } catch (error) {
    return stop("source-refused", { workspace, error: errorText(error) });
  }
  if ((await storyListAt(cwd, ref, identity)) !== queueHeading)
    return stop("not-queued", {
      workspace,
      fetched,
      error: `${identity} is not queued on ${ref}`,
    });
  return { ok: true, fetched };
}

// The repository a start fetches from and creates a missing workspace with:
// the integration checkout, else the repository context, else an existing
// workspace itself. A workspace that does not exist yet also needs its
// branch; without these, the stop that refuses the request.
export function preparationRepository(request) {
  const { workspace, branch } = request;
  const exists = existsSync(workspace);
  const repository =
    request.integration ?? request.repository ?? (exists ? workspace : null);
  if (repository && (exists || branch)) return { ok: true, repository, exists };
  return stop("invalid-request", {
    workspace,
    error:
      "a workspace that does not exist yet needs --branch and --integration or --repository to create it",
  });
}

// Selects the owned workspace at queued fetched trunk `fetched`, creating it
// on its branch when its path does not exist yet, so a refused request leaves
// nothing behind.
export async function selectAtFetchedTrunk(request, repository, fetched) {
  const selected = await selectOwnedWorkspace({
    ...request,
    repository,
    base: fetched,
  });
  if (selected.ok) return selected;
  const { workspace, branch } = request;
  return stop("workspace-selection-failed", {
    workspace,
    ...(branch ? { branch } : {}),
    fetched,
    error: selected.recovery.error,
  });
}

// Verifies that a retained start's workspace still exists as its own checkout
// on its branch, or the stop that refuses continuing it.
async function verifyRetainedWorkspace(request) {
  const { workspace, branch } = request;
  try {
    if (!existsSync(workspace))
      throw new Error("the retained workspace is missing");
    const root = (
      await git(workspace, "rev-parse", "--show-toplevel")
    ).stdout.trim();
    const current = (
      await git(workspace, "symbolic-ref", "--short", "HEAD")
    ).stdout.trim();
    if (
      realpathSync(root) !== realpathSync(workspace) ||
      !branch ||
      current !== branch
    )
      throw new Error(
        "the retained workspace or branch differs from this request",
      );
    await revParse(workspace, `refs/heads/${branch}`);
    return { ok: true };
  } catch (error) {
    return stop("continuation-refused", {
      workspace,
      error: errorText(error),
    });
  }
}

// Creates a missing owned workspace for an announcement. An existing path is
// the caller's owned workspace, verified by the announcement. A continuation
// only verifies its retained workspace.
export async function selectPreparationWorkspace(request) {
  if (request.continueOnly) return verifyRetainedWorkspace(request);
  const error = await mainWorktreeError(request.workspace);
  if (error)
    return stop("workspace-selection-failed", {
      workspace: request.workspace,
      error,
    });
  const located = preparationRepository(request);
  if (!located.ok) return located;
  if (located.exists) return { ok: true };
  const { repository } = located;
  const trunk = await fetchQueuedTrunk(repository, request);
  if (!trunk.ok) return trunk;
  const selected = await selectAtFetchedTrunk(
    request,
    repository,
    trunk.fetched,
  );
  if (!selected.ok) return selected;
  return {
    ok: true,
    selection: {
      created: true,
      branch: selected.branch,
      startingRevision: selected.startingRevision,
    },
  };
}
