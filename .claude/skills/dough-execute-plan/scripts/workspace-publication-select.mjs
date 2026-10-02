// Select an owned workspace for a Take, or the session's default checkout.
// Claim commits and publication remain separate steps.
import { existsSync, realpathSync } from "node:fs";
import { resolve } from "node:path";
import {
  fastForwardToFetchedTrunk,
  ongoingOperation,
} from "./maintain-default-checkout.mjs";
import { git, revParse } from "./publication-git.mjs";
import {
  createdForRef,
  isAncestor,
  remoteOf,
  remoteRef,
  stopped,
  trailers,
} from "./workspace-publication-ownership.mjs";

const continuable = new Set(["advanced", "already current"]);

// A repository's main worktree shares its Git directory with the common
// directory. Linked worktrees (including those of bare repositories) do not.
// Inspect before any fetch, refresh, or carried-edit park. Non-checkout paths
// keep the caller's existing selection diagnostics.
export async function mainWorktreeError(workspace) {
  if (!existsSync(workspace)) return undefined;
  try {
    const directory = await revParse(workspace, "--absolute-git-dir");
    const common = await revParse(workspace, "--git-common-dir");
    if (realpathSync(directory) === realpathSync(resolve(workspace, common)))
      return "the repository's main worktree cannot be used as a separate owned workspace; select a linked worktree instead";
  } catch {
    // Selection still owns errors for paths that are not usable checkouts.
  }
  return undefined;
}

async function verifyRetained(request) {
  const { retained } = request;
  const recovery = {
    workspace: retained.workspace,
    branch: retained.branch,
    startingRevision: retained.startingRevision,
  };
  const refused = (error) =>
    stopped("setup-failed", { recovery: { ...recovery, error } });
  try {
    const branch = (
      await git(retained.workspace, "rev-parse", "--abbrev-ref", "HEAD")
    ).stdout.trim();
    if (branch !== retained.branch) {
      return refused(`HEAD is ${branch}`);
    }
    const matches = await isAncestor(
      retained.workspace,
      retained.startingRevision,
      "HEAD",
    );
    const head = await revParse(retained.workspace, "HEAD");
    const message = (await git(retained.workspace, "log", "-1", "--format=%B"))
      .stdout;
    const owned = trailers(message);
    // A successful semantic replay rewrites the candidate onto newer trunk.
    // In that case its original base need not remain an ancestor of HEAD.
    const replayedCandidate =
      retained.candidateSha === head &&
      owned.publisher === request.publisherId &&
      owned.identity === request.identity;
    if (!matches && !replayedCandidate) {
      return refused("starting revision is not contained in HEAD");
    }
    return {
      ok: true,
      created: false,
      workspace: retained.workspace,
      branch,
      startingRevision: retained.startingRevision,
    };
  } catch (error) {
    return refused(error.stderr || error.message);
  }
}
// Creation ref for the identity, unless it is absent or invalid as a ref name.
async function creationRecord(repository, identity) {
  if (!identity) return undefined;
  const ref = createdForRef(identity);
  try {
    await git(repository, "check-ref-format", ref);
    return ref;
  } catch (error) {
    if (error.code === 1) return undefined;
    throw error;
  }
}

// Reuse a supplied fetched `base` (such as a carried escalation's park).
// Require explicit `repository` context for fetching and workspace creation;
// Git must never fall back to the process working directory.
export async function selectOwnedWorkspace(request) {
  if (request.retained?.workspace) return verifyRetained(request);
  if (!request.repository)
    return stopped("setup-failed", {
      recovery: {
        workspace: request.workspace,
        branch: request.branch,
        error: "workspace selection needs a repository",
      },
    });
  try {
    const error = await mainWorktreeError(request.workspace);
    if (error)
      return stopped("invalid-request", {
        recovery: {
          workspace: request.workspace,
          branch: request.branch,
          error,
        },
      });
    let { base } = request;
    if (!base) {
      await git(request.repository, "fetch", remoteOf(request));
      base = await revParse(request.repository, remoteRef(request));
    }
    if (existsSync(request.workspace)) {
      const actual = await revParse(request.workspace, "--show-toplevel");
      // A reused workspace continues on fetched trunk only through refresh's
      // fast-forward eligibility; its own commits, edits, and any ongoing Git
      // operation stay as they are.
      const reused =
        actual === request.workspace
          ? await fastForwardToFetchedTrunk(
              request.workspace,
              base,
              request.branch,
            )
          : { reason: "not-the-workspace-toplevel" };
      if (!continuable.has(reused.result)) {
        return stopped("setup-failed", {
          recovery: {
            workspace: request.workspace,
            branch: request.branch,
            error: `existing workspace cannot continue on fetched trunk as ${request.branch}: ${reused.reason}`,
          },
        });
      }
      return {
        ok: true,
        created: false,
        workspace: request.workspace,
        branch: request.branch,
        startingRevision: base,
      };
    }
    // Record creation ownership so later sessions can distinguish reuse.
    const record = await creationRecord(request.repository, request.identity);
    await git(
      request.repository,
      "worktree",
      "add",
      "-b",
      request.branch,
      request.workspace,
      base,
    );
    if (record) await git(request.workspace, "update-ref", record, base);
    return {
      ok: true,
      created: true,
      workspace: request.workspace,
      branch: request.branch,
      startingRevision: base,
    };
  } catch (error) {
    return stopped("setup-failed", {
      recovery: {
        workspace: request.workspace,
        branch: request.branch,
        error: `${request.workspace}: ${error.stderr || error.message}`,
      },
    });
  }
}

// Normalize the existing default checkout as its own repository on target,
// without a separate integration checkout, or return `{ error }`. Supplied
// integration/repository paths must name that checkout; branch must name target.
export function defaultCheckoutRequest(input) {
  const workspace = resolve(input.workspace);
  const { integration, repository, branch, target } = input;
  if (!existsSync(workspace))
    return {
      error: `--default-main works in the existing default checkout; ${workspace} does not exist`,
    };
  for (const [flag, path] of [
    ["--integration", integration],
    ["--repository", repository],
  ])
    if (path && resolve(path) !== workspace)
      return {
        error: `--default-main works in the default checkout itself; ${flag} must name ${workspace} or be omitted`,
      };
  if (branch && branch !== target)
    return {
      error: `--default-main works on the target branch ${target}; omit --branch or name ${target}`,
    };
  return {
    request: {
      ...input,
      integration: undefined,
      workspace,
      repository: workspace,
      branch: target,
    },
  };
}

// Take the default checkout as is: preserve HEAD, index, edits, and local
// commits; never reset, refresh, fast-forward, or create a worktree or branch.
// Require checkout top level, target branch, and no ongoing Git operation;
// refusals change nothing. `startingRevision` is its actual HEAD.
export async function selectDefaultCheckout({ workspace, target }) {
  const refused = (error) => stopped("setup-failed", { workspace, error });
  try {
    const toplevel = await revParse(workspace, "--show-toplevel");
    if (toplevel !== realpathSync(workspace))
      return refused(`${workspace} is not a checkout's top level`);
    const operation = await ongoingOperation(workspace);
    if (operation)
      return refused(
        `the default checkout has an ongoing Git operation (${operation}); finish or abort it first`,
      );
    const branch = (
      await git(workspace, "branch", "--show-current")
    ).stdout.trim();
    if (branch !== target)
      return refused(
        `the default checkout is on ${branch ? `branch ${branch}` : "a detached HEAD"}, not the target branch ${target}; switch it to ${target} yourself, or work in an isolated workspace`,
      );
    return {
      ok: true,
      role: "default-checkout",
      created: false,
      workspace,
      branch,
      startingRevision: await revParse(workspace, "HEAD"),
    };
  } catch (error) {
    return refused(`${workspace}: ${error.stderr || error.message}`);
  }
}
