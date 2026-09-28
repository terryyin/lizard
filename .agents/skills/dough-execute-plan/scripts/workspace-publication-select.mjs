// Select or reuse the owned execution workspace for a Take. Committing the
// claim there is workspace-publication-claim.mjs; publication of that SHA is a
// separate step.
import { existsSync } from "node:fs";
import { git, revParse } from "./publication-git.mjs";
import {
  isAncestor,
  remoteOf,
  remoteRef,
  stopped,
  trailers,
} from "./workspace-publication-ownership.mjs";

async function verifyRetained(request) {
  const { retained } = request;
  const recovery = {
    workspace: retained.workspace,
    branch: retained.branch,
    startingRevision: retained.startingRevision,
  };
  try {
    const branch = (
      await git(retained.workspace, "rev-parse", "--abbrev-ref", "HEAD")
    ).stdout.trim();
    if (branch !== retained.branch) {
      return stopped("setup-failed", {
        recovery: { ...recovery, error: `HEAD is ${branch}` },
      });
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
      return stopped("setup-failed", {
        recovery: {
          ...recovery,
          error: "starting revision is not contained in HEAD",
        },
      });
    }
    return {
      ok: true,
      created: false,
      workspace: retained.workspace,
      branch,
      startingRevision: retained.startingRevision,
    };
  } catch (error) {
    return stopped("setup-failed", {
      recovery: { ...recovery, error: error.stderr || error.message },
    });
  }
}

// A supplied `base` is fetched trunk the caller already reset the workspace
// to, such as a carried escalation's park; it is used without fetching again.
export async function selectOwnedWorkspace(request) {
  if (request.retained?.workspace) return verifyRetained(request);
  try {
    let { base } = request;
    if (!base) {
      await git(request.integration, "fetch", remoteOf(request));
      base = await revParse(request.integration, remoteRef(request));
    }
    if (existsSync(request.workspace)) {
      const actual = await revParse(request.workspace, "--show-toplevel");
      const branch = (
        await git(request.workspace, "branch", "--show-current")
      ).stdout.trim();
      const head = await revParse(request.workspace, "HEAD");
      const status = (await git(request.workspace, "status", "--porcelain"))
        .stdout;
      if (
        actual !== request.workspace ||
        branch !== request.branch ||
        head !== base ||
        status !== ""
      ) {
        return stopped("setup-failed", {
          recovery: {
            workspace: request.workspace,
            branch: request.branch,
            error:
              "existing workspace does not match clean fetched trunk and owned branch",
          },
        });
      }
      return {
        ok: true,
        created: false,
        workspace: request.workspace,
        branch,
        startingRevision: base,
      };
    }
    await git(
      request.integration,
      "worktree",
      "add",
      "-b",
      request.branch,
      request.workspace,
      base,
    );
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
