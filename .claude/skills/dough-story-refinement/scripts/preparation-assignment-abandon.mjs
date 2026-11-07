// Explicit abandonment of a preparation: publishes the end of one assignment
// as a coordination-only commit on fetched trunk that removes exactly its
// profile. Addressed by workspace, it ends that workspace's own assignment
// and leaves its HEAD, index, draft and commits untouched, so the result
// stays recoverable for keep or discard. Addressed by profile path and
// allocation from the integration checkout, it ends a lost workspace's
// assignment once the developer confirms that exact one is abandoned, without
// touching that checkout's files, index or HEAD. Each attempt rereads trunk
// first, so a repeated or delayed abandonment never ends a later allocation
// of the same name.
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { agentIdentity } from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { maintenance } from "../../dough-execute-plan/scripts/execution-start-maintenance.mjs";
import {
  exec,
  git,
  revParse,
  tryPushExactRef,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import {
  creditDeveloper,
  DeveloperIdentityRefused,
} from "../../dough-execute-plan/scripts/workspace-agent-authorship.mjs";
import { remoteRef } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import { addressedAssignment } from "./preparation-assignment-lost-workspace.mjs";
import {
  alreadyReleased,
  assignmentFields,
  errorText,
  noAssignment,
  stop,
  workspaceAssignment,
} from "./preparation-assignment-ownership.mjs";
import { requestOf } from "./preparation-assignment-request.mjs";

// A commit on `tip` whose only change removes `own`'s profile, built in a
// scratch index so the checkout's own index and files stay as they are. The
// agent authors it and the developer committing in `cwd` is credited; an
// unusable developer throws DeveloperIdentityRefused before anything is made.
async function endingCommit(cwd, tip, own) {
  const { identity } = own.profile;
  const { agent, email } = agentIdentity(own.name);
  const message = await creditDeveloper(
    cwd,
    `End preparation: ${identity}\n\nPreparation-Identity: ${identity}\n`,
    { agent, email },
  );
  const scratch = mkdtempSync(join(tmpdir(), "dough-abandon-"));
  const env = {
    ...process.env,
    GIT_INDEX_FILE: join(scratch, "index"),
    GIT_AUTHOR_NAME: agent,
    GIT_AUTHOR_EMAIL: email,
  };
  const run = async (...args) =>
    (await exec("git", args, { cwd, env })).stdout.trim();
  try {
    await run("read-tree", tip);
    await run("update-index", "--force-remove", "--", own.path);
    const tree = await run("write-tree");
    return await run("commit-tree", tree, "-p", tip, "-m", message);
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
}

// Up to two publication attempts; each is preceded by a fresh read of trunk,
// which also settles whether a push with a lost response was accepted.
const attempts = 2;

// Publishes the end of the assignment `locate(ref)` finds held on fetched
// trunk, reading and pushing from the checkout `cwd`. `place` is what every
// receipt says about where the request ran.
async function publishEnding(request, cwd, place, locate) {
  const { remote, target } = request;
  const ref = remoteRef(request);
  let candidate;
  for (let pass = 0; ; pass += 1) {
    let found;
    try {
      await git(cwd, "fetch", "--quiet", remote);
      found = await locate(ref);
    } catch (error) {
      if (candidate === undefined)
        return stop("source-refused", { ...place, error: errorText(error) });
      return stop("unconfirmed", {
        ...place,
        candidateSha: candidate,
        error: `whether trunk accepted the end of the assignment is unknown; rerun abandon: ${errorText(error)}`,
      });
    }
    if (found.state === "ended") {
      if (candidate === undefined || found.endedBy !== candidate)
        return alreadyReleased(request, found);
      return {
        ok: true,
        status: "abandoned",
        ...assignmentFields(found.own.profile, found.own),
        publishedSha: candidate,
        ...place,
        refresh: await maintenance(request),
      };
    }
    if (found.state !== "held") return found.receipt;
    if (pass === attempts)
      return stop("unpublished", {
        ...assignmentFields(found.own.profile, found.own),
        ...place,
        candidateSha: candidate,
        error:
          "remote trunk did not accept the end of the assignment; it is still published",
      });
    const tip = await revParse(cwd, ref);
    try {
      candidate = await endingCommit(cwd, tip, found.own);
    } catch (error) {
      if (!(error instanceof DeveloperIdentityRefused)) throw error;
      return stop("developer-identity-refused", {
        ...assignmentFields(found.own.profile, found.own),
        ...place,
        error: error.message,
      });
    }
    try {
      await tryPushExactRef(cwd, candidate, remote, `refs/heads/${target}`);
    } catch {
      // The response is lost or refused: the next read of trunk decides.
    }
  }
}

export async function abandonPreparation(input) {
  const requested = requestOf("abandon", input);
  if (!requested.ok) return requested;
  const { request } = requested;
  if (request.profile !== undefined)
    return publishEnding(request, request.integration, {}, (ref) =>
      addressedAssignment(request, ref),
    );
  const { workspace } = request;
  return publishEnding(request, workspace, { workspace }, async (ref) => {
    const found = await workspaceAssignment(request, ref);
    if (found.state === "held" || found.state === "ended") return found;
    return {
      state: "stopped",
      receipt: noAssignment(request, ref, found, "published"),
    };
  });
}
