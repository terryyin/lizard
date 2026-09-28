// Ending a preparation assignment with the kept result: stages removal of
// exactly the profile this workspace's own announcement added, beside the
// retained result, so the landing that publishes the result also ends it.
// Rerunning after a rejected or already accepted landing reports what trunk
// shows, and refuses to land a removal that would end a later allocation.
// A story that has left the queue on fetched trunk stops the keep before
// anything is staged: where the story went is the developer's to weigh.
import { existsSync } from "node:fs";
import { join } from "node:path";
import {
  queueHeading,
  takenHeading,
} from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { occupiedAssignments } from "../../dough-execute-plan/scripts/agent-assignments.mjs";
import {
  git,
  revParse,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import {
  backlogPath,
  remoteRef,
} from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import {
  alreadyReleased,
  assignmentFields,
  errorText,
  noAssignment,
  requestOf,
  stop,
  storyListAt,
  workspaceAssignment,
} from "./preparation-assignment-ownership.mjs";

// The developer's choices when the prepared story has left the queue; the
// command makes none of them.
const leftQueueChoices = [
  {
    choice: "abandon",
    effect:
      "end this preparation assignment with abandon; the draft stays in the workspace",
  },
  { choice: "discard", effect: "discard the retained draft" },
  {
    choice: "separate-work",
    effect:
      "take the draft's content up as separate work with the story's current owner",
  },
];

// The stop for keeping assignment `own` when its story is no longer queued on
// trunk `ref`, or undefined while it still is. Reports where the story is
// now: Taken, with the execution assignments naming it, or absent.
async function storyLeftQueue(request, ref, own) {
  const { workspace, identity } = request;
  const list = await storyListAt(workspace, ref, identity);
  if (list === queueHeading) return undefined;
  let story = { place: "absent" };
  let where = "no longer lists it";
  if (list === takenHeading) {
    const held = await occupiedAssignments(workspace, ref, backlogPath);
    const owners = held.filter(
      (each) => each.activity === "execution" && each.identity === identity,
    );
    story = { place: "taken", owners };
    const named = owners.map((each) => each.agent).join(", ");
    where = `lists it as Taken${named ? ` by ${named}` : ""}`;
  }
  return stop("story-left-queue", {
    ...assignmentFields(request, own),
    workspace,
    fetched: await revParse(workspace, ref),
    story,
    choices: leftQueueChoices,
    error: `${ref} ${where}, so ${identity} is no longer queued; nothing was staged and nothing will land. The developer or coordinator decides what happens to this preparation`,
  });
}

async function tracked(workspace, ...args) {
  try {
    await git(workspace, ...args);
    return true;
  } catch {
    return false;
  }
}

// Where the workspace holds `path`: its working tree, index, and HEAD.
async function presence(workspace, path) {
  return {
    worktree: existsSync(join(workspace, path)),
    index: await tracked(workspace, "ls-files", "--error-unmatch", "--", path),
    head: await tracked(workspace, "cat-file", "-e", `HEAD:${path}`),
  };
}

export async function releasePreparation(input) {
  const requested = requestOf("release", input);
  if (!requested.ok) return requested;
  const { request } = requested;
  const { workspace, remote } = request;
  const ref = remoteRef(request);
  try {
    await git(workspace, "fetch", "--quiet", remote);
  } catch (error) {
    return stop("source-refused", { workspace, error: errorText(error) });
  }
  const found = await workspaceAssignment(request, ref);
  if (found.state === "ended") {
    const held = await presence(workspace, found.own.path);
    if (found.successor && !(held.worktree && held.index && held.head))
      return stop("release-conflict", {
        ...assignmentFields(request, found.own),
        successor: found.successor,
        workspace,
        error: `${ref} now holds a later allocation (${found.successor}) of ${found.own.path}; landing this workspace's removal of it would end that assignment. Take the removal out of the workspace's unpublished changes and commits before landing`,
      });
    return alreadyReleased(request, found);
  }
  if (found.state !== "held")
    return noAssignment(request, ref, found, "staged");
  const left = await storyLeftQueue(request, ref, found.own);
  if (left) return left;
  const { path } = found.own;
  const held = await presence(workspace, path);
  let staged;
  if (held.index) {
    await git(workspace, "rm", "--quiet", "--", path);
    staged = "staged";
  } else if (held.worktree) {
    return stop("release-conflict", {
      workspace,
      error: `${path} is untracked yet present; inspect it before landing`,
    });
  } else {
    // An earlier release staged the removal, or a landing already committed it.
    staged = held.head ? "already-staged" : "already-committed";
  }
  return {
    ok: true,
    status: "release-staged",
    staged,
    ...assignmentFields(request, found.own),
    workspace,
  };
}
