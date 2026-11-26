#!/usr/bin/env node
// Read the revisions and directory of a stopped rebase from Git's own state.
import { existsSync, readFileSync } from "node:fs";
import { basename, join } from "node:path";
import { gitLine, gitPath } from "./product-backlog-git-repository.mjs";

function readOperationStateFile(path) {
  try {
    const contents = readFileSync(path, "utf8").trim();
    return contents === "" ? undefined : contents;
  } catch {
    return undefined;
  }
}

// Which of Git's two rebase state directories, if either, is actually on
// disk right now. Modern Git defaults ordinary (non-`-i`) `git rebase` to the
// "merge" backend (`.git/rebase-merge`), verified directly against this
// project's installed Git rather than assumed; `.git/rebase-apply` is the
// older "apply" backend, still reachable through `git rebase --apply` or a
// project's own configuration. Both name the commit currently failing to
// apply, just under different filenames, so one caller-facing shape covers
// either.
function rebaseStateDirectory(repoRoot) {
  const merge = gitPath(repoRoot, "rebase-merge");
  if (existsSync(merge)) {
    return { directory: merge, replayedFile: "stopped-sha" };
  }
  const apply = gitPath(repoRoot, "rebase-apply");
  if (existsSync(apply)) {
    return { directory: apply, replayedFile: "original-commit" };
  }
  return undefined;
}

// Which rebase state directory ("rebase-merge" or "rebase-apply") is on disk,
// or `undefined` when no rebase is in progress. The directory, not
// `REBASE_HEAD`, marks a rebase in progress: Git can leave `REBASE_HEAD`
// behind after a conflicted rebase completes.
export function rebaseInProgress(repoRoot) {
  const state = rebaseStateDirectory(repoRoot);
  return state ? basename(state.directory) : undefined;
}

// The real revisions a stopped rebase is actually replaying, read from Git's
// own rebase state rather than trusted from the "ours"/"theirs" conflict
// labels the merge machinery reuses for rebase too. During a rebase those
// labels are reversed from what they mean during a merge: "ours" is the
// destination this step is replaying onto, and "theirs" is the commit being
// replayed off the branch actually being rebased — the opposite of whose
// side is whose. `destination` is this step's actual parent (current HEAD,
// which only equals `onto` for the first replayed commit; later commits
// replay onto the previous step's own new commit). `onto` is the rebase's
// overall destination.
// Returns `undefined` when no rebase is in progress.
export function rebaseState(repoRoot) {
  const state = rebaseStateDirectory(repoRoot);
  if (!state) {
    return undefined;
  }
  const replayedCommit = readOperationStateFile(
    join(state.directory, state.replayedFile),
  );
  if (!replayedCommit) {
    return undefined;
  }
  return {
    replayedCommit,
    replayedParent: gitLine(["rev-parse", `${replayedCommit}^`], repoRoot),
    destination: gitLine(["rev-parse", "HEAD"], repoRoot),
    onto: readOperationStateFile(join(state.directory, "onto")),
  };
}
