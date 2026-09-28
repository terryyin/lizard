#!/usr/bin/env node
// Reads the state Git itself keeps for a rebase or cherry-pick stopped in
// progress in one checkout: which real revisions the stopped step is
// replaying or picking, from Git's own state files rather than from the
// "ours"/"theirs" conflict labels. `gitPath` and `gitLine` from
// `product-backlog-git-repository.mjs` locate those files and resolve the
// revisions they name.
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { gitLine, gitPath } from "./product-backlog-git-repository.mjs";

// Reads one small state file Git itself maintains for an in-progress
// operation (inside a rebase's or the sequencer's own directory, or
// cherry-pick's top-level marker), trimmed, or `undefined` when absent or
// empty — the same "nothing here means no operation in progress" shape every
// caller below shares.
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

// The real revisions a stopped rebase is actually replaying, read from Git's
// own rebase state rather than trusted from the "ours"/"theirs" conflict
// labels the merge machinery reuses for rebase too. During a rebase those
// labels are reversed from what they mean during a merge: "ours" is the
// destination this step is replaying onto, and "theirs" is the commit being
// replayed off the branch actually being rebased — the opposite of whose
// side is whose. `destination` is this step's actual parent (current HEAD,
// which only equals `onto` for the first replayed commit; later commits
// replay onto the previous step's own new commit). `onto` and `origHead` are
// the rebase's overall destination and the branch's pre-rebase tip, which
// stays reachable there for as long as the rebase remains unfinished or is
// abandoned, so a stopped rebase never puts the unpublished suffix at risk.
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
    origHead: readOperationStateFile(join(state.directory, "orig-head")),
    headName: readOperationStateFile(join(state.directory, "head-name")),
  };
}

// The commit a stopped `todo` line names, however Git chose to abbreviate it
// ("pick <sha> <subject>"), read only when `CHERRY_PICK_HEAD` itself is
// absent — confirmed empirically to happen for one real, in-between state: a
// human resolved Git's own "this step is now empty" stop by committing it
// directly (`git commit --allow-empty`) rather than through this tool.
// That commit clears `CHERRY_PICK_HEAD` immediately, before the sequencer has
// advanced its own `todo` past that same line, so a cherry-pick genuinely
// still in progress (confirmed by `git cherry-pick --continue` still working
// correctly there) would otherwise be misreported as finished.
function pendingPickedCommit(sequencerDirectory) {
  const todo = readOperationStateFile(join(sequencerDirectory, "todo"));
  const line = todo?.split("\n").find((entry) => entry.startsWith("pick "));
  return line?.split(" ")[1];
}

// The real revisions a stopped cherry-pick is actually applying, read from
// Git's own state under `.git/sequencer/` and the top-level
// `CHERRY_PICK_HEAD` marker — confirmed empirically to live there, never
// under `.git/rebase-merge`/`.git/rebase-apply` the way a rebase's own state
// does. A cherry-pick (or a sequence still holding further commits) is
// genuinely in progress whenever either marker exists; `CHERRY_PICK_HEAD`
// alone is not a reliable "finished" signal (see `pendingPickedCommit`).
// Unlike a rebase, cherry-pick's index stages are never reversed: stage 2
// ("ours") is always the current branch (the destination), and stage 3
// ("theirs") is always the commit being picked, the same convention an
// ordinary merge uses. `mainline` is the same 1-based parent number a caller
// would pass to `-m` for picking a merge commit (defaulting to 1, the same
// default `git cherry-pick -m 1` would use, which is also correct for an
// ordinary, non-merge picked commit). Returns `undefined` when no cherry-pick
// is in progress.
export function cherryPickState(repoRoot, mainline = 1) {
  const sequencerDirectory = gitPath(repoRoot, "sequencer");
  const pickedCommit =
    readOperationStateFile(gitPath(repoRoot, "CHERRY_PICK_HEAD")) ??
    (existsSync(sequencerDirectory)
      ? pendingPickedCommit(sequencerDirectory)
      : undefined);
  if (!pickedCommit) {
    return undefined;
  }
  return {
    pickedCommit,
    pickedParent: gitLine(
      ["rev-parse", `${pickedCommit}^${mainline}`],
      repoRoot,
    ),
    destination: gitLine(["rev-parse", "HEAD"], repoRoot),
  };
}
