#!/usr/bin/env node
// Interprets one stopped cherry-pick step for `product-backlog-git-cherry-pick.mjs`,
// split out for cohesion the way `product-backlog-git-rebase.mjs` keeps its
// own `interpretStop`/`describeReplay` inline (this adapter needed one more
// stop shape than rebase does, so the pair grew past sharing a file cleanly).
//
// Two of the three possible stops here have no rebase analogue, both
// confirmed empirically rather than assumed:
//
// - A picked merge commit with no `-m`/`--mainline` supplied: Git itself
//   refuses with a distinct message and a `128` exit (never `1`), which this
//   module surfaces as this tool's own refusal rather than inventing a
//   mainline policy on the caller's behalf.
// - Git may stop a step not for a textual conflict but because applying it
//   would produce no change at all — the destination already carries this
//   step's own net effect. Rebase's default backend silently drops such a
//   step; cherry-pick's default requires an explicit human `--skip` or
//   `git commit --allow-empty` first. This is not this tool's decision to
//   make either, so it is its own distinct, actionable status rather than
//   folded into "conflict" (nothing to edit or `git add`) or "blocked" (this
//   path's own step is exactly what stopped it).
import { cherryPickState } from "./product-backlog-git-operation-state.mjs";
import {
  blockedStopMessage,
  gitLine,
} from "./product-backlog-git-repository.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

function describePick(repoRoot, mainline) {
  const state = cherryPickState(repoRoot, mainline);
  if (!state) {
    return (
      `Git's own cherry-pick state could not be read; identify the picked ` +
      `commit, its parent, and the current branch directly with ` +
      `\`git status\` and \`git log\` before resolving.`
    );
  }
  const subject = gitLine(
    ["log", "-1", "--format=%s", state.pickedCommit],
    repoRoot,
  );
  return (
    `Picking ${state.pickedCommit} ("${subject}"), whose own parent is ` +
    `${state.pickedParent}, onto ${state.destination}.`
  );
}

function isEmptyStop(outcome) {
  return /previous cherry-pick is now empty/.test(outcome.stderr ?? "");
}

// The commit named is read back out of Git's own message, not matched
// against the caller's supplied revisions, so it names the real sha Git
// itself identified regardless of what form the caller supplied it in.
function requireMainline(outcome) {
  const match = /commit (\S+) is a merge but no -m option was given/.exec(
    outcome.stderr ?? "",
  );
  if (!match) {
    return;
  }
  throw new BacklogError(
    `${match[1]} is a merge commit; supply --mainline <n> naming which ` +
      `parent is the line to pick against (the same parent number Git's ` +
      `own \`-m\` expects). This tool does not choose a mainline for you.`,
  );
}

// The recoverable shape of a stop this gate did not itself decide: something
// other than this path is still unresolved (or, uniquely to cherry-pick,
// this path's own step is a no-op Git itself refuses to apply silently), so
// nothing about this path's own content is judged. Mirrors `interpretStop`
// in `product-backlog-git-rebase.mjs`, plus the "empty" case above.
export function interpretStop(repoRoot, file, mainline, outcome) {
  requireMainline(outcome);
  const unresolved = gitLine(["ls-files", "-u", "--", file], repoRoot);
  if (unresolved !== "") {
    return {
      status: "conflict",
      message:
        `${describePick(repoRoot, mainline)}\n${file} is left unresolved ` +
        `by this pick; the cherry-pick stays paused and Git stays mid-pick. ` +
        `Resolve it by hand, \`git add ${file}\`, then run \`continue\` for ` +
        `this same cherry-pick.`,
    };
  }
  if (isEmptyStop(outcome)) {
    return {
      status: "empty",
      message:
        `${describePick(repoRoot, mainline)}\nApplying it changes nothing: ` +
        `the destination already carries this commit's own net effect. This ` +
        `is not a content dispute this tool can resolve on your behalf — ` +
        `decide by hand whether to \`git cherry-pick --skip\` it or keep it ` +
        `as a recorded no-op with \`git commit --allow-empty\`, then run ` +
        `\`continue\` for this same cherry-pick.`,
    };
  }
  return {
    status: "blocked",
    message: blockedStopMessage("cherry-pick", file, outcome.stderr),
  };
}
