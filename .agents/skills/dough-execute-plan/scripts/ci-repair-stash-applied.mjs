// What a CI repair restore put back in the execution checkout, judged against
// the saved entry and Git's own report, so a conflict names its paths and says
// whether none, part, or all of the paused work returned.
import { gitOutputOrNull } from "./ci-repair-stash-git.mjs";
import { git } from "./publication-git.mjs";

export async function conflictPaths(checkout, output) {
  const unmerged = (
    (await gitOutputOrNull(
      checkout,
      "diff",
      "--name-only",
      "--diff-filter=U",
    )) ?? ""
  )
    .split("\n")
    .filter(Boolean);
  const overwritten = [
    ...output.matchAll(/would be overwritten[^\n]*\n((?:\t[^\n]*\n?)+)/g),
  ].flatMap((match) => match[1].split("\n"));
  const reported = [
    ...output.matchAll(/Merge conflict in (.+)$/gm),
    // An index that cannot take the saved staging names only this line.
    ...output.matchAll(/^error: patch failed: (.+):\d+$/gm),
    ...output.matchAll(/^(.+) already exists, no checkout$/gm),
  ]
    .map((match) => match[1])
    .concat(overwritten)
    .map((path) => path.trim())
    .filter(Boolean);
  return [...new Set([...unmerged, ...reported])];
}

// Paths the record saved that the restored tree does not show in the same
// staged, unstaged, or untracked state.
export function unrestored(record, current) {
  return ["staged", "unstaged", "untracked"].flatMap((kind) =>
    record[kind].filter((path) => !current[kind].includes(path)),
  );
}

// What an attempt put back, judged by content against the entry's own trees
// (worktree at the OID, index at ^2, untracked files at ^3), not by Git's exit
// code: a conflict can leave some paths applied and the saved index unstaged.
export async function appliedState({
  checkout,
  oid,
  staged,
  unstaged,
  untracked,
}) {
  const tracked = [...new Set([...staged, ...unstaged])];
  const differing = async (...args) => {
    if (tracked.length === 0) return [];
    const diff = ["diff", "--name-only", "-z", ...args, "--", ...tracked];
    const { stdout } = await git(checkout, "--literal-pathspecs", ...diff);
    return stdout.split("\0").filter(Boolean);
  };
  const notApplied = await differing(oid);
  for (const path of untracked) {
    const saved = await gitOutputOrNull(
      checkout,
      "rev-parse",
      `${oid}^3:${path}`,
    );
    const current = await gitOutputOrNull(checkout, "hash-object", "--", path);
    if (!saved || saved !== current) notApplied.push(path);
  }
  const stagedNotRestored = (await differing("--cached", `${oid}^2`)).filter(
    (path) => staged.includes(path),
  );
  const applied =
    notApplied.length + stagedNotRestored.length === 0
      ? "all"
      : notApplied.length === tracked.length + untracked.length
        ? "none"
        : "partial";
  return { applied, stagedNotRestored };
}
