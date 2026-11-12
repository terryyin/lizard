// Shared Git change discovery for consumer tracing and implicated refactoring.
// Renames stay as deletion/addition so both paths participate in scope.
import { readFileSync, statSync } from "node:fs";
import { resolve } from "node:path";

const DIFF = ["-c", "core.quotePath=false", "diff", "--no-renames"];
const paths = (output) => output.split("\0").filter(Boolean);

function diffNames(git, root, args) {
  return paths(git(root, [...DIFF, "--name-only", "-z", ...args, "--"]));
}

function untrackedPaths(git, root) {
  return paths(git(root, ["ls-files", "-z", "--others", "--exclude-standard"]));
}

function workingLayers(git, root, base) {
  const revision = base === undefined ? [] : [base];
  return {
    worktree: diffNames(git, root, revision),
    staged: diffNames(git, root, ["--cached", ...revision]),
    untracked: untrackedPaths(git, root),
  };
}

// Preserve the existing range-oriented consumer-trace/MakeMe API. A supplied
// range is compared by Git, rather than interpreted as one commit's patch.
export function repositoryChangePaths(git, root, base, committed = []) {
  return {
    ...workingLayers(git, root, base),
    committed: [
      ...new Set(committed.flatMap((range) => diffNames(git, root, [range]))),
    ],
  };
}

export function currentSourcePaths(project, paths) {
  return paths.filter((path) => {
    try {
      return statSync(resolve(project, path)).isFile();
    } catch (error) {
      if (error.code === "ENOENT" || error.code === "ENOTDIR") return false;
      throw error;
    }
  });
}

// Include index and worktree independently: an unstaged reversal of a staged
// edit still implicates that source. Only supplied commits add committed scope.
export function changedForRefactoring(git, root, committed = []) {
  const changed = Object.values(workingLayers(git, root)).flat();
  for (const revision of committed) {
    if (revision.includes("..")) {
      if (revision.startsWith("-"))
        throw new Error("Supply a Git revision range, not options");
      changed.push(...diffNames(git, root, [revision]));
      continue;
    }
    const sha = git(root, [
      "rev-parse",
      "--verify",
      "--end-of-options",
      `${revision}^{commit}`,
    ]).trim();
    changed.push(
      ...paths(
        git(root, [
          "diff-tree",
          "--root",
          "--no-commit-id",
          "--name-only",
          "-z",
          "--no-renames",
          "-r",
          "-m",
          "--first-parent",
          sha,
          "--",
        ]),
      ),
    );
  }
  return [...new Set(changed)].sort();
}

function hunkLines(patch) {
  const lines = [];
  let removed = 0;
  for (const line of patch.split("\n")) {
    const header = line.match(/^@@ -(\d+)(?:,\d+)? \+\d+(?:,\d+)? @@/);
    if (header) removed = Number(header[1]);
    else if (line.startsWith("-") && !line.startsWith("--- "))
      lines.push({ side: "removed", line: removed++, text: line.slice(1) });
    else if (line.startsWith("+") && !line.startsWith("+++ "))
      lines.push({ side: "added", text: line.slice(1) });
  }
  return lines;
}

// The checkout's text at `path`, or undefined when it is unreadable or binary.
export function readText(root, path) {
  let content;
  try {
    content = readFileSync(resolve(root, path));
  } catch {
    return undefined;
  }
  return content.subarray(0, 8000).includes(0)
    ? undefined
    : content.toString("utf8");
}

// Each patch section is assigned by its own `a/<path> b/<path>` header, since
// a type change (a file replaced by a symlink) yields two sections for one
// name; a header Git had to quote derives no lines.
function patchLines(names, patch) {
  const byHeader = new Map(names.map((path) => [`a/${path} b/${path}`, path]));
  const lines = new Map(names.map((path) => [path, []]));
  for (const section of patch.split(/^diff --git /m).slice(1)) {
    const path = byHeader.get(section.slice(0, section.indexOf("\n")));
    if (path) lines.get(path).push(...hunkLines(section));
  }
  return lines;
}

// Every file the working tree changes against `base`, staged or not, with
// its changed lines; untracked non-ignored files count as added whole.
export function changedAgainst(git, root, base) {
  const names = diffNames(git, root, [base]);
  const lines = patchLines(
    names,
    git(root, [...DIFF, "--no-ext-diff", "--no-color", "-U0", base]),
  );
  const changes = names.map((path) => ({ path, lines: lines.get(path) }));
  const untracked = untrackedPaths(git, root);
  for (const path of untracked) {
    const text = readText(root, path) ?? "";
    const added = text
      .split(/\r?\n/)
      .map((line) => ({ side: "added", text: line }));
    changes.push({ path, lines: added });
  }
  return changes;
}
