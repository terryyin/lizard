// The identity-evidence history source: the seed and done-record files the
// Git history reachable from HEAD shows added, which still shows seeds and
// records cleanup has since removed, each with the commit that first added
// it. It reports in the shape of the other sources
// (`product-backlog-identity-sources.mjs`) and reads Git only through the
// shared repository primitives.

import { realpathSync } from "node:fs";
import { join, posix, relative } from "node:path";
import {
  doneRecordDirectory,
  doneRecordIdentity,
  isDoneRecordFileName,
} from "./product-backlog-done-record.mjs";
import {
  collectingGitDiagnostics,
  git,
  gitLine,
  gitOutcome,
  repositoryRoot,
} from "./product-backlog-git-repository.mjs";
import {
  posixPath,
  seedNumberOf,
  unread,
} from "./product-backlog-identity-sources.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

// A seed file's ID read from its name, the leading text through its first
// run of digits ("SEED-090-x.md" names SEED-090), since history may no longer
// hold the file's front matter.
const seedFileNamePattern = /^(?<seed>\D*\d+)/;

// Each file the history reachable from `revision` shows added directly under
// one of `directories` (repository-relative), with the earliest commit that
// added it: { path, commit, directory }.
function addedFiles(root, revision, directories) {
  const marker = "\u0001";
  const output = git(
    [
      "-c",
      "core.quotePath=false",
      "log",
      "--no-renames",
      "--diff-filter=A",
      "--name-only",
      `--format=${marker}%H`,
      revision,
      "--",
      ...directories,
    ],
    root,
  );
  const added = new Map();
  let commit;
  for (const line of output.split("\n")) {
    if (line.startsWith(marker)) {
      commit = line.slice(marker.length);
      continue;
    }
    if (line === "") continue;
    const directory = directories.find((each) => posix.dirname(line) === each);
    // Newest first: the last commit seen for a path is the one that first
    // added it.
    if (directory !== undefined)
      added.set(line, { path: line, commit, directory });
  }
  // In the order the directories are named, then by path: a seed file wins a
  // tie with a done record of the same number.
  return [...added.values()].sort(
    (a, b) =>
      directories.indexOf(a.directory) - directories.indexOf(b.directory) ||
      a.path.localeCompare(b.path),
  );
}

// Where `directory` (inside or beside the backlog) sits in the repository at
// `root`, read through the backlog directory's real path as Git reports it.
function repositoryPath(root, backlogReal, backlogDirectory, directory) {
  const real = join(backlogReal, relative(backlogDirectory, directory));
  return posixPath(relative(root, real)) || ".";
}

function readHistory(backlogDirectory, seedDirectory) {
  let root;
  try {
    root = repositoryRoot(backlogDirectory);
  } catch (error) {
    if (!(error instanceof BacklogError)) throw error;
    return unread("history", ".", "not a Git work tree");
  }
  const backlogReal = realpathSync(backlogDirectory);
  const path = posixPath(relative(backlogReal, root)) || ".";
  if (gitLine(["rev-parse", "--is-shallow-repository"], root) === "true")
    return unread(
      "history",
      path,
      "shallow history: commits beyond its boundary are unread",
    );
  const head = gitOutcome(
    ["rev-parse", "--verify", "--quiet", "HEAD^{commit}"],
    root,
  );
  const revision = head.code === 0 ? head.stdout.trim() : null;
  const source = { source: "history", path, revision, read: true };
  if (revision === null) return { ...source, named: [], gaps: [] };
  const seeds = repositoryPath(
    root,
    backlogReal,
    backlogDirectory,
    seedDirectory,
  );
  const done = repositoryPath(
    root,
    backlogReal,
    backlogDirectory,
    join(backlogDirectory, doneRecordDirectory),
  );
  const named = [];
  const gaps = [];
  for (const { path: file, commit, directory } of addedFiles(root, revision, [
    seeds,
    done,
  ])) {
    const evidence = posixPath(relative(backlogReal, join(root, file)));
    const fileName = posix.basename(file);
    if (directory === seeds) {
      if (!fileName.endsWith(".md")) continue;
      const seed = seedFileNamePattern.exec(fileName)?.groups.seed;
      if (seed === undefined || seedNumberOf(seed) === undefined) {
        gaps.push({
          path: evidence,
          commit,
          reason: "file name names no seed ID",
        });
      } else {
        named.push({ identity: seed, evidence, commit, kind: "seed" });
      }
    } else if (isDoneRecordFileName(fileName)) {
      const identity = doneRecordIdentity(fileName);
      if (identity === undefined) {
        gaps.push({
          path: evidence,
          commit,
          reason: "file name names no done record identity",
        });
      } else {
        named.push({ identity, evidence, commit, kind: "done-record" });
      }
    }
  }
  return { ...source, named, gaps };
}

// The history source, reading Git only through the shared repository
// primitives; what Git writes to stderr is kept from this report's caller,
// and a Git command that fails leaves history unread with Git's reason.
export async function historySource(backlogDirectory, seedDirectory) {
  const run = await collectingGitDiagnostics(() =>
    readHistory(backlogDirectory, seedDirectory),
  );
  if (!run.threw) return run.result;
  if (typeof run.error?.status !== "number") throw run.error;
  const reason = (run.error.stderr || run.stderr).trim().split("\n")[0];
  return unread("history", ".", reason || "Git could not read history");
}

// What the history shows of `identity`: the done record its own file name
// held (`record`), and the first file of the same seed ID in the history's
// order, a seed file before a done record (`seed`). Undefined when history
// shows neither.
export function historyFact(identity, history) {
  const seed = seedNumberOf(identity)?.seed;
  const record = history.named.find(
    (entry) => entry.kind === "done-record" && entry.identity === identity,
  );
  const sameSeed =
    seed === undefined
      ? undefined
      : history.named.find(
          (entry) => seedNumberOf(entry.identity)?.seed === seed,
        );
  const fact = {
    ...(record === undefined
      ? {}
      : { record: { evidence: record.evidence, commit: record.commit } }),
    ...(sameSeed === undefined
      ? {}
      : {
          seed: {
            seed,
            evidence: sameSeed.evidence,
            commit: sameSeed.commit,
          },
        }),
  };
  return Object.keys(fact).length === 0 ? undefined : fact;
}
