// Git ancestry of a candidate snapshot C over accepted revision B, judged in
// this checkout for integrated-state evidence (ci-integrated-state.mjs).
import { execFileSync } from "node:child_process";
import { git } from "./publication-git.mjs";

export const ancestryBasis = "git merge-base --is-ancestor";

// Candidate fetches per poll.
const fetchBound = 3;

// "ancestor", "not_ancestor", or "unknown" when the local repository cannot
// answer (for example, the descendant was never fetched).
function gitAncestry(repoDir, ancestor, descendant) {
  try {
    execFileSync(
      "git",
      ["-C", repoDir, "merge-base", "--is-ancestor", ancestor, descendant],
      { stdio: "ignore" },
    );
    return "ancestor";
  } catch (error) {
    return error.status === 1 ? "not_ancestor" : "unknown";
  }
}

const localGit = (repoDir, args) =>
  execFileSync("git", ["-C", repoDir, ...args], {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "ignore"],
  }).trim();

// The remote this checkout observes `repo`'s target through: the request's
// own, the only remote, the target branch's configured remote, or the one
// whose URL names `repo`. Undefined when none is identified.
function observedRemote(repoDir, { remote, repo, branch }) {
  if (remote) return remote;
  let remotes;
  try {
    remotes = localGit(repoDir, ["remote"]).split("\n").filter(Boolean);
  } catch {
    return undefined;
  }
  if (remotes.length === 1) return remotes[0];
  try {
    const tracked = localGit(repoDir, ["config", `branch.${branch}.remote`]);
    if (remotes.includes(tracked)) return tracked;
  } catch {
    // No configured remote for the target branch.
  }
  const escaped = String(repo).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const named = new RegExp(`[/:]${escaped}(\\.git)?/?$`);
  return remotes.find((name) => {
    try {
      return named.test(localGit(repoDir, ["remote", "get-url", name]));
    } catch {
      return false;
    }
  });
}

// Fetches only `sha`'s objects from the observed remote, within the bounded
// transport: no ref, FETCH_HEAD, index or worktree changes. False when no
// remote is identified or the fetch fails. The end of observation stops the
// fetch's process group and rethrows.
async function fetchCandidate(repoDir, sha, target, signal) {
  const remote = observedRemote(repoDir, target);
  if (!remote) return false;
  let fetched = true;
  try {
    await git(
      repoDir,
      "fetch",
      "--no-tags",
      "--no-write-fetch-head",
      "--quiet",
      remote,
      sha,
      { signal },
    );
  } catch {
    fetched = false;
  }
  if (signal?.aborted) throw signal.reason;
  return fetched;
}

// One poll's ancestry reader over `repoDir`. With `fetch`, a candidate this
// checkout lacks is fetched once per poll, within the bound; one the remote
// cannot serve stays unknown.
export function candidateAncestry({ repoDir, remote, repo, branch, signal }) {
  const fetches = new Map();
  return async (acceptedSha, sha, { fetch }) => {
    const ancestry = gitAncestry(repoDir, acceptedSha, sha);
    if (ancestry !== "unknown" || !fetch) return ancestry;
    if (!fetches.has(sha)) {
      if (fetches.size >= fetchBound) return ancestry;
      fetches.set(
        sha,
        fetchCandidate(repoDir, sha, { remote, repo, branch }, signal),
      );
    }
    return (await fetches.get(sha))
      ? gitAncestry(repoDir, acceptedSha, sha)
      : ancestry;
  };
}
