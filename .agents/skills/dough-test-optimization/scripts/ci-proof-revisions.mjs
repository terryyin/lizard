// What local Git says for a lead reading (./ci-proof-leads.mjs): the
// checkout's root, the requested revision, how an attempt's revision relates
// to it, the commits and in-scope paths between them, and the project's
// repository and default branch as its `origin` remote names them.
import { git } from "../../dough-execute-plan/scripts/publication-git.mjs";

const output = async (root, ...args) =>
  (await git(root, ...args)).stdout.trim();

/** @param {string} directory */
export const checkoutRoot = (directory) =>
  output(directory, "rev-parse", "--show-toplevel");

/**
 * SPEC's full commit name in ROOT, lower case; rejected when Git cannot
 * name a commit for it.
 * @param {string} root
 * @param {string} spec
 */
export async function resolvedRevision(root, spec) {
  const revision = await output(
    root,
    "rev-parse",
    "--verify",
    "--quiet",
    "--end-of-options",
    `${spec}^{commit}`,
  ).catch(() => "");
  if (!/^[0-9a-f]{40}$/.test(revision))
    throw new Error(`Git cannot name a commit for revision ${spec}`);
  return revision;
}

/**
 * How a revision relates to REQUESTED in ROOT: "requested", an "ancestor",
 * "other", or "unknown" when local Git does not have it. Each revision is
 * asked of Git once.
 * @param {string} root
 * @param {string} requested
 * @returns {(revision: string) => Promise<"requested" | "ancestor" | "other" | "unknown">}
 */
export function ancestryOf(root, requested) {
  const known = new Map();
  const judged = async (revision) => {
    if (revision === requested) return "requested";
    try {
      await git(root, "merge-base", "--is-ancestor", revision, requested);
      return "ancestor";
    } catch (error) {
      return error.code === 1 ? "other" : "unknown";
    }
  };
  return (revision) => {
    if (!known.has(revision)) known.set(revision, judged(revision));
    return known.get(revision);
  };
}

/**
 * How many commits REQUESTED has that ANCESTOR does not.
 * @param {string} root
 * @param {string} ancestor
 * @param {string} requested
 */
export async function commitsBetween(root, ancestor, requested) {
  return Number(
    await output(root, "rev-list", "--count", `${ancestor}..${requested}`),
  );
}

/**
 * The paths under PATHS that differ between FROM and TO, with Git's status
 * letter each, renames as a deletion and an addition.
 * @param {string} root
 * @param {string} from
 * @param {string} to
 * @param {readonly string[]} paths
 * @returns {Promise<{ status: string; path: string }[]>}
 */
export async function changedPaths(root, from, to, paths) {
  const { stdout } = await git(
    root,
    "diff",
    "--name-status",
    "--no-renames",
    "-z",
    from,
    to,
    "--",
    ...paths,
  );
  const fields = stdout.split("\0").filter((field) => field !== "");
  const changed = [];
  for (let index = 0; index + 1 < fields.length; index += 2)
    changed.push({ status: fields[index], path: fields[index + 1] });
  return changed;
}

/**
 * The `OWNER/REPO` the `origin` remote's URL names, when it is a hosted
 * URL; undefined otherwise.
 * @param {string} root
 */
export async function originRepository(root) {
  const url = await output(root, "remote", "get-url", "origin").catch(() => "");
  const hosted =
    /^(?:[a-z][a-z0-9+.-]*:\/\/[^/]+\/|[^/@:\s]+@[^/:\s]+:)(.+?)(?:\.git)?\/?$/i.exec(
      url,
    );
  const parts = hosted?.[1].split("/") ?? [];
  return parts.length >= 2 ? parts.slice(-2).join("/") : undefined;
}

/**
 * The branch `origin`'s HEAD names, else the checkout's current branch;
 * undefined when neither is known.
 * @param {string} root
 */
export async function defaultBranch(root) {
  const remote = await output(
    root,
    "symbolic-ref",
    "--quiet",
    "--short",
    "refs/remotes/origin/HEAD",
  ).catch(() => "");
  if (remote.startsWith("origin/")) return remote.slice("origin/".length);
  const current = await output(root, "branch", "--show-current").catch(
    () => "",
  );
  return current || undefined;
}
