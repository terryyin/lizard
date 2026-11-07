// The carry step of `start --admit --carry`: a grown one-shot attempt's
// uncommitted edits (tracked, untracked and deleted files) are parked as a
// commit object under a ref named for the owned branch, the workspace returns
// to clean fetched trunk for the ordinary admission claim, and once the claim
// is accepted the edits are restored over it, uncommitted, and the ref is
// deleted. The ref is the durable record between those steps: an interrupted
// start resumes with it, and a restore that conflicts with the claim keeps it
// for a human decision. The shared stash is never used.
import { existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { exec, git, revParse } from "./publication-git.mjs";
import { isAncestor, stopped } from "./workspace-publication-ownership.mjs";

export const carriedRef = (branch) => `refs/dough/carried/${branch}`;

async function refSha(workspace, ref) {
  try {
    return (
      await git(workspace, "rev-parse", "--verify", "-q", ref)
    ).stdout.trim();
  } catch {
    return undefined;
  }
}

const pending = async (workspace) =>
  (await git(workspace, "status", "--porcelain")).stdout !== "";

// Runs `use(run, directory)` with Git bound to a temporary index, so the
// workspace's own index stays as it is.
async function withTemporaryIndex(workspace, use) {
  const directory = mkdtempSync(join(tmpdir(), "dough-carry-"));
  const env = { ...process.env, GIT_INDEX_FILE: join(directory, "index") };
  try {
    return await use(
      (...args) => exec("git", args, { cwd: workspace, env }),
      directory,
    );
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
}

// The tree the workspace's files would commit to over HEAD.
const worktreeTree = (workspace) =>
  withTemporaryIndex(workspace, async (run) => {
    await run("read-tree", "HEAD");
    await run("add", "-A");
    return (await run("write-tree")).stdout.trim();
  });

// The parked edits applied over HEAD: `{tree}`, or `{paths}` where they no
// longer apply. Plain patch application keeps this to long-available Git.
const restoredTree = (workspace, parked) =>
  withTemporaryIndex(workspace, async (run, directory) => {
    const patch = join(directory, "carried.patch");
    await run("diff", "--binary", `--output=${patch}`, `${parked}^`, parked);
    await run("read-tree", "HEAD");
    try {
      await run("apply", "--cached", patch);
    } catch (error) {
      const paths = new Set();
      for (const line of (error.stderr ?? "").split("\n")) {
        const path =
          line.match(/^error: patch failed: (.+):\d+$/)?.[1] ??
          line.match(/^error: (.+?): /)?.[1];
        if (path) paths.add(path);
      }
      return { paths: [...paths].sort() };
    }
    return { tree: (await run("write-tree")).stdout.trim() };
  });

// Whether each path the workspace changed still holds either the parked
// version or the version it is being reset from or to: a park interrupted
// after its ref was written, before or during the reset. Anything else is an
// edit made since, which parking again would lose.
async function interruptedPark(workspace, parked, trunkRef) {
  const tree = await worktreeTree(workspace);
  const differing = async (rev) =>
    new Set(
      (await git(workspace, "diff", "--name-only", tree, rev)).stdout
        .split("\n")
        .filter(Boolean),
    );
  const [fromParked, fromStart, fromTrunk] = await Promise.all(
    [parked, `${parked}^`, trunkRef].map(differing),
  );
  return [...fromParked].every(
    (path) => !fromStart.has(path) || !fromTrunk.has(path),
  );
}

// Parks the workspace's edits (once) and returns it to clean fetched trunk
// `trunkRef`. A resumed start already holds its claim candidate, so it
// changes nothing. Only uncommitted edits are carried: a commit that fetched
// trunk lacks, such as a composed queued completion, is refused.
export async function parkCarriedEdits(request, trunkRef) {
  if (request.retained) return { ok: true };
  const { workspace, branch } = request;
  const ref = carriedRef(branch);
  const refuse = (error) =>
    stopped("setup-failed", { workspace, branch, error });
  if (!existsSync(workspace))
    return refuse(
      "--carry needs the existing workspace whose edits it carries",
    );
  const current = (
    await git(workspace, "branch", "--show-current")
  ).stdout.trim();
  if (current !== branch)
    return refuse(
      `workspace is on ${current || "a detached HEAD"}, not ${branch}`,
    );
  if (!(await isAncestor(workspace, "HEAD", trunkRef)))
    return refuse(
      "workspace has commits fetched trunk lacks; only uncommitted edits are carried",
    );
  const parked = await refSha(workspace, ref);
  if (await pending(workspace)) {
    if (!parked) {
      const tree = await worktreeTree(workspace);
      const head = await revParse(workspace, "HEAD");
      const message = `Carried one-shot edits on ${branch}`;
      const sha = (
        await git(workspace, "commit-tree", tree, "-p", head, "-m", message)
      ).stdout.trim();
      await git(workspace, "update-ref", ref, sha, "");
    } else if (!(await interruptedPark(workspace, parked, trunkRef)))
      return refuse(`workspace edits differ from those carried in ${ref}`);
  }
  await git(workspace, "reset", "-q", "--hard", trunkRef);
  await git(workspace, "clean", "-fdq");
  return { ok: true };
}

// Restores the parked edits over the workspace's HEAD, the accepted claim, as
// uncommitted changes, then deletes the ref. A workspace already holding
// exactly that restoration only loses the ref. The stop `carry-conflict`
// leaves the workspace and the ref as they were.
async function restore(result, { workspace, branch }, ref, parked) {
  const kept = (error, fields = {}) => ({
    ...result,
    ...stopped("carry-conflict", {
      workspace,
      branch,
      carried: { ref, restored: false },
      ...fields,
      error,
    }),
  });
  const { tree, paths } = await restoredTree(workspace, parked);
  if (!tree)
    return kept(`carried edits conflict with the claim; they stay in ${ref}`, {
      paths,
    });
  if (await pending(workspace)) {
    if ((await worktreeTree(workspace)) !== tree)
      return kept(`workspace holds edits other than those in ${ref}`);
  } else {
    await git(workspace, "read-tree", "-m", "-u", "HEAD", tree);
    await git(workspace, "reset", "-q");
  }
  await git(workspace, "update-ref", "-d", ref, parked);
  return { ...result, carried: { restored: true } };
}

// The start's result with its carry reported: an accepted claim restores
// parked edits; any other result keeps them and names the ref.
export async function finishCarry(request, result) {
  const ref = carriedRef(request.branch);
  const parked = existsSync(request.workspace)
    ? await refSha(request.workspace, ref)
    : undefined;
  if (!parked)
    return result.ok ? { ...result, carried: { restored: false } } : result;
  if (!result.ok) return { ...result, carried: { ref, restored: false } };
  try {
    return await restore(result, request, ref, parked);
  } catch (error) {
    return {
      ...result,
      ...stopped("carry-conflict", {
        carried: { ref, restored: false },
        error: error.stderr || error.message,
      }),
    };
  }
}
