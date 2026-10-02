// Git mechanics for maintain-default-checkout.md "Refresh eligibility".
// Fast-forwards only a clean checkout that is strictly behind fetched trunk.
// Installed guidance is the agent's contract.
import { existsSync } from "node:fs";
import { isAbsolute, join } from "node:path";
import { rebaseInProgress } from "../../dough-product-backlog/scripts/product-backlog-git-operation-state.mjs";
import { git, inspectCheckout, revParse } from "./publication-git.mjs";

const IN_PROGRESS_REFS = ["MERGE_HEAD", "CHERRY_PICK_HEAD", "REVERT_HEAD"];

function decision(result, reason, state, remoteSha) {
  return { result, reason, remoteSha, ...state };
}

async function isAncestor(checkout, ancestor, descendant) {
  try {
    await git(checkout, "merge-base", "--is-ancestor", ancestor, descendant);
    return true;
  } catch {
    return false;
  }
}

// Whether `git rev-parse -q --verify ref` succeeds: the ref resolves, even
// to a missing object. `extra` options run in the same process; their output
// precedes the verified object name. Failures other than an unresolved ref
// are thrown.
async function verifies(checkout, ref, ...extra) {
  try {
    const { stdout } = await git(
      checkout,
      "rev-parse",
      ...extra,
      "-q",
      "--verify",
      ref,
    );
    return { verified: true, stdout };
  } catch (error) {
    if (error.code !== 1) throw error;
    return { verified: false, stdout: error.stdout };
  }
}

async function present(checkout, ref) {
  try {
    return (await verifies(checkout, ref)).verified;
  } catch {
    // Any other failure also leaves this ref absent.
    return false;
  }
}

function lockPath(checkout, printed) {
  const path = printed.trim();
  return isAbsolute(path) ? path : join(checkout, path);
}

// The index lock path and the first in-progress ref, read by one process
// when it can answer both; otherwise by separate reads.
async function lockAndFirstRef(checkout, ref) {
  try {
    const { verified, stdout } = await verifies(
      checkout,
      ref,
      "--git-path",
      "index.lock",
    );
    const lines = stdout.replace(/\n$/, "").split("\n");
    if (verified) lines.pop();
    return { lock: lockPath(checkout, lines.join("\n")), verified };
  } catch {
    const printed = (
      await git(checkout, "rev-parse", "--git-path", "index.lock")
    ).stdout;
    return {
      lock: lockPath(checkout, printed),
      verified: await present(checkout, ref),
    };
  }
}

// The Git operation in progress in a checkout, or null: an index lock (which
// takes precedence), an in-progress merge, cherry-pick, or revert ref, or a
// rebase state directory.
export async function ongoingOperation(checkout) {
  const [first, ...rest] = IN_PROGRESS_REFS;
  const { lock, verified } = await lockAndFirstRef(checkout, first);
  if (existsSync(lock)) return "index.lock";
  if (verified) return first;
  for (const ref of rest) {
    if (await present(checkout, ref)) return ref;
  }
  return rebaseInProgress(checkout) ?? null;
}

// HEAD, the fetched trunk, and the current branch after the refresh's fetch.
// One `rev-parse` answers all three whenever it prints two object names and
// a branch ref (or detached HEAD); anything else takes the separate reads,
// which keep their own results and failures.
async function fetchedState(checkout, remoteRef) {
  try {
    const [head, remoteSha, symbolic, ...extra] = (
      await git(
        checkout,
        "rev-parse",
        "HEAD",
        remoteRef,
        "--symbolic-full-name",
        "HEAD",
      )
    ).stdout
      .replace(/\n$/, "")
      .split("\n");
    const objectName = /^[0-9a-f]{40}([0-9a-f]{24})?$/;
    if (
      extra.length === 0 &&
      objectName.test(head) &&
      objectName.test(remoteSha) &&
      (symbolic === "HEAD" || symbolic?.startsWith("refs/heads/"))
    ) {
      const branch =
        symbolic === "HEAD" ? "" : symbolic.slice("refs/heads/".length);
      return {
        current: await inspectCheckout(checkout, head),
        remoteSha,
        branch,
      };
    }
  } catch {
    // The separate reads below report any failure.
  }
  const current = await inspectCheckout(checkout);
  const remoteSha = await revParse(checkout, remoteRef);
  const branch = (
    await git(checkout, "branch", "--show-current")
  ).stdout.trim();
  return { current, remoteSha, branch };
}

// A missing snapshot argument is intentional: callers cannot supply a stale
// one. With no default checkout supplied there is nothing to refresh. A
// supplied checkout that cannot be read or refreshed (a missing path, or Git
// failing there) reports `refresh-failed`; the caller's accepted publication
// stands.
export async function refreshDefaultCheckout(request) {
  if (!request.checkout) return { result: "not applicable" };
  try {
    return await attemptRefresh(request);
  } catch (error) {
    return {
      result: "deferred",
      reason: "refresh-failed",
      error: existsSync(request.checkout)
        ? error.stderr || error.message
        : `default checkout not found: ${request.checkout}`,
    };
  }
}

async function attemptRefresh({
  checkout,
  integrationBranch = "main",
  remote = "origin",
}) {
  // An ongoing operation stops refresh before the fetch.
  if (await ongoingOperation(checkout)) {
    const state = { head: await revParse(checkout, "HEAD"), status: null };
    return decision("deferred", "ongoing-operation", state, null);
  }

  await git(checkout, "fetch", remote);
  return fastForwardToFetchedTrunk(
    checkout,
    `${remote}/${integrationBranch}`,
    integrationBranch,
  );
}

// Refresh eligibility after a fetch, shared by refresh and by callers that
// reuse an existing workspace: a checkout on `expectedBranch` (when one is
// required) with no ongoing Git operation, no edits, and no commits of its
// own that is strictly behind `fetchedRef` is fast-forwarded to it. Anything
// else is left unchanged and reported with the reason it stopped.
export async function fastForwardToFetchedTrunk(
  checkout,
  fetchedRef,
  expectedBranch,
) {
  const { current, remoteSha, branch } = await fetchedState(
    checkout,
    fetchedRef,
  );

  if (await ongoingOperation(checkout)) {
    return decision("deferred", "ongoing-operation", current, remoteSha);
  }
  if (expectedBranch !== undefined && branch !== expectedBranch) {
    return decision("stopped", "unexpected-branch", current, remoteSha);
  }
  const behind = await isAncestor(checkout, current.head, fetchedRef);
  const ahead = await isAncestor(checkout, fetchedRef, current.head);
  if (!behind && !ahead) {
    return decision("stopped", "diverged", current, remoteSha);
  }
  if (current.status !== "") {
    return decision("deferred", "pending-edit", current, remoteSha);
  }
  if (current.head === remoteSha) {
    return decision("already current", null, current, remoteSha);
  }
  if (behind) {
    await git(checkout, "merge", "--ff-only", fetchedRef);
    const advanced = await inspectCheckout(checkout);
    if (advanced.head !== remoteSha || advanced.status !== "") {
      throw new Error(
        "fast-forward did not leave a clean checkout at fetched trunk",
      );
    }
    return decision("advanced", null, advanced, remoteSha);
  }
  return decision("deferred", "unpublished-commits", current, remoteSha);
}
