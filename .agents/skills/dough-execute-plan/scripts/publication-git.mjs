// Git repository operations shared by the installed publication commands.
import { existsSync } from "node:fs";
import { git } from "./publication-git-process.mjs";

export {
  exec,
  execWithInput,
  git,
  transportBoundMs,
  transportBoundSetting,
} from "./publication-git-process.mjs";

export async function revParse(cwd, ref) {
  return (await git(cwd, "rev-parse", ref)).stdout.trim();
}

// Verify a ref, even if its object is missing. Extra options can request
// output before the verified name. Only an unresolved ref returns false.
export async function verifyRef(checkout, ref, ...extra) {
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

// The full commit ID a hexadecimal object ID, full or uniquely abbreviated,
// names in `cwd` as `{ commit }`, or `{ problem }` when it names none:
// `not-an-object-id` (such as a ref name), `unknown`, `ambiguous` (more than
// one object), or `not-a-commit`.
export async function resolveCommitId(cwd, objectId) {
  if (!/^[0-9a-f]{4,64}$/i.test(objectId ?? ""))
    return { problem: "not-an-object-id" };
  const listed = await git(cwd, "rev-parse", `--disambiguate=${objectId}`);
  const matches = listed.stdout.split("\n").filter(Boolean);
  if (matches.length === 0) return { problem: "unknown" };
  if (matches.length > 1) return { problem: "ambiguous" };
  const type = (await git(cwd, "cat-file", "-t", matches[0])).stdout.trim();
  return type === "commit"
    ? { commit: matches[0] }
    : { problem: "not-a-commit" };
}

// The SHA `remote` holds at `ref`, read live; a remote name resolves in `cwd`.
export async function lsRemoteSha(remote, ref, cwd) {
  const { stdout } = await git(cwd, "ls-remote", remote, ref);
  // A qualified ref can still match the tail of another ref in ls-remote.
  if (ref.startsWith("refs/")) {
    for (const row of stdout.split("\n")) {
      const [sha, name] = row.split("\t");
      if (name === ref) return sha;
    }
    return "";
  }
  return stdout.trim().split(/\s+/)[0];
}

// Branch name from an authorized heads ref (refs/heads/NAME → NAME).
export function targetBranchName(targetRef) {
  if (!targetRef.startsWith("refs/heads/")) {
    throw new Error(`authorized target must be a branch ref: ${targetRef}`);
  }
  return targetRef.slice("refs/heads/".length);
}

// Authorized publication target as the workspace's remote-tracking ref.
export function originTrackingRef(targetRef, remote = "origin") {
  return `${remote}/${targetBranchName(targetRef)}`;
}

// The repository management context: its shared Git directory, read from a
// worktree that still exists. Retirement runs from there, so removing the
// repository's last worktree leaves fetch, containment, and branch deletion
// usable without a default checkout.
export async function managementContext(worktree) {
  const args = ["rev-parse", "--path-format=absolute", "--git-common-dir"];
  return (await git(worktree, ...args)).stdout.trim();
}

// The retained management context when supplied, otherwise the one recorded
// from the worktree; null when neither is available.
export async function resolveManagementContext(repository, worktree) {
  if (typeof repository === "string" && repository !== "") {
    return repository;
  }
  if (worktree && existsSync(worktree)) {
    return managementContext(worktree);
  }
  return null;
}

export async function pushExactRef(workspace, sha, remote, targetRef) {
  await git(workspace, "push", remote, `${sha}:${targetRef}`);
}

// The checkout facts maintenance decisions use: HEAD and porcelain status.
// Full index and patch snapshots stay in test fixtures, which prove
// preservation independently of this production inspection. A caller that
// already read HEAD passes it as `head`.
export async function inspectCheckout(checkout, head, statusOptions = []) {
  return {
    head: head ?? (await revParse(checkout, "HEAD")),
    status: (await git(checkout, "status", "--porcelain", ...statusOptions))
      .stdout,
  };
}

// Read the authorized remote-tracking tip, or null when it is absent.
export async function fetchedTarget(workspace, targetRef, remote = "origin") {
  return readTrackingTip(workspace, originTrackingRef(targetRef, remote));
}

// Fetch and pin the observed tip of an authorized tracking ref for a decision.
export async function observePublicationTarget(workspace, remote, tracking) {
  await git(workspace, "fetch", remote);
  return readTrackingTip(workspace, tracking);
}

export async function readTrackingTip(workspace, tracking) {
  try {
    return await revParse(workspace, tracking);
  } catch (error) {
    const text = `${error.stderr ?? ""}\n${error.message ?? ""}`;
    if (
      error.code === 128 ||
      /unknown revision|Needed a single revision|ambiguous argument/.test(text)
    ) {
      return null;
    }
    throw error;
  }
}

// Whether any fetched ref of `remote` already contains `sha`. A revision the
// workspace cannot resolve is not held there either.
export async function remoteHolds(workspace, sha, remote = "origin") {
  try {
    const { stdout } = await git(
      workspace,
      "for-each-ref",
      "--count=1",
      "--format=%(refname)",
      "--contains",
      sha,
      `refs/remotes/${remote}/`,
    );
    return stdout.trim() !== "";
  } catch {
    return false;
  }
}

function isNonFastForward(error) {
  const text = `${error.message ?? ""}\n${error.stderr ?? ""}`;
  return /rejected|non-fast-forward|fetch first/i.test(text);
}

// Push one exact candidate; non-fast-forward rejection is returned, not thrown.
export async function tryPushExactRef(workspace, candidate, remote, targetRef) {
  try {
    await pushExactRef(workspace, candidate, remote, targetRef);
    return { rejected: false };
  } catch (error) {
    if (!isNonFastForward(error)) {
      throw error;
    }
    return { rejected: true, error };
  }
}

// Inspection-only maintenance result for a default checkout after remote
// acceptance. Does not refresh. A clean checkout already at the accepted tip
// is already current; any other state is deferred.
export function maintenanceFromInspection(checkoutState, remoteSha) {
  if (checkoutState.head === remoteSha && checkoutState.status === "") {
    return "already current";
  }
  return "deferred";
}

// Without a supplied default checkout there is nothing to inspect. A supplied
// checkout that cannot be read is deferred; the acceptance stands.
// Reuse the authorized tip when the publisher already observed it.
export async function inspectDefaultCheckoutMaintenance(
  workspace,
  defaultCheckout,
  remote,
  targetRef,
  observedTip = undefined,
) {
  if (!defaultCheckout) return "not applicable";
  const remoteTip =
    observedTip ??
    (await revParse(workspace, originTrackingRef(targetRef, remote)));
  try {
    return maintenanceFromInspection(
      await inspectCheckout(defaultCheckout),
      remoteTip,
    );
  } catch {
    return "deferred";
  }
}
