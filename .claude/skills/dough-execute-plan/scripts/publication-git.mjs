// Git operations shared by the installed publication commands. Test fixtures
// import these mechanics, but production never imports fixture setup.
import { execFile, spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { promisify } from "node:util";

export const exec = promisify(execFile);

// Remote transport commands answer within a bound; local commands do not need
// one. The default suits ordinary networks; the setting raises or lowers it.
const transportSubcommands = new Set(["fetch", "push", "ls-remote"]);
export const transportBoundSetting = "OPEN_DOUGH_GIT_TRANSPORT_BOUND_MS";
const defaultTransportBoundMs = 120_000;
const terminationGraceMs = 1_000;

// The bound in force now: the setting when it is a positive integer.
export function transportBoundMs(env = process.env) {
  const value = env[transportBoundSetting] ?? "";
  return /^[1-9]\d*$/.test(value) ? Number(value) : defaultTransportBoundMs;
}

// A remote transport command's subcommand and remote (its first operand);
// null for local commands. Managed callers name the subcommand first.
function transportOf([subcommand, ...operands]) {
  if (!transportSubcommands.has(subcommand)) return null;
  const remote = operands.find((arg) => !arg.startsWith("-")) ?? null;
  return { subcommand, remote };
}

export async function git(cwd, ...args) {
  const transport = transportOf(args);
  if (transport) return boundedTransport(cwd, args, transport);
  return exec("git", args, { cwd });
}

function signalProcessGroup(pid, signal) {
  try {
    process.kill(-pid, signal);
  } catch {
    // The group already ended.
  }
}

// Runs one remote transport command in its own process group and resolves as
// `exec` does. When the bound expires, the whole group (Git and the transport
// it started, such as ssh) receives SIGTERM, then SIGKILL once it closes or a
// short grace passes, and the call rejects with `code: "transport-timeout"`.
// Other failures reject with the shape `exec` gives a failed command.
/** @returns {Promise<{ stdout: string, stderr: string }>} */
function boundedTransport(cwd, args, { subcommand, remote }) {
  const boundMs = transportBoundMs();
  const cmd = `git ${args.join(" ")}`;
  return new Promise((resolve, reject) => {
    const child = spawn("git", args, {
      cwd,
      detached: true,
      stdio: ["ignore", "pipe", "pipe"],
    });
    const output = { stdout: "", stderr: "" };
    for (const stream of ["stdout", "stderr"]) {
      child[stream].setEncoding("utf8").on("data", (chunk) => {
        output[stream] += chunk;
      });
    }
    const fail = (error, fields) =>
      reject(Object.assign(error, fields, { cmd, ...output }));
    let expired = false;
    const timer = setTimeout(() => {
      expired = true;
      signalProcessGroup(child.pid, "SIGTERM");
      const stop = () => {
        clearTimeout(grace);
        child.off("close", stop);
        signalProcessGroup(child.pid, "SIGKILL");
        const error = new Error(
          `${cmd} did not answer within ${boundMs} ms; its process tree was stopped`,
        );
        const facts = { subcommand, remote, boundMs, killed: true };
        fail(error, { code: "transport-timeout", ...facts });
      };
      const grace = setTimeout(stop, terminationGraceMs);
      child.once("close", stop);
    }, boundMs);
    child.on("error", (error) => {
      clearTimeout(timer);
      fail(error, {});
    });
    child.on("close", (code, signal) => {
      if (expired) return;
      clearTimeout(timer);
      if (code === 0) resolve(output);
      else {
        const error = new Error(`Command failed: ${cmd}\n${output.stderr}`);
        fail(error, { code, killed: false, signal });
      }
    });
  });
}

export async function revParse(cwd, ref) {
  return (await git(cwd, "rev-parse", ref)).stdout.trim();
}

// The SHA `remote` holds at `ref`, read live; a remote name resolves in `cwd`.
export async function lsRemoteSha(remote, ref, cwd) {
  const { stdout } = await git(cwd, "ls-remote", remote, ref);
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
  return (
    await git(
      worktree,
      "rev-parse",
      "--path-format=absolute",
      "--git-common-dir",
    )
  ).stdout.trim();
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
export async function inspectCheckout(checkout, head) {
  return {
    head: head ?? (await revParse(checkout, "HEAD")),
    status: (await git(checkout, "status", "--porcelain")).stdout,
  };
}

// Soft-fail lookup of the workspace's remote-tracking tip for an authorized
// target. Missing tracking refs are treated as an absent remote tip.
export async function fetchedTarget(workspace, targetRef, remote = "origin") {
  try {
    return await revParse(workspace, originTrackingRef(targetRef, remote));
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
export async function inspectDefaultCheckoutMaintenance(
  workspace,
  defaultCheckout,
  remote,
  targetRef,
) {
  if (!defaultCheckout) return "not applicable";
  const remoteTip = await revParse(
    workspace,
    originTrackingRef(targetRef, remote),
  );
  try {
    return maintenanceFromInspection(
      await inspectCheckout(defaultCheckout),
      remoteTip,
    );
  } catch {
    return "deferred";
  }
}
