// Mechanical preparation for one fresh execution commit. Semantic proof and
// project command selection stay with the coordinator; each actual operation
// must finish successfully before its dependent mutation starts.
import { lstat } from "node:fs/promises";
import { isAbsolute, relative, resolve } from "node:path";
import { agentCommit } from "./agent-commit.mjs";
import { exec, git, revParse } from "./publication-git.mjs";
import { markSelectedSliceDone } from "./selected-slice-status.mjs";
import { checkStoryObligations } from "./story-obligations.mjs";

function commandRefusal(operation) {
  if (!operation || typeof operation !== "object") return "missing requirement";
  if (operation.notRequired !== undefined) {
    return typeof operation.notRequired === "string" &&
      operation.notRequired.trim() &&
      operation.command === undefined
      ? null
      : "invalid or conflicting notRequired reason";
  }
  if (
    !Array.isArray(operation.command) ||
    !operation.command.length ||
    operation.command.some((part) => typeof part !== "string") ||
    !operation.command[0]
  )
    return "supply an executable and argument array, or an explicit notRequired reason";
  if (
    operation.timeoutMs !== undefined &&
    (!Number.isSafeInteger(operation.timeoutMs) || operation.timeoutMs <= 0)
  )
    return "timeoutMs must be a positive integer";
  return null;
}

function localPath(workspace, path) {
  return (
    typeof path === "string" &&
    path !== "" &&
    !isAbsolute(path) &&
    path !== "." &&
    !path.startsWith("../") &&
    relative(workspace, resolve(workspace, path)) === path
  );
}

function inputRefusal({ workspace, branch, message, preparation: p }) {
  if (!workspace || !branch) return "missing workspace or branch";
  if (typeof message !== "string" || !message.trim())
    return "missing commit message";
  if (!p || !["planned", "quick"].includes(p.kind))
    return "missing planned or quick execution context";
  if (!["agent", "git"].includes(p.commit))
    return "missing resolved agent or ordinary Git commit contract";
  for (const name of ["generation", "formatting", "summary"]) {
    const error = commandRefusal(p[name]);
    if (error) return `${name}: ${error}`;
  }
  if (p.kind === "planned") {
    if (
      !p.plan ||
      !localPath(workspace, p.plan.path) ||
      !Number.isSafeInteger(p.plan.slice) ||
      p.plan.slice < 1
    )
      return "planned work requires its workspace-relative plan and selected slice";
    const extra = Object.keys(p.plan).filter(
      (key) => !["path", "slice"].includes(key),
    );
    if (extra.length)
      return `plan accepts only path and slice, not ${extra.join(", ")}; delivery marks the selected slice done itself`;
  } else if (p.plan !== undefined) return "quick work must not invent a plan";
  const staging = p.staging;
  if (
    !staging ||
    !Array.isArray(staging.ownedPaths) ||
    !Array.isArray(staging.stagedPaths)
  )
    return "missing ownedPaths and stagedPaths staging boundaries";
  const paths = [...staging.ownedPaths, ...staging.stagedPaths];
  if (
    !paths.length ||
    paths.some((path) => !localPath(workspace, path)) ||
    new Set(paths).size !== paths.length
  )
    return "staging requires distinct exact workspace-relative file paths";
  if (p.kind === "planned" && !staging.ownedPaths.includes(p.plan.path))
    return "the plan must be included in ownedPaths";
  return null;
}

async function assertOwnedIndex(workspace, staging) {
  const { stdout } = await git(
    workspace,
    "diff",
    "--cached",
    "--name-only",
    "-z",
  );
  const owned = new Set([...staging.ownedPaths, ...staging.stagedPaths]);
  const unowned = stdout.split("\0").filter((path) => path && !owned.has(path));
  if (unowned.length)
    throw new Error(
      `staged ownership is unresolved: ${unowned.join(", ")}; resolve it with its owner without unstaging their work`,
    );
  for (const path of owned) {
    const entry = await lstat(resolve(workspace, path)).catch((error) => {
      if (error.code !== "ENOENT") throw error;
    });
    if (entry?.isDirectory())
      throw new Error(`stage exact files rather than directory ${path}`);
  }
}

async function runCommand(workspace, operation) {
  if (operation.notRequired !== undefined) return;
  const [executable, ...args] = operation.command;
  await exec(executable, args, {
    cwd: workspace,
    ...(operation.timeoutMs === undefined
      ? {}
      : { timeout: operation.timeoutMs }),
  });
}

// No rollback: failed preparation, index contents, and hooks remain available
// for a repaired attempt. This returns a commit receipt, never publication.
export async function commitPreparedExecutionIncrement(request) {
  const refusal = inputRefusal(request);
  if (refusal)
    return {
      ok: false,
      status: "preparation-refused",
      operation: "context",
      error: refusal,
    };
  const { workspace, branch, message, preparation: p } = request;
  let operation = "checkout";
  try {
    const current = (
      await git(workspace, "symbolic-ref", "--short", "HEAD")
    ).stdout.trim();
    if (current !== branch)
      throw new Error(`expected branch ${branch}, found ${current}`);
    operation = "staged ownership";
    await assertOwnedIndex(workspace, p.staging);
    for (const name of ["generation", "formatting"]) {
      operation = name;
      await runCommand(workspace, p[name]);
    }
    const obligations = async () => {
      operation = "story obligations";
      const result = await checkStoryObligations(
        resolve(workspace, p.plan.path),
        { slice: p.plan.slice },
      );
      if (!result.ok)
        throw Object.assign(
          new Error("selected slice obligations refused commit"),
          { details: result },
        );
    };
    if (p.kind === "planned") {
      await obligations();
      operation = "plan/status mutation";
      await markSelectedSliceDone(workspace, p.plan);
    }
    operation = "summary";
    await runCommand(workspace, p.summary);
    // Also check the changed record: the done status and the summary cannot
    // erase this dependent check or introduce an open obligation unnoticed.
    if (p.kind === "planned") await obligations();
    operation = "staged ownership";
    await assertOwnedIndex(workspace, p.staging);
    operation = "staging";
    if (p.staging.ownedPaths.length)
      await git(
        workspace,
        "--literal-pathspecs",
        "add",
        "--",
        ...p.staging.ownedPaths,
      );
    operation = "staged whitespace";
    await git(workspace, "diff", "--cached", "--check");
    operation = "commit";
    if (p.commit === "agent") {
      const committed = await agentCommit(workspace, { message });
      return committed.ok ? committed : { ...committed, operation: "commit" };
    }
    const committing = exec("git", ["commit", "--quiet", "-F", "-"], {
      cwd: workspace,
    });
    committing.child.stdin.end(message);
    await committing;
    return {
      ok: true,
      status: "committed",
      sha: await revParse(workspace, "HEAD"),
    };
  } catch (error) {
    return {
      ok: false,
      status: operation === "commit" ? "commit-failed" : "preparation-failed",
      operation,
      error:
        `${error.stdout ?? ""}${error.stderr ?? ""}`.trim() || error.message,
      ...(error.code === undefined ? {} : { code: error.code }),
      ...(error.signal ? { signal: error.signal } : {}),
      ...(error.killed === undefined ? {} : { killed: error.killed }),
      ...(error.details ? { details: error.details } : {}),
    };
  }
}
