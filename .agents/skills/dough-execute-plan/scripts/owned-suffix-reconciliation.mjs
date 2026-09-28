// Replay only the owned unpublished suffix onto a newer authorized tip.
// Backlog-touching suffixes go through the product-backlog rebase adapter;
// other suffixes use a raw rebase. Callers own proof, push, and observation.
import { fileURLToPath } from "node:url";
import { exec, git } from "./publication-git.mjs";

// Same default as dough-product-backlog; avoid importing that skill at module
// load so a deployed execute-plan skill remains loadable alone until a
// backlog-touching reconcile needs the adapter CLI beside it.
export const defaultBacklogPath = ".planning/PRODUCT-BACKLOG.md";

const rebaseCli = fileURLToPath(
  new URL(
    "../../dough-product-backlog/scripts/product-backlog-git-rebase.mjs",
    import.meta.url,
  ),
);

async function suffixTouchesBacklog(workspace, base, branch, backlogPath) {
  const names = (
    await git(
      workspace,
      "diff",
      "--name-only",
      `${base}..${branch}`,
      "--",
      backlogPath,
    )
  ).stdout.trim();
  return names !== "";
}

async function rebaseThroughAdapter({
  workspace,
  onto,
  upstream,
  branch,
  backlogPath,
}) {
  try {
    const { stdout, stderr } = await exec(process.execPath, [
      rebaseCli,
      "rebase",
      "--onto",
      onto,
      "--ref",
      upstream,
      "--branch",
      branch,
      "--cwd",
      workspace,
      "--file",
      backlogPath,
    ]);
    return { ok: true, stdout, stderr };
  } catch (error) {
    return {
      ok: false,
      code: error.code ?? 1,
      stdout: error.stdout ?? "",
      stderr: error.stderr ?? "",
    };
  }
}

async function rebaseRaw(workspace, onto, upstream, branch) {
  try {
    await git(workspace, "rebase", "--onto", onto, upstream, branch);
    return { ok: true };
  } catch (error) {
    return {
      ok: false,
      code: error.code ?? 1,
      stdout: error.stdout ?? "",
      stderr: error.stderr ?? "",
      message: error.message,
    };
  }
}

export async function reconcileOwnedSuffix({
  workspace,
  onto,
  upstream,
  branch,
  backlogPath = defaultBacklogPath,
}) {
  const touches = await suffixTouchesBacklog(
    workspace,
    upstream,
    branch,
    backlogPath,
  );
  if (touches) {
    return rebaseThroughAdapter({
      workspace,
      onto,
      upstream,
      branch,
      backlogPath,
    });
  }
  return rebaseRaw(workspace, onto, upstream, branch);
}
