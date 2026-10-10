#!/usr/bin/env node
// The Git repository primitives the Git-aware backlog adapters share:
// resolving the real repository root a caller means, running real `git`
// commands against it while collecting what Git writes to stderr for the
// operation that ran them, and registering a custom merge driver for one
// attributed path, the shared resolver's for the backlog among them. Nothing
// here knows about the backlog's own content or invariants;
// `product-backlog-git-merge.mjs` and its driver own that, as
// `product-backlog-git-done-catalog.mjs` owns the done catalog's. A stopped
// rebase's or cherry-pick's own state is read in
// `product-backlog-git-operation-state.mjs`.
import { AsyncLocalStorage } from "node:async_hooks";
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { BacklogError } from "./product-backlog-refusal.mjs";

const driverName = "dough-product-backlog";
const driverScript = fileURLToPath(
  new URL("./product-backlog-git-driver.mjs", import.meta.url),
);

const gitDiagnostics = new AsyncLocalStorage();

// Runs one operation with its own collector for everything Git (and the
// merge driver and hooks Git runs) writes to stderr, and settles with that
// text beside how the operation ended: its `result`, or the `error` it threw
// (`threw` distinguishes the two). The caller decides whether the text
// matters: a successful operation has nothing to add to its receipt, while a
// stopped or failed one owes the caller Git's own diagnostics.
export async function collectingGitDiagnostics(operation) {
  const collector = { stderr: "" };
  try {
    const result = await gitDiagnostics.run(collector, operation);
    return { threw: false, result, stderr: collector.stderr };
  } catch (error) {
    return { threw: true, error, stderr: collector.stderr };
  }
}

// Git's stderr from one call reaches the collector of the operation running
// it, or — for a caller that runs no such operation — this process's own
// stderr, exactly as an uncaptured call would have shown it.
function reportGitStderr(stderr) {
  const collector = gitDiagnostics.getStore();
  if (collector) {
    collector.stderr += stderr;
  } else if (stderr !== "") {
    process.stderr.write(stderr);
  }
}

// Every real Git call this adapter makes, in one place: `cwd` is always the
// resolved repository root, never the caller's own working directory, so a
// launch from a subdirectory of the repository still names paths Git and
// this tool agree on. `env` is only ever additional variables layered onto
// this process's own environment — never a replacement of it — for the rare
// call (resuming a rebase) that must not stop to open an editor. Git's stderr
// is captured rather than forwarded (see `reportGitStderr`); a failure throws
// an error carrying Git's exit `status`, `stdout`, and `stderr`.
export function git(args, cwd, env) {
  const result = spawnSync("git", args, {
    cwd,
    encoding: "utf8",
    env: env ? { ...process.env, ...env } : process.env,
    stdio: ["ignore", "pipe", "pipe"],
  });
  const stdout = result.stdout ?? "";
  const stderr = result.stderr ?? "";
  reportGitStderr(stderr);
  if (result.error || result.status !== 0) {
    const error = new Error(
      `Command failed: git ${args.join(" ")}\n${stderr}`,
      result.error ? { cause: result.error } : undefined,
    );
    error.status = result.status;
    error.stdout = stdout;
    error.stderr = stderr;
    throw error;
  }
  return stdout;
}

export function gitLine(args, cwd, env) {
  return git(args, cwd, env).trim();
}

// The same call, reporting failure instead of throwing, for the calls whose
// non-zero exit is an ordinary outcome this adapter reads rather than a
// crash: a merge that left conflicts, or a commit Git refuses because
// something unrelated is still unresolved.
export function gitOutcome(args, cwd, env) {
  try {
    return { code: 0, stdout: git(args, cwd, env) };
  } catch (error) {
    return {
      code: typeof error.status === "number" ? error.status : 1,
      stdout: error.stdout ?? "",
      stderr: error.stderr ?? "",
    };
  }
}

// The developer name Git is configured with for work in `cwd`, as a commit
// made there would record it, or undefined when Git names none (or cannot be
// run). Nothing in `cwd` needs to be a repository.
export function configuredUserName(cwd) {
  const outcome = gitOutcome(["config", "user.name"], cwd);
  const name = outcome.code === 0 ? outcome.stdout.trim() : "";
  return name === "" ? undefined : name;
}

export function repositoryRoot(cwd) {
  try {
    return gitLine(["rev-parse", "--show-toplevel"], cwd);
  } catch {
    throw new BacklogError(`${cwd} is not inside a Git repository.`);
  }
}

// Where Git itself keeps one named piece of its own state for this checkout.
// `<repoRoot>/.git` is a directory only in a primary checkout; in a linked
// worktree (`git worktree add`) it is a file, shared state such as
// `info/attributes` lives in the common Git directory, and in-progress
// operation state (`MERGE_HEAD`, `rebase-merge`, `sequencer`, ...) is that
// worktree's own. Git already knows which is which, so it is asked rather
// than assumed; a relative answer is relative to the checkout it was asked
// from.
export function gitPath(repoRoot, name) {
  return resolve(
    repoRoot,
    gitLine(["rev-parse", "--git-path", name], repoRoot),
  );
}

// Registers `driverScript` as the custom merge driver `name` (described by
// `description`) for the one repository-relative `path`, for every future
// content merge Git attempts on it in this checkout. Both the attribute and
// the driver command are written to this checkout's own Git directory —
// never to a file a project tracks or ships — because registering a driver
// for every checkout a project might have, on every install, is a separate,
// later concern; an adapter only needs the mechanism to be in effect for the
// operation it is about to run. Git hands the driver `%O %A %B %P`.
export function registerMergeDriver(
  repoRoot,
  path,
  { name, description, script },
) {
  const attributesPath = gitPath(repoRoot, "info/attributes");
  const line = `/${path} merge=${name}`;
  const existing = existsSync(attributesPath)
    ? readFileSync(attributesPath, "utf8")
    : "";
  if (!existing.split("\n").includes(line)) {
    const separator = existing !== "" && !existing.endsWith("\n") ? "\n" : "";
    // Git creates `info/` only from its templates, which a repository may lack.
    mkdirSync(dirname(attributesPath), { recursive: true });
    writeFileSync(attributesPath, `${existing}${separator}${line}\n`, "utf8");
  }
  git(["config", `merge.${name}.name`, description], repoRoot);
  git(
    [
      "config",
      `merge.${name}.driver`,
      `${process.execPath} ${script} %O %A %B %P`,
    ],
    repoRoot,
  );
}

// Registers the backlog path itself to be reconciled by the shared resolver.
export function ensureDriverRegistered(repoRoot, file) {
  registerMergeDriver(repoRoot, file, {
    name: driverName,
    description: "Reconcile the product backlog through its shared resolver",
    script: driverScript,
  });
}

// The recoverable shape of a stop neither adapter itself decided: something
// other than this path is still unresolved, so nothing about this path's own
// content is judged. `product-backlog-git-rebase.mjs` and
// `product-backlog-git-cherry-pick-stop.mjs` independently reached this exact
// wording, differing only in the operation noun, once cherry-pick's own
// "blocked" case existed alongside rebase's; expressed once here rather than
// duplicated a second time.
export function blockedStopMessage(operationNoun, file, stderr) {
  return (
    `The ${operationNoun} stopped, but ${file} is not the unresolved path; ` +
    `something unrelated needs human resolution first. This gate leaves ` +
    `the ${operationNoun} exactly as it is.\n${stderr}`
  );
}
