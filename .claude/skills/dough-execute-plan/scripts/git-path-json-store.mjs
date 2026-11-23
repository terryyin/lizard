// One local JSON file at a Git path, such as `open-dough/session-record`, so
// each worktree keeps its own. Reading an absent file gives undefined;
// replacing writes the file whole so an interruption leaves the previous
// content intact; removing an absent file is a no-op.
import { execFileSync } from "node:child_process";
import {
  mkdirSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";

function storePath(workspace, gitPath) {
  const found = execFileSync("git", ["rev-parse", "--git-path", gitPath], {
    cwd: workspace,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  });
  return resolve(workspace, found.trim());
}

export function readGitPathJson(workspace, gitPath) {
  try {
    return JSON.parse(readFileSync(storePath(workspace, gitPath), "utf8"));
  } catch (error) {
    if (error.code === "ENOENT") return undefined;
    throw error;
  }
}

export function replaceGitPathJson(workspace, gitPath, value) {
  const file = storePath(workspace, gitPath);
  mkdirSync(dirname(file), { recursive: true });
  const temporary = `${file}.${process.pid}.tmp`;
  writeFileSync(temporary, `${JSON.stringify(value, null, 2)}\n`);
  renameSync(temporary, file);
  return value;
}

export function removeGitPathJson(workspace, gitPath) {
  rmSync(storePath(workspace, gitPath), { force: true });
}
