// Validate the backlog bytes a Git operation is about to accept.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { backlogIsUnmerged, git } from "./product-backlog-git-repository.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { readVersion } from "./product-backlog-version.mjs";

export function validateCandidate(contents, label) {
  try {
    readVersion(contents, label);
    return { valid: true };
  } catch (error) {
    if (error instanceof BacklogError) {
      return { valid: false, reason: error.refusal };
    }
    throw error;
  }
}

// Read both copies so an unstaged edit cannot be accepted by coincidence.
export function acceptStaged(repoRoot, file) {
  let staged;
  try {
    staged = git(["show", `:0:${file}`], repoRoot);
  } catch {
    throw new BacklogError(
      `${file} is not staged in ${repoRoot}; there is nothing to accept.`,
    );
  }
  const worktree = readFileSync(join(repoRoot, file), "utf8");
  if (worktree !== staged) {
    return {
      valid: false,
      reason:
        `The working copy of ${file} does not match the bytes staged for ` +
        `this merge. Stage the exact candidate you intend to accept with ` +
        `\`git add ${file}\`, then continue.`,
    };
  }
  return validateCandidate(staged, "the staged candidate");
}

// Read back a human's decision without reconsidering automatic reconciliation.
export function stagedResolutionRefusal(repoRoot, file) {
  if (backlogIsUnmerged(repoRoot, file)) {
    throw new BacklogError(
      `${file} is still unresolved in ${repoRoot}. Edit it by hand, then ` +
        `\`git add ${file}\`, before continuing.`,
    );
  }
  const outcome = acceptStaged(repoRoot, file);
  if (!outcome.valid) {
    return { status: "refused", message: outcome.reason };
  }
  return undefined;
}
