#!/usr/bin/env node
// Whether a candidate's bytes are a backlog `product-backlog-git-merge.mjs`
// can read back — the same invariants every version this tool reads is
// already held to — without repeating the three-way reconciliation a
// human's decision may deliberately differ from. This is the whole of what
// "accepting a human resolution" means: whether the result is readable, not
// whether it matches what the automatic merge would have produced.
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { git } from "./product-backlog-git-repository.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { readVersion } from "./product-backlog-version.mjs";

function withTemporaryFile(contents, run) {
  const directory = mkdtempSync(join(tmpdir(), "dough-backlog-candidate-"));
  try {
    const path = join(directory, "candidate.md");
    writeFileSync(path, contents, "utf8");
    return run(path);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
}

export function validateCandidate(contents, label) {
  try {
    withTemporaryFile(contents, (path) => {
      try {
        readVersion(path, label);
      } catch (error) {
        if (!(error instanceof BacklogError)) {
          throw error;
        }
        // `readVersion` names the temporary file this validation happens to
        // use; that path means nothing to whoever reads the refusal, so it is
        // dropped rather than shown as if it were a real, repairable file.
        throw new BacklogError(error.message.split(` (${path})`).join(""));
      }
    });
    return { valid: true };
  } catch (error) {
    if (error instanceof BacklogError) {
      return { valid: false, reason: error.refusal };
    }
    throw error;
  }
}

// The bytes about to be accepted, read from the index rather than assumed
// from the working tree, so a worktree file a human forgot to `git add`, or
// one that no longer matches what actually got staged, cannot pass by
// coincidence. Both are inspected, and a mismatch between them is refused in
// its own right before either is judged against the backlog's invariants.
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
