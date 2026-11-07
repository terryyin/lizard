import { git } from "./publication-git.mjs";

export async function isAncestor(workspace, ancestor, descendant) {
  try {
    await git(workspace, "merge-base", "--is-ancestor", ancestor, descendant);
    return true;
  } catch {
    return false;
  }
}

// A retained comparison is optional for legacy callers. When supplied, both
// ends must be fixed commit IDs and the base must belong to that candidate's
// history. Validate before fetch, push, or registration; never infer a base
// from the accepted tip's parent or today's target.
export async function validateRetainedComparison(
  workspace,
  candidateSha,
  suffixBase,
) {
  if (suffixBase === undefined) return;
  for (const sha of [suffixBase, candidateSha]) {
    if (
      typeof sha !== "string" ||
      !/^(?:[a-f0-9]{40}|[a-f0-9]{64})$/i.test(sha) ||
      (await git(workspace, "cat-file", "-t", sha)).stdout.trim() !== "commit"
    ) {
      throw new Error("retained comparison requires full commit IDs");
    }
  }
  if (!(await isAncestor(workspace, suffixBase, candidateSha))) {
    throw new Error("retained suffix base is not an ancestor of the candidate");
  }
}
