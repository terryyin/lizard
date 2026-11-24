// Transport staging and owned-commit identity checks for publication resume.
import { git, revParse } from "./publication-git.mjs";

// Runs one remote transport step. A step that outlasts the transport bound
// rethrows its `transport-timeout` error naming the resume `stage` and the
// attempt's facts, so a managed caller can report it as a stop while other
// callers see it as any other thrown Git failure.
export async function atStage(stage, facts, operation) {
  try {
    return await operation();
  } catch (error) {
    if (error?.code === "transport-timeout") {
      Object.assign(error, { stage, ...facts });
    }
    throw error;
  }
}

// A repository management context without a checked-out commit (a Git
// directory whose owned worktree is already retired) holds no owned commits.
export async function ownedCommitIdentity(workspace) {
  try {
    await git(workspace, "rev-parse", "-q", "--verify", "HEAD");
  } catch (error) {
    if (error.code !== 1) throw error;
    return { head: null, count: 0 };
  }
  return {
    head: await revParse(workspace, "HEAD"),
    count: Number(
      (await git(workspace, "rev-list", "--count", "HEAD")).stdout.trim(),
    ),
  };
}

export function assertOwnedCommitsPreserved(before, after) {
  if (after.head !== before.head || after.count !== before.count) {
    throw new Error("resume duplicated or moved the owned commit");
  }
}
