import { git, gitLine } from "./product-backlog-git-repository.mjs";
import { mergeBacklogs } from "./product-backlog-merge.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

function contentAt(repoRoot, file, ref) {
  try {
    return git(["show", `${ref}:${file}`], repoRoot);
  } catch {
    throw new BacklogError(
      `${file} is not present at ${ref}, so the whole-operation aggregate ` +
        `comparison cannot be run.`,
    );
  }
}

// Individually clean steps can hide a dispute visible at the original endpoints.
function reconciliationRefusal(
  repoRoot,
  file,
  preRebaseTip,
  destinationAtStart,
) {
  const base = gitLine(
    ["merge-base", preRebaseTip, destinationAtStart],
    repoRoot,
  );
  const ancestor = contentAt(repoRoot, file, base);
  const branch = contentAt(repoRoot, file, preRebaseTip);
  const destination = contentAt(repoRoot, file, destinationAtStart);
  const actual = contentAt(repoRoot, file, "HEAD");
  try {
    if (mergeBacklogs(ancestor, branch, destination).source !== actual) {
      return (
        `The shared resolver's own whole-operation candidate for ` +
        `${file} does not match what the operation actually left at HEAD, ` +
        `comparing the true pre-operation tip (${preRebaseTip}) against ` +
        `the true destination-at-start (${destinationAtStart}).`
      );
    }
  } catch (error) {
    if (!(error instanceof BacklogError)) throw error;
    return error.message;
  }
  return undefined;
}

export function acceptCleanRebase(
  repoRoot,
  file,
  ref,
  preRebaseTip,
  destinationAtStart,
) {
  const refusal = reconciliationRefusal(
    repoRoot,
    file,
    preRebaseTip,
    destinationAtStart,
  );
  if (refusal !== undefined) {
    return {
      status: "disputed",
      message:
        `Rebased onto ${ref} with no Git conflict in ${file} at any step, ` +
        `but the whole-rebase aggregate comparison of the true pre-rebase ` +
        `tip (${preRebaseTip}) against the true destination-at-start ` +
        `(${destinationAtStart}) disagrees with the result. This is not a ` +
        `Git conflict, so there is nothing staged to resolve in the usual ` +
        `way: the rebase's local commits are left exactly where the ` +
        `rebase completed them — on the branch, unpublished and fully ` +
        `recoverable. Repair ${file} by hand, or decide the current result ` +
        `should stand as is; either way, run \`validate\` on it before the ` +
        `managed caller publishes.\n${refusal}`,
    };
  }
  return {
    status: "rebased",
    message:
      `Rebased onto ${ref}; no Git conflict touched ${file}, and the ` +
      `whole-rebase aggregate comparison of the true pre-rebase tip and the ` +
      `true destination-at-start agrees with the result.`,
  };
}
