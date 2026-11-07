// Keeps the done catalog beside the backlog current through the Git-aware
// adapters. An operation that changed the done directory has its catalog
// rebuilt from the final record files by the producer's own rebuild, and a
// change to the catalog alone never stops it, through a self-registered
// driver whose provisional result that rebuild settles. A merge stages the
// rebuilt catalog into its own commit; an accepted rebase or cherry-pick
// gains one catalog-only commit at its tip when the rebuild changed the
// catalog. An operation that changed no done path leaves the catalog's bytes
// as they are.
import { join, posix } from "node:path";
import { fileURLToPath } from "node:url";
import {
  creditInWorkspace,
  DeveloperIdentityRefused,
} from "../../dough-execute-plan/scripts/workspace-agent-authorship.mjs";
import { rebuildDoneCatalog } from "./product-backlog-complete.mjs";
import { doneCatalogPath } from "./product-backlog-done-catalog.mjs";
import { doneRecordDirectory } from "./product-backlog-done-record.mjs";
import {
  git,
  gitLine,
  gitOutcome,
  registerMergeDriver,
} from "./product-backlog-git-repository.mjs";

const catalogDriver = {
  name: "dough-done-catalog",
  description: "Combine done catalog rows until the adapter rebuilds it",
  script: fileURLToPath(
    new URL("./product-backlog-git-done-catalog-driver.mjs", import.meta.url),
  ),
};

// The repository-relative done directory beside the backlog at `file`.
export function doneDirectoryBeside(file) {
  return posix.join(posix.dirname(file), doneRecordDirectory);
}

function doneCatalogBeside(file) {
  return posix.join(posix.dirname(file), doneCatalogPath);
}

// Registers the catalog beside the backlog at `file` to be combined by the
// catalog driver, which never reports a conflict.
export function ensureDoneCatalogDriverRegistered(repoRoot, file) {
  registerMergeDriver(repoRoot, doneCatalogBeside(file), catalogDriver);
}

// Whether any path under the done directory beside `file` differs between
// `base` and any of `sides` (commits or refs).
export function doneDirectoryChanged(repoRoot, file, base, sides) {
  const directory = doneDirectoryBeside(file);
  return sides.some(
    (side) =>
      gitLine(
        ["diff", "--name-only", base, side, "--", directory],
        repoRoot,
      ) !== "",
  );
}

// Rebuilds the catalog beside `file` from the record files in the worktree
// and stages the result (or its removal), once no record file there is left
// unresolved. While one is, nothing is rebuilt: a later call after the
// human's resolution rebuilds from the resolved records. Returns the
// rebuild's outcome, or undefined when it waited.
export function stageRebuiltDoneCatalog(repoRoot, file) {
  const catalog = doneCatalogBeside(file);
  const unresolvedRecords = git(
    ["ls-files", "-u", "-z", "--", doneDirectoryBeside(file)],
    repoRoot,
  )
    .split("\0")
    .filter((line) => line !== "" && line.split("\t")[1] !== catalog);
  if (unresolvedRecords.length > 0) return undefined;
  const outcome = rebuildDoneCatalog(join(repoRoot, posix.dirname(file)));
  if (outcome.status === "absent" || outcome.status === "removed") {
    git(["rm", "--cached", "-q", "--ignore-unmatch", "--", catalog], repoRoot);
  } else {
    git(["add", "--", catalog], repoRoot);
  }
  return outcome;
}

const catalogCommitSubject = "Rebuild the done catalog from its record files\n";

// Concludes a rebase or cherry-pick whose replay Git completed and this
// adapter accepted, its result `accepted`, onto the commit `base` it
// replayed onto. When the replayed commits changed the done directory beside
// `file`, the catalog rebuilt from the tip's record files is committed at
// the tip in one commit changing only the catalog, credited like any other
// agent commit in an agent's owned workspace; no replayed commit is
// rewritten. Returns `accepted` when nothing needed committing or the commit
// was made; an unusable developer or a commit Git refuses leaves the rebuilt
// catalog staged and uncommitted, reported as `catalog-uncommitted`.
export async function commitRebuiltDoneCatalog(repoRoot, file, base, accepted) {
  if (!doneDirectoryChanged(repoRoot, file, base, ["HEAD"])) return accepted;
  stageRebuiltDoneCatalog(repoRoot, file);
  const catalog = doneCatalogBeside(file);
  const staged = gitOutcome(
    ["diff", "--cached", "--quiet", "HEAD", "--", catalog],
    repoRoot,
  );
  if (staged.code === 0) return accepted;
  const uncommitted =
    `${accepted.message}\nThe done catalog rebuilt from ` +
    `the record files at the tip is staged but not committed`;
  let message;
  try {
    message = await creditInWorkspace(repoRoot, catalogCommitSubject);
  } catch (error) {
    if (!(error instanceof DeveloperIdentityRefused)) throw error;
    return {
      status: "catalog-uncommitted",
      message:
        `${uncommitted}, because its developer credit is refused. Fix the ` +
        `Git committer identity, then commit ${catalog} alone with the ` +
        `developer credited.\n${error.message}`,
    };
  }
  const committed = gitOutcome(
    ["commit", "-q", "-m", message, "--", catalog],
    repoRoot,
  );
  if (committed.code !== 0) {
    return {
      status: "catalog-uncommitted",
      message:
        `${uncommitted}, because Git refused the commit. Settle what Git ` +
        `reports, then commit ${catalog} alone.\n${committed.stderr}`,
    };
  }
  return accepted;
}
