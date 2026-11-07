// The three-way comparison behind the source freshness command: one record's
// base (the Take, or a later trunk revision the branch copy contains), trunk,
// and branch-copy texts, merged with `git merge-file` in
// scratch files under the repository's own Git directory, so a host that only
// lets the agent write inside the workspace can run it. Trunk text equal to
// the base, or a clean application equal to the branch copy, is `unchanged`; an overlapping edit, or an application that
// changes or removes a slice the branch plan marks done, is a `conflict`; any
// other change is `amended`, carrying the merged text as its `application`.
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { gitPath } from "../../dough-product-backlog/scripts/product-backlog-git-repository.mjs";
import {
  readPlanSlices,
  unfencedPlanLines,
} from "../../dough-product-backlog/scripts/product-backlog-plan-reader.mjs";
import { splitSource } from "../../dough-product-backlog/scripts/product-backlog-source.mjs";
import { git } from "./publication-git.mjs";

// A fresh scratch directory in `repository`'s (per-worktree) Git directory,
// which `git fetch` already writes and `git status` never shows.
function scratchDirectory(repository) {
  return mkdtemp(gitPath(repository, "source-freshness-"));
}

// `git merge-file` of branch (ours), base and trunk (theirs) texts, and the
// base-to-trunk change as a unified diff, computed in scratch files
// removed afterwards.
async function compareThreeWay(repository, branch, base, trunk) {
  const dir = await scratchDirectory(repository);
  try {
    await writeFile(join(dir, "branch"), branch);
    await writeFile(join(dir, "take"), base);
    await writeFile(join(dir, "trunk"), trunk);
    let change;
    try {
      await git(
        dir,
        "diff",
        "--no-index",
        "--no-ext-diff",
        "--no-color",
        "take",
        "trunk",
      );
      change = "";
    } catch (error) {
      if (error.code !== 1) throw error;
      change = error.stdout;
    }
    try {
      const merged = await git(
        dir,
        "merge-file",
        "-p",
        "-L",
        "branch",
        "-L",
        "take",
        "-L",
        "trunk",
        "branch",
        "take",
        "trunk",
      );
      return { clean: true, merged: merged.stdout, change };
    } catch (error) {
      if (!(Number.isInteger(error.code) && error.code > 0 && error.code < 128))
        throw error;
      return { clean: false, merged: error.stdout, change };
    }
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
}

// The text of slice `index` from its heading to the next heading, without
// trailing blank lines; undefined when the plan has no such slice.
function sliceBlock(source, index) {
  const { lines } = splitSource(source);
  const unfenced = unfencedPlanLines(lines);
  const heading = new RegExp(`^### +${index}\\. `);
  const start = lines.findIndex(
    (line, at) => unfenced[at] && heading.test(line),
  );
  if (start === -1) return undefined;
  let end = lines.length;
  for (let at = start + 1; at < lines.length; at += 1) {
    if (unfenced[at] && /^##+ /.test(lines[at])) {
      end = at;
      break;
    }
  }
  const block = lines.slice(start, end);
  while (block.at(-1)?.trim() === "") block.pop();
  return block.join("\n");
}

// Done slices of the branch plan that `application` changes or removes.
function doneSlicesChanged(branchPlan, application) {
  const read = readPlanSlices(branchPlan);
  if (read.status !== "interpreted") return [];
  return read.slices
    .filter((slice) => slice.status === "done")
    .flatMap(({ index, name }) => {
      const before = sliceBlock(branchPlan, index);
      const after = sliceBlock(application, index);
      if (after === before) return [];
      return [
        { index, name, change: after === undefined ? "removed" : "changed" },
      ];
    });
}

// One record's comparison in `repository`: its state, and for `amended` the
// merged record text `application` (the whole plan, or the story section's
// text).
export async function compareRecord({
  repository,
  record,
  path,
  base,
  trunk,
  branch,
  doneRule,
}) {
  if (trunk === base) return { record, path, state: "unchanged" };
  const { clean, merged, change } = await compareThreeWay(
    repository,
    branch,
    base,
    trunk,
  );
  if (clean && merged === branch) return { record, path, state: "unchanged" };
  const conflict = (reason, fields = {}) => ({
    record,
    path,
    state: "conflict",
    reason,
    ...fields,
    change,
    branchText: branch,
    trunkText: trunk,
  });
  if (!clean) return conflict("overlapping-edit");
  if (doneRule) {
    const slices = doneSlicesChanged(branch, merged);
    if (slices.length) return conflict("done-slice", { slices });
  }
  return { record, path, state: "amended", change, application: merged };
}

const asLines = (text) => `${text}\n`;
const fromLines = (text) => text.replace(/\n$/, "");

// `compareRecord` for a story section's text, which has no final newline:
// compared as whole lines, reported as section text.
export async function compareSection({ base, trunk, branch, ...rest }) {
  const compared = await compareRecord({
    ...rest,
    base: asLines(base),
    trunk: asLines(trunk),
    branch: asLines(branch),
  });
  for (const field of ["application", "branchText", "trunkText"])
    if (compared[field] !== undefined)
      compared[field] = fromLines(compared[field]);
  return compared;
}
