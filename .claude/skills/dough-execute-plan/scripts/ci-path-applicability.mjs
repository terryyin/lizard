// Conservative Git comparison used to decide whether a registered
// revision's CI coverage can be answered without waiting for an exact-SHA
// run: `required`, `not_required` (with the applicable ancestor attempt as
// basis), or `indeterminate` when the evidence does not safely support
// either verdict. Delegates workflow-policy reading to
// `ci-workflow-path-policy.mjs`.
//
// This module is a pure classifier. It does not read or write mailbox
// coverage state, publish events, or call `gh`; `ci-mailbox-revision-
// coverage.mjs` and `ci-mailbox-store.mjs` own that (a later slice wires
// this classifier into them).

import { execFileSync } from "node:child_process";
import { readCiPathIgnorePolicy } from "./ci-workflow-path-policy.mjs";

function isPathIgnored(path, pathsIgnore) {
  return pathsIgnore.some((pattern) => {
    const prefix = pattern.slice(0, -"/**".length);
    return path === prefix || path.startsWith(`${prefix}/`);
  });
}

function gitOutput(repoDir, args) {
  return execFileSync("git", ["-C", repoDir, ...args], {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  });
}

// Reads the workflow file's text at a specific revision (the registered
// revision is the policy source, per the plan's decision). Returns null for
// an unreadable revision or missing file rather than throwing.
export function readWorkflowContentAtRevision(
  repoDir,
  sha,
  workflowPath = ".github/workflows/ci.yml",
) {
  try {
    return gitOutput(repoDir, ["show", `${sha}:${workflowPath}`]);
  } catch {
    return null;
  }
}

function isAncestor(repoDir, ancestorSha, descendantSha) {
  if (ancestorSha === descendantSha) return true;
  try {
    gitOutput(repoDir, [
      "merge-base",
      "--is-ancestor",
      ancestorSha,
      descendantSha,
    ]);
    return true;
  } catch {
    return false;
  }
}

function ancestorDistance(repoDir, ancestorSha, descendantSha) {
  if (ancestorSha === descendantSha) return 0;
  try {
    const out = gitOutput(repoDir, [
      "rev-list",
      "--count",
      `${ancestorSha}..${descendantSha}`,
    ]).trim();
    const count = Number.parseInt(out, 10);
    return Number.isFinite(count) ? count : Number.POSITIVE_INFINITY;
  } catch {
    return Number.POSITIVE_INFINITY;
  }
}

// Returns parsed `git diff --name-status` entries between two revisions, or
// null when the comparison itself cannot be read.
function diffNameStatus(repoDir, fromSha, toSha) {
  if (fromSha === toSha) return [];
  try {
    const out = gitOutput(repoDir, [
      "diff",
      "--name-status",
      "-M",
      fromSha,
      toSha,
    ]);
    return out
      .split("\n")
      .filter((line) => line.trim() !== "")
      .map((line) => line.split("\t"));
  } catch {
    return null;
  }
}

// Conservative classification of a tree comparison: deletions and renames
// touching any non-ignored path never qualify as ignored-only, even alone,
// so moving or deleting a non-ignored path cannot look like an ignored-only
// edit. A clean, unambiguous non-ignored change (not mixed with an ignored
// one) is `required`; a mix of ignored and non-ignored changes is
// `indeterminate` rather than an assumed `required`, since the comparison
// basis may span more than the registered revision's own change.
function classifyDiff(entries, pathsIgnore) {
  if (entries === null)
    return { verdict: "indeterminate", reason: "unreadable-diff" };

  let sawIgnored = false;
  let sawNonIgnored = false;
  for (const [status, ...paths] of entries) {
    const kind = status[0];
    if (kind === "D") {
      if (isPathIgnored(paths[0], pathsIgnore)) sawIgnored = true;
      else return { verdict: "indeterminate", reason: "non-ignored-delete" };
    } else if (kind === "R" || kind === "C") {
      const [oldPath, newPath] = paths;
      if (
        !isPathIgnored(oldPath, pathsIgnore) ||
        !isPathIgnored(newPath, pathsIgnore)
      )
        return { verdict: "indeterminate", reason: "non-ignored-rename" };
      sawIgnored = true;
    } else if (kind === "A" || kind === "M") {
      if (isPathIgnored(paths[0], pathsIgnore)) sawIgnored = true;
      else sawNonIgnored = true;
    } else {
      return { verdict: "indeterminate", reason: "unsupported-diff-status" };
    }
  }
  if (sawNonIgnored && sawIgnored)
    return { verdict: "indeterminate", reason: "mixed-paths" };
  if (sawNonIgnored) return { verdict: "required" };
  return { verdict: "not_required" };
}

// The classifier itself. `candidateShas` are discovered attempt SHAs (e.g.
// from ci-runs.mjs discovery) to consider as a reuse basis; the nearest one
// that is a proved ancestor of `registeredSha` is used. Returns
// `{ result: "required" }`, `{ result: "not_required", basis: { sha } }`, or
// `{ result: "indeterminate", reason }`. Does not mutate any coverage state.
export function classifyRevisionApplicability({
  repoDir,
  event,
  registeredSha,
  candidateShas = [],
  workflowPath = ".github/workflows/ci.yml",
}) {
  const content = readWorkflowContentAtRevision(
    repoDir,
    registeredSha,
    workflowPath,
  );
  if (content === null)
    return { result: "indeterminate", reason: "unreadable-revision" };

  const policy = readCiPathIgnorePolicy(content);
  if (!policy.supported)
    return { result: "indeterminate", reason: "unsupported-workflow-policy" };

  const eventPolicy = policy.events[event];
  if (!eventPolicy)
    return { result: "indeterminate", reason: "unsupported-workflow-policy" };

  if (eventPolicy.pathsIgnore.length === 0) return { result: "required" };

  const ancestors = candidateShas
    .filter((sha) => isAncestor(repoDir, sha, registeredSha))
    .map((sha) => ({
      sha,
      distance: ancestorDistance(repoDir, sha, registeredSha),
    }))
    .sort((a, b) => a.distance - b.distance);
  if (ancestors.length === 0)
    return { result: "indeterminate", reason: "no-ancestor-basis" };

  const nearestSha = ancestors[0].sha;
  const entries = diffNameStatus(repoDir, nearestSha, registeredSha);
  const classified = classifyDiff(entries, eventPolicy.pathsIgnore);

  if (classified.verdict === "not_required")
    return { result: "not_required", basis: { sha: nearestSha } };
  if (classified.verdict === "required") return { result: "required" };
  return { result: "indeterminate", reason: classified.reason };
}
