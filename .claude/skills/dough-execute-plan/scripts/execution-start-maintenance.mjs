import { lstatSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import {
  joinSource,
  splitSource,
} from "../../dough-product-backlog/scripts/product-backlog-source.mjs";
import { sectionOf, selectedPreparation, show } from "./execution-source.mjs";
import { git } from "./publication-git.mjs";
import {
  ongoingOperation,
  refreshDefaultCheckout,
} from "./maintain-default-checkout.mjs";
import { remoteOf } from "./workspace-publication-ownership.mjs";

// Startup reports a local refresh failure separately from remote acceptance.
export function maintenance(request) {
  return refreshDefaultCheckout({
    checkout: request.integration,
    remote: remoteOf(request),
    integrationBranch: request.target,
  });
}

function outcome({ result, reason, error }) {
  return { result, ...(reason ? { reason } : {}), ...(error ? { error } : {}) };
}

function unresolved({ result }) {
  return result === "deferred" || result === "stopped";
}

// The start's local-refresh report: the latest outcome and its reason, plus
// an earlier distinct issue when the latest attempt also left the checkout
// unrefreshed. Checkout inventories stay out of the report; the decisions
// were made before this projection.
export function reportedMaintenance(latest, earlier) {
  const report = { maintenance: outcome(latest) };
  if (
    earlier &&
    earlier !== latest &&
    unresolved(earlier) &&
    unresolved(latest) &&
    (earlier.result !== latest.result || earlier.reason !== latest.reason)
  )
    report.earlierMaintenance = outcome(earlier);
  return report;
}

// Only accepted admission owns withdrawal of its published contribution.
// Withdrawal preserves the index; ordinary refresh still requires a clean
// checkout, so staged content and later source edits stay local.
export async function acceptedMaintenance(request, publishedSha) {
  if (request.admit && request.integration) {
    try {
      await withdrawPublishedDrafts(request, publishedSha);
    } catch (error) {
      return {
        result: "deferred",
        reason: "refresh-failed",
        error: error.stderr || error.message,
      };
    }
  }
  return maintenance(request);
}

function fileText(path) {
  try {
    return lstatSync(path).isFile() ? readFileSync(path, "utf8") : undefined;
  } catch (error) {
    if (error.code === "ENOENT") return null;
    throw error;
  }
}

async function draftCheckoutHead(checkout, target) {
  if (await ongoingOperation(checkout)) return undefined;
  const branch = (
    await git(checkout, "branch", "--show-current")
  ).stdout.trim();
  if (branch !== target) return undefined;
  return (await git(checkout, "rev-parse", "HEAD")).stdout.trim();
}

async function hasStagedChanges(checkout, path) {
  return Boolean(
    (await git(checkout, "diff", "--cached", "--name-only", "--", path)).stdout,
  );
}

function fileMode(path) {
  return lstatSync(path, { throwIfNoEntry: false })?.mode & 0o777;
}

// Removing a newly added section also removes separator bytes. Compare those
// bytes exactly so whitespace-only edits made after drafting remain local.
function sectionBoundary(section) {
  const { document, region } = section;
  let start = region.start;
  while (start > 0 && document.lines[start - 1] === "") start -= 1;
  return JSON.stringify([
    document.newline,
    document.lines.slice(start, region.end),
    region.end === document.lines.length ? document.hasFinalNewline : true,
  ]);
}

// Existing seeds are owned by section. Restore only the matching admitted
// section; preserve every other byte, including sibling drafts and separators.
function withdrawnStory(current, published, original, href) {
  const selected = sectionOf(current, href);
  const accepted = sectionOf(published, href);
  if (!selected || selected.text !== accepted?.text) return undefined;
  const previous = sectionOf(original, href);
  if (!previous && sectionBoundary(selected) !== sectionBoundary(accepted))
    return undefined;
  const { document, region } = selected;
  const lines = [...document.lines];
  if (previous) {
    const count = selected.text.split("\n").length;
    lines.splice(region.start, count, ...previous.text.split("\n"));
  } else {
    let start = region.start;
    if (region.end === lines.length) {
      while (start > 0 && lines[start - 1] === "") start -= 1;
      const originalDocument = splitSource(original);
      const originalLines = originalDocument.lines;
      // Retain the original final separator when removing an appended section.
      lines.splice(
        start,
        region.end - start,
        ...originalLines.slice(
          originalLines.findLastIndex((line) => line !== "") + 1,
        ),
      );
      document.hasFinalNewline = originalDocument.hasFinalNewline;
    } else lines.splice(start, region.end - start);
  }
  return joinSource({ ...document, lines });
}

async function withdrawPublishedDrafts(request, sha) {
  const checkout = request.integration;
  const head = await draftCheckoutHead(checkout, request.target);
  if (!head) return;
  // A checkout that moved independently is not this admission's draft basis.
  try {
    await git(checkout, "merge-base", "--is-ancestor", head, sha);
  } catch {
    return;
  }
  const selection = selectedPreparation(request.repository, request.link);
  const home = await show(request.repository, sha, selection.homePath);
  if (home === null) return;
  const declared = selection.declaredPlan(selection.read(home).approach);
  const paths = [
    ...new Set([selection.homePath, declared.planPath].filter(Boolean)),
  ];
  for (const path of paths) {
    const published = await show(request.repository, sha, path);
    const parent = await show(request.repository, `${sha}^`, path);
    if (published === null || published === parent) continue;
    // Never unstage or rewrite a file with staged changes.
    if (await hasStagedChanges(checkout, path)) continue;
    const original = await show(checkout, head, path);
    const absolute = join(checkout, path);
    const treeMode = (
      await git(request.repository, "ls-tree", sha, "--", path)
    ).stdout.split(" ")[0];
    const publishedMode = Number.parseInt(treeMode.slice(-3), 8);
    if (fileMode(absolute) !== publishedMode) continue;
    const current = fileText(absolute);
    if (current === null || current === undefined || current === original)
      continue;
    const replacement =
      path === selection.homePath && original !== null
        ? withdrawnStory(current, published, original, request.link)
        : current === published
          ? original
          : undefined;
    if (replacement === undefined || replacement === current) continue;
    // Recheck after the asynchronous reads; a changed draft stays untouched.
    if ((await draftCheckoutHead(checkout, request.target)) !== head) return;
    if (await hasStagedChanges(checkout, path)) continue;
    if (fileText(absolute) !== current) continue;
    if (fileMode(absolute) !== publishedMode) continue;
    if (replacement === null) rmSync(absolute);
    else writeFileSync(absolute, replacement);
  }
}
