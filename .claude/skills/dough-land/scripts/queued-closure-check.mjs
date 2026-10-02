#!/usr/bin/env node
// Dough Land's fetched-target check: the candidate's removed queued entries
// identify the stories to protect, even without retained one-shot context.
import { resolve } from "node:path";
import { isDirectCliEntry } from "../../dough-execute-plan/scripts/ci-direct-entry.mjs";
import { show } from "../../dough-execute-plan/scripts/execution-source.mjs";
import { queuedOwnershipGuard } from "../../dough-execute-plan/scripts/one-shot-ownership.mjs";
import {
  git,
  originTrackingRef,
  revParse,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import { backlogPath } from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import {
  parseBacklog,
  queueHeading,
} from "../../dough-product-backlog/scripts/product-backlog-document.mjs";

async function entriesAt(checkout, revision) {
  const source = await show(checkout, revision, backlogPath);
  return source === null ? [] : parseBacklog(source).entries;
}

export async function checkQueuedClosures({ checkout, remote, targetRef }) {
  const tracking = originTrackingRef(targetRef, remote);
  await git(checkout, "fetch", remote);
  const candidate = await revParse(checkout, "HEAD");
  const remoteTip = await revParse(checkout, tracking);
  const base = (
    await git(checkout, "merge-base", candidate, remoteTip)
  ).stdout.trim();
  const before = (await entriesAt(checkout, base)).filter(
    (entry) => entry.list === queueHeading,
  );
  // Without a queued entry in the base the candidate cannot close one.
  if (before.length === 0) return { ok: true, status: "clear", closes: [] };
  const after = await entriesAt(checkout, candidate);
  const remaining = new Set(after.map((entry) => entry.identity));
  const closes = before
    .filter((entry) => !remaining.has(entry.identity))
    .map((entry) => entry.identity);
  for (const identity of closes) {
    const stop = await queuedOwnershipGuard({ workspace: checkout, identity })({
      candidate,
      remoteTip,
    });
    if (stop) return { ok: false, status: stop.status, ...stop.fields };
  }
  return { ok: true, status: "clear", closes };
}

const usage =
  "usage: queued-closure-check.mjs check --checkout PATH --remote NAME --target-ref refs/heads/BRANCH";
function argumentsOf(argv) {
  if (argv[0] !== "check") throw new Error(usage);
  const result = {};
  const flags = {
    "--checkout": "checkout",
    "--remote": "remote",
    "--target-ref": "targetRef",
  };
  for (let index = 1; index < argv.length; index += 2) {
    const key = flags[argv[index]];
    const value = argv[index + 1];
    if (!key || !value || value.startsWith("--") || result[key])
      throw new Error(usage);
    result[key] = value;
  }
  if (
    !result.checkout ||
    !result.remote ||
    !result.targetRef?.startsWith("refs/heads/") ||
    result.targetRef === "refs/heads/"
  )
    throw new Error(usage);
  return { ...result, checkout: resolve(result.checkout) };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  let args;
  try {
    args = argumentsOf(process.argv.slice(2));
  } catch (error) {
    process.stdout.write(
      `${JSON.stringify({ ok: false, status: "usage-error", error: error.message })}\n`,
    );
    process.exitCode = 2;
  }
  if (args) {
    let result;
    try {
      result = await checkQueuedClosures(args);
    } catch (error) {
      result = { ok: false, status: "check-failed", error: error.message };
    }
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  }
}
