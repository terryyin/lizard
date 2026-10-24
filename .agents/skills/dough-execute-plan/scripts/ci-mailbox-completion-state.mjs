import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { readMailbox } from "./ci-mailbox-location.mjs";
import { publishJsonExclusively } from "./ci-mailbox-json-file.mjs";
import { isFullGitRevision } from "./ci-revisions.mjs";

export const completionBudgetMs = 60 * 60 * 1000;
const shellQuote = (value) => `'${value.replaceAll("'", "'\\''")}'`;
const read = (path) => JSON.parse(readFileSync(path, "utf8"));

// Freshness describes current progress, not the identity of a proved CI result.
// Keep all applicable run/attempt, SHA, state, policy and ancestry evidence.
function successfulEvidenceIdentity({ revision, accepted, ...evidence }) {
  const facts = (record) => {
    const value = { ...record };
    delete value.observedAt;
    delete value.queue;
    return value;
  };
  return {
    ...evidence,
    ...(revision ? { revision: facts(revision) } : {}),
    ...(accepted ? { accepted: facts(accepted) } : {}),
  };
}

// The exclusive first record survives caller exit and cannot be renewed by a
// resume, concurrent completion, or a coalesced descendant's progress.
export function completionState(directory, sha, options = {}) {
  if (!isFullGitRevision(sha))
    throw new Error("Expected a full Git revision SHA");
  const request = readMailbox(directory, options.root, options.storage);
  const normalized = sha.toLowerCase();
  const records = join(directory, "completion");
  mkdirSync(records, { recursive: true, mode: 0o700 });
  const record = join(records, `${normalized}.json`);
  const startedAt = (options.now ?? Date.now)();
  const launcher = join(
    dirname(fileURLToPath(import.meta.url)),
    "ci-mailbox.mjs",
  );
  publishJsonExclusively(records, `${normalized}.json`, {
    requestedSha: normalized,
    directory,
    target: { repo: request.repo, branch: request.branch },
    pendingReceipt: join(records, `${normalized}.pending.json`),
    startedAt,
    deadline: startedAt + completionBudgetMs,
    record,
    command: `${shellQuote(process.execPath)} ${shellQuote(launcher)} complete-revision ${shellQuote(directory)} ${normalized}`,
  });
  const handoff = read(record);
  const name = (suffix) => `${normalized}.${suffix}.json`;
  const existing = (suffix) => {
    const path = join(records, name(suffix));
    return existsSync(path) ? read(path) : undefined;
  };
  const successName = (evidence) =>
    `success-${createHash("sha256")
      .update(JSON.stringify(successfulEvidenceIdentity(evidence)))
      .digest("hex")}`;
  return {
    handoff,
    expired: () => existing("expired"),
    expire: () =>
      publishJsonExclusively(records, name("expired"), {
        ...handoff,
        closedAt: (options.now ?? Date.now)(),
      }),
    pending: (receipt) =>
      publishJsonExclusively(records, name("pending"), receipt),
    success: (evidence) => existing(successName(evidence)),
    succeed: (receipt) =>
      publishJsonExclusively(
        records,
        name(successName(receipt.effectiveEvidence)),
        receipt,
      ),
  };
}

export function completionExpired(awaited) {
  const unresolved = { ...awaited };
  delete unresolved.verdict;
  const observation =
    awaited.unresolvedReason &&
    !["timeout", "wait_cancelled"].includes(awaited.unresolvedReason)
      ? {
          unresolvedReason: awaited.unresolvedReason,
          ...(awaited.detail ? { detail: awaited.detail } : {}),
        }
      : undefined;
  return {
    ...unresolved,
    ...(observation ? { observation } : {}),
    unresolvedReason: "completion_expired",
    detail: "overall_completion_deadline",
  };
}
