// A session close record sealed from accepted work, and its publication once:
// one versioned JSON file per session UUID beside the configured backlog,
// carrying only accepted Git comparisons. Sealing fixes the UUID, close time and
// comparisons once the work is accepted; publishing a sealed record settles on an
// identical published record, reports a different one at the same UUID as a
// conflict, and otherwise sends one record commit the authorized route.
import { mkdir, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import { publishExecutionIncrement } from "./execution-increment-publication.mjs";
import {
  git,
  observePublicationTarget,
  originTrackingRef,
} from "./publication-git.mjs";
import { expiredSessionRecordPaths } from "./session-record-pruning.mjs";
import { rangeUncertainty } from "./shared-checkout-note.mjs";
import {
  hasSessionStory,
  isSessionId,
  sessionRecordPathBeside,
  sessionRecordVersion,
} from "./session-record.mjs";

export function sessionRecordPath(sessionId, backlogPath = defaultBacklogPath) {
  if (!isSessionId(sessionId)) {
    throw new Error(`session id ${JSON.stringify(sessionId)} is not a UUID`);
  }
  return sessionRecordPathBeside(backlogPath, sessionId);
}

/**
 * Ranges are ordered `{ base, candidate }` full commit pairs, each with an
 * optional `uncertainty` mark.
 * @param {{ sessionId: string, workflow: string, closedAt: string, story?: unknown, ranges: { base: string, candidate: string, uncertainty?: string }[] }} close
 */
export function sessionRecord({
  sessionId,
  workflow,
  closedAt,
  story,
  ranges,
}) {
  return {
    version: sessionRecordVersion,
    sessionId,
    workflow,
    closedAt,
    ...(story ? { story } : {}),
    ranges: ranges.map(({ base, candidate, uncertainty }) => ({
      base,
      candidate,
      ...(uncertainty ? { uncertainty } : {}),
    })),
  };
}

// Seals the record from the accepted work, marking its range when a
// shared-checkout note's overlap started at or before the acceptance.
export function sealRecord(
  { sessionId, workflow, story, closedAt, sharedCheckout },
  { suffixBase, sha },
) {
  return sessionRecord({
    sessionId,
    workflow,
    closedAt,
    story,
    ranges: [
      {
        base: suffixBase,
        candidate: sha,
        uncertainty: rangeUncertainty(sharedCheckout, closedAt),
      },
    ],
  });
}

const recordBytes = (record) => `${JSON.stringify(record, null, 2)}\n`;

async function publishedBytes({ workspace, remote, targetRef }, path) {
  const tip = await observePublicationTarget(
    workspace,
    remote,
    originTrackingRef(targetRef, remote),
  );
  if (!tip) return undefined;
  try {
    return (await git(workspace, "show", `${tip}:${path}`)).stdout;
  } catch {
    return undefined;
  }
}

// Commits the record once; a rerun finds it already committed. The same commit,
// and only it, removes the expired records beside it at `now`.
async function commitRecord({ workspace, backlogPath, now }, path, record) {
  const file = join(workspace, path);
  await mkdir(dirname(file), { recursive: true });
  await writeFile(file, recordBytes(record));
  // The record is published content even where the project's own directory
  // is ignored, as for a project below an ignoring Git toplevel.
  await git(workspace, "add", "--force", "--", path);
  const pending = (await git(workspace, "status", "--porcelain", "--", path))
    .stdout;
  if (!pending.trim()) return;
  const expired = await expiredSessionRecordPaths(
    workspace,
    backlogPath,
    now(),
  );
  if (expired.length)
    await git(workspace, "rm", "--quiet", "--ignore-unmatch", "--", ...expired);
  await git(
    workspace,
    "commit",
    "-m",
    `Record session ${record.sessionId}`,
    "--",
    path,
    ...expired,
  );
}

// Publishes a sealed record after its work was accepted. A commit or
// publication that throws stops the record stage with the sealed record kept,
// so the close can resume. `now` dates the expiry of the records it prunes.
export async function publishSealedRecord({
  record,
  now = () => new Date(),
  remote = "origin",
  backlogPath = defaultBacklogPath,
  ...request
}) {
  const path = sessionRecordPath(record.sessionId, backlogPath);
  const publication = { ...request, remote, backlogPath };
  try {
    return await sendRecord(publication, path, record, now);
  } catch (error) {
    return {
      status: "stopped",
      path,
      record,
      error: error.message,
      ...(error.code ? { code: error.code, stage: error.stage } : {}),
    };
  }
}

async function sendRecord(publication, path, record, now) {
  const accepted = await publishedBytes(publication, path);
  if (accepted !== undefined) {
    const identical = accepted === recordBytes(record);
    return {
      status: identical ? "published" : "session-record-conflict",
      path,
      record,
      publication: { ok: identical, pushCount: 0 },
    };
  }
  // A Dashboard ad hoc close may resume a published historical record, but
  // cannot publish a new record without the story that owns its session.
  if (record.workflow === "ad-hoc" && !hasSessionStory(record.story))
    return {
      status: "stopped",
      path,
      record,
      error: "no story identity is bound to this session",
    };
  await commitRecord({ ...publication, now }, path, record);
  // The record commit's parent is the accepted tip it was committed or
  // last rebased onto: the session's last range, or the integrated trunk.
  const recordBase = (
    await git(publication.workspace, "rev-parse", "HEAD^")
  ).stdout.trim();
  const published = await publishExecutionIncrement({
    ...publication,
    previouslyPublishedBase: recordBase,
    // A record commit is never the launch's landing, even while its landing
    // expectation is still open.
    landingContext: null,
  });
  return {
    status: published.ok ? "published" : "stopped",
    path,
    record,
    publication: published,
  };
}
