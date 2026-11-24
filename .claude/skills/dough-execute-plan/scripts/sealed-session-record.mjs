// A session close record sealed from accepted work, and its publication once:
// one versioned JSON file per session UUID beside the configured backlog,
// carrying only accepted Git comparisons. Sealing fixes the UUID, close time and
// comparisons once the work is accepted; publishing a sealed record settles on a
// previously published close for its Done story, or reports competing facts,
// and otherwise sends one record commit through the authorized route.
import { mkdir, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";
import { rebuildDoneCatalog } from "../../dough-product-backlog/scripts/product-backlog-complete.mjs";
import {
  doneRecordPath,
  renderDoneRecord,
} from "../../dough-product-backlog/scripts/product-backlog-done-record.mjs";
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import { publishExecutionIncrement } from "./execution-increment-publication.mjs";
import { git, originTrackingRef, revParse } from "./publication-git.mjs";
import { expiredSessionRecordPaths } from "./session-record-pruning.mjs";
import { rangeUncertainty } from "./shared-checkout-note.mjs";
import {
  adHocCloseTitle,
  adHocStoryIdentity,
  isOriginatingAdHocClose,
  isSessionId,
  matchingOriginatingAdHocClose,
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
 * @param {{ sessionId: string, workflow: string, closedAt: string, story?: unknown, title?: string, ranges: { base: string, candidate: string, uncertainty?: string }[] }} close
 */
export function sessionRecord({
  sessionId,
  workflow,
  closedAt,
  story,
  title,
  ranges,
}) {
  return {
    version: sessionRecordVersion,
    sessionId,
    workflow,
    closedAt,
    ...(story ? { story } : {}),
    ...(title ? { title } : {}),
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
  { sessionId, workflow, story, title, closedAt, sharedCheckout },
  { suffixBase, sha },
) {
  const doneTitle = story ? undefined : adHocCloseTitle(workflow, title);
  return sessionRecord({
    sessionId,
    workflow,
    closedAt,
    story: doneTitle ? adHocStoryIdentity(sessionId) : story,
    title: doneTitle,
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

async function settledRecordAt(workspace, revision, path, record) {
  if (!revision) return undefined;
  const found = await git(workspace, "show", `${revision}:${path}`).catch(
    () => undefined,
  );
  if (!found) return undefined;
  const accepted =
    found.stdout === recordBytes(record)
      ? record
      : matchingOriginatingAdHocClose(found.stdout, record);
  return {
    status: accepted ? "published" : "session-record-conflict",
    path,
    record: accepted ?? record,
    publication: { ok: accepted !== undefined, pushCount: 0 },
  };
}

async function writeCloseFiles(workspace, backlogPath, path, record) {
  const file = join(workspace, path);
  await mkdir(dirname(file), { recursive: true });
  await writeFile(file, recordBytes(record));
  if (!isOriginatingAdHocClose(record)) return [path];
  const directory = dirname(backlogPath);
  const donePath = join(directory, doneRecordPath(record.story));
  const doneFile = join(workspace, donePath);
  await mkdir(dirname(doneFile), { recursive: true });
  await writeFile(
    doneFile,
    renderDoneRecord({
      identity: record.story,
      title: record.title,
      completedAt: record.closedAt,
    }),
  );
  const catalog = rebuildDoneCatalog(join(workspace, directory));
  return [path, donePath, join(directory, catalog.path)];
}

// Commits the record once; a rerun finds it already committed. The same commit,
// and only it, removes the expired records beside it at `now`.
async function commitRecord({ workspace, backlogPath, now }, path, record) {
  const files = await writeCloseFiles(workspace, backlogPath, path, record);
  // The close is published content even under an ignored project directory.
  await git(workspace, "add", "--force", "--", ...files);
  const pending = (
    await git(workspace, "status", "--porcelain", "--", ...files)
  ).stdout;
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
    ...files,
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
  const { workspace, remote, targetRef, onFetchedTarget } = publication;
  await git(workspace, "fetch", remote);
  const settled = await settledRecordAt(
    workspace,
    originTrackingRef(targetRef, remote),
    path,
    record,
  );
  if (settled) return settled;
  await commitRecord({ ...publication, now }, path, record);
  const { stdout } = await git(
    workspace,
    "log",
    "-1",
    "--format=%H %P",
    "--",
    path,
  );
  const [closeCommit, recordBase] = stdout.trim().split(" ");
  const tip = await revParse(workspace, publication.branch);
  // Only the exact validated replay can add a catalog commit above the close.
  if (
    tip !== closeCommit &&
    (tip !== publication.validatedCandidate ||
      (await revParse(workspace, `${tip}^`)) !== closeCommit)
  ) {
    throw new Error(
      "The workspace has unvalidated commits above the session close.",
    );
  }
  /** @type {Awaited<ReturnType<typeof settledRecordAt>>} */
  let settlement;
  const published = await publishExecutionIncrement({
    ...publication,
    previouslyPublishedBase: recordBase,
    onFetchedTarget: async (observation) => {
      settlement = await settledRecordAt(
        workspace,
        observation.remoteTip,
        path,
        record,
      );
      return settlement
        ? { status: settlement.status }
        : onFetchedTarget?.(observation);
    },
    // A record commit is never the launch's landing, even while its landing
    // expectation is still open.
    landingContext: null,
  });
  if (settlement) return settlement;
  return {
    status: published.ok ? "published" : "stopped",
    path,
    record,
    publication: published,
  };
}
