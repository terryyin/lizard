// Portable session close record: one versioned JSON file per session UUID
// beside the configured backlog, carrying only accepted Git comparisons.
// publishSessionClose publishes the work through the shared publisher first;
// only an accepted result is recorded, then the record commit follows the same
// authorized route. The session UUID is the Dashboard launch's landing context
// `reference`, or for a native start the explicitly retained `sessionId`. Launch state, conversations, paths and native controls
// stay local and never enter the record.
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";
import { publishExecutionIncrement } from "./execution-increment-publication.mjs";
import { defaultBacklogPath } from "./owned-suffix-reconciliation.mjs";
import { git } from "./publication-git.mjs";
import { resolvePublicationLandingContext } from "./resolve-publication-landing-context.mjs";
import {
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
 * Ranges are ordered `{ base, candidate }` full commit pairs.
 * @param {{ sessionId: string, workflow: string, closedAt: string, story?: unknown, ranges: { base: string, candidate: string }[] }} close
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
    ranges: ranges.map(({ base, candidate }) => ({ base, candidate })),
  };
}

// The kept close identity, refused before any push when absent or conflicting.
async function closeIdentity(retained, landingContext) {
  const launched = landingContext
    ? JSON.parse(await readFile(landingContext, "utf8")).reference
    : undefined;
  if (launched && retained && launched !== retained) {
    return {
      status: "session-identity-conflict",
      error: `retained session ${retained} differs from launch reference ${launched}`,
    };
  }
  const sessionId = launched ?? retained;
  if (!sessionId) {
    return {
      status: "session-identity-missing",
      error:
        "no landing context reference or retained session id names this session",
    };
  }
  return { sessionId };
}

export async function publishSessionClose({
  session: { sessionId: retained, workflow, story },
  now = () => new Date(),
  backlogPath = defaultBacklogPath,
  ...request
}) {
  const landingContext = await resolvePublicationLandingContext({
    remote: "origin",
    ...request,
  });
  const identity = await closeIdentity(retained, landingContext);
  if (!identity.sessionId) {
    return {
      work: { ok: false, ...identity },
      record: { status: "not-published" },
    };
  }
  const { sessionId } = identity;
  const path = sessionRecordPath(sessionId, backlogPath);
  const publication = { ...request, backlogPath, landingContext };
  const work = await publishExecutionIncrement(publication);
  if (!work.ok) return { work, record: { status: "not-published" } };
  const record = sessionRecord({
    sessionId,
    workflow,
    closedAt: now().toISOString(),
    story,
    ranges: [{ base: work.suffixBase, candidate: work.receipt.sha }],
  });
  const { workspace } = publication;
  const file = join(workspace, path);
  await mkdir(dirname(file), { recursive: true });
  await writeFile(file, `${JSON.stringify(record, null, 2)}\n`);
  await git(workspace, "add", "--", path);
  await git(
    workspace,
    "commit",
    "-m",
    `Record session ${sessionId}`,
    "--",
    path,
  );
  const published = await publishExecutionIncrement({
    ...publication,
    previouslyPublishedBase: work.receipt.sha,
    validatedCandidate: undefined,
    landingContext: undefined,
  });
  return {
    work,
    record: {
      status: published.ok ? "published" : "stopped",
      path,
      record,
      publication: published,
    },
  };
}
