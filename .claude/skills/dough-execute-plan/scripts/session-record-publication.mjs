// Publish the work, then its sealed session record. Keep the exact comparison
// and record locally per session so a close can resume at either step.
import { defaultBacklogPath } from "../../dough-product-backlog/scripts/product-backlog-store.mjs";
import { publishExecutionIncrement } from "./execution-increment-publication.mjs";
import { closeNativeSession } from "./execution-session-evidence.mjs";
import {
  readGitPathJson,
  removeGitPathJson,
  replaceGitPathJson,
} from "./git-path-json-store.mjs";
import { resumeInterruptedPublication } from "./publication-resume.mjs";
import {
  publishSealedRecord,
  sealRecord,
  sessionRecord,
  sessionRecordPath,
} from "./sealed-session-record.mjs";
import { closeTarget } from "./session-close-identity.mjs";

export { sessionRecord, sessionRecordPath };

const closeStatePath = (sessionId) =>
  `open-dough/session-close/${sessionId}.json`;

function saveCloseState(workspace, sessionId, facts) {
  const path = closeStatePath(sessionId);
  replaceGitPathJson(workspace, path, {
    ...readGitPathJson(workspace, path),
    ...facts,
  });
}

function sealClose(target, close, accepted, publication) {
  if (target.native)
    closeNativeSession(publication.workspace, {
      sessionId: target.sessionId,
      closedAt: close.closedAt,
    });
  const record = sealRecord(
    {
      sessionId: target.sessionId,
      ...close,
      story: target.story,
      title: target.title,
      sharedCheckout: target.sharedCheckout,
    },
    accepted,
  );
  saveCloseState(publication.workspace, target.sessionId, { record });
  return publishRecord(record, {
    ...publication,
    validatedCandidate: undefined,
  });
}

// Publishes a sealed record and retires its session's saved close state once
// published.
async function publishRecord(record, request) {
  const result = await publishSealedRecord({ ...request, record });
  if (result.status === "published")
    removeGitPathJson(request.workspace, closeStatePath(record.sessionId));
  return result;
}

export async function publishSessionClose({
  session,
  now = () => new Date(),
  backlogPath = defaultBacklogPath,
  ...request
}) {
  const target = await closeTarget({
    session,
    backlogPath,
    ...request,
  });
  if (target.refused) return target.refused;
  const { landingContext } = target;
  const publication = { ...request, backlogPath, landingContext };
  const work = await publishExecutionIncrement({
    ...publication,
    beforePush: async (comparison) => {
      const { candidate, suffixBase } = comparison;
      saveCloseState(request.workspace, target.sessionId, {
        candidate,
        suffixBase,
        title: target.title,
      });
      await request.beforePush?.(comparison);
    },
  });
  if (!work.ok) return { work, record: { status: "not-published" } };
  return {
    work,
    record: await sealClose(
      target,
      { workflow: session.workflow, closedAt: now().toISOString() },
      { suffixBase: work.suffixBase, sha: work.receipt.sha },
      { ...publication, now },
    ),
  };
}

/**
 * Resumes a close from its first unfinished step without publishing the work
 * twice. With the sealed `record`, only the record stage runs. Without one
 * (the work's result was lost before sealing), `candidateSha` and
 * `suffixBase` confirm the work through interrupted publication recovery, then
 * the record is sealed and published. Whatever the caller does not supply
 * comes from the state the session's close saved in this workspace, found by
 * the `session` identity or the landing context's reference. A record
 * commit rebased onto an advanced target stops as `needs-validation` like any
 * publication; rerun with `validatedCandidate` set to its candidate.
 * @param {{ record?: ReturnType<typeof sessionRecord>, session?: { sessionId?: string, workflow: string, story?: unknown, title?: string }, candidateSha?: string, suffixBase?: string, now?: () => Date, workspace: string, branch: string, targetRef: string, remote?: string, backlogPath?: string, validatedCandidate?: string, landingContext?: string }} request
 */
export async function resumeSessionClose({
  record,
  session,
  candidateSha,
  suffixBase,
  now = () => new Date(),
  ...request
}) {
  if (record)
    return {
      record: await publishRecord(sessionRecord(record), { ...request, now }),
    };
  const { remote = "origin", backlogPath = defaultBacklogPath } = request;
  const target = await closeTarget({
    session,
    ...request,
    backlogPath,
  });
  if (target.refused) return target.refused;
  const { landingContext } = target;
  const saved =
    readGitPathJson(request.workspace, closeStatePath(target.sessionId)) ?? {};
  if (saved.record && !candidateSha)
    return {
      record: await publishRecord(sessionRecord(saved.record), {
        ...request,
        now,
      }),
    };
  const work = await resumeInterruptedPublication({
    ownedWorkspace: request.workspace,
    candidateSha: candidateSha ?? saved.candidate,
    suffixBase: candidateSha ? suffixBase : saved.suffixBase,
    remote,
    targetRef: request.targetRef,
    branch: request.branch,
    landingContext,
    backlogPath,
  });
  if (work.held) return { work, record: { status: "not-published" } };
  return {
    work,
    record: await sealClose(
      { ...target, title: saved.title ?? target.title },
      { workflow: session.workflow, closedAt: now().toISOString() },
      { suffixBase: work.suffixBase, sha: work.acceptedSha },
      { ...request, remote, backlogPath, landingContext, now },
    ),
  };
}
