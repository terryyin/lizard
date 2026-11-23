// A workflow session's local evidence: one per-worktree file at the Git
// path `open-dough/session-record` holding the session UUID, workflow, story
// and the ordered accepted increment pairs. A fresh native claim, admission or
// preparation announcement starts a new UUID; a resumed start keeps the file.
// A Dashboard launch's landing-context reference names the session, and a
// different reference replaces an unclosed earlier session's evidence, except
// where Story Branch integration adopts the launch reference. Only accepted
// pairs are appended, so refused or rewritten candidates never enter.
import { randomUUID } from "node:crypto";
import { readGitPathJson, replaceGitPathJson } from "./git-path-json-store.mjs";
import { executionSessionWorkflow } from "./session-record.mjs";

const sessionEvidencePath = "open-dough/session-record";

export function readSessionEvidence(workspace) {
  return readGitPathJson(workspace, sessionEvidencePath);
}

function writeSessionEvidence(workspace, evidence) {
  return replaceGitPathJson(workspace, sessionEvidencePath, evidence);
}

function freshSession(sessionId, story, workflow = executionSessionWorkflow) {
  return {
    sessionId,
    workflow,
    ...(story ? { story } : {}),
    ranges: [],
  };
}

// A fresh native claim, admission or preparation announcement: a new UUID
// with no ranges yet, under the starting workflow.
export function beginNativeSession(workspace, { story, workflow } = {}) {
  return writeSessionEvidence(
    workspace,
    freshSession(randomUUID(), story, workflow),
  );
}

/**
 * Appends one accepted increment's exact pair. `reference` is the Dashboard
 * launch's landing-context reference when this publication carried one.
 * @param {string} workspace
 * @param {{ base: string, candidate: string, reference?: string }} accepted
 */
export function recordAcceptedIncrement(
  workspace,
  { base, candidate, reference },
) {
  const kept = readSessionEvidence(workspace);
  const evidence =
    reference && kept?.sessionId !== reference
      ? freshSession(reference, kept?.story)
      : (kept ?? freshSession(randomUUID()));
  const last = evidence.ranges.at(-1);
  if (last?.base !== base || last?.candidate !== candidate)
    evidence.ranges.push({ base, candidate });
  return writeSessionEvidence(workspace, evidence);
}

// Closes the native session `sessionId` at the close time its sealed record
// carries, so a later close in this workspace cannot reuse its identity.
export function closeNativeSession(workspace, { sessionId, closedAt }) {
  const kept = readSessionEvidence(workspace);
  if (kept?.sessionId !== sessionId || kept.closedAt) return kept;
  return writeSessionEvidence(workspace, { ...kept, closedAt });
}

// A Story Branch session's branch publications carry no landing context, so
// its evidence starts native; the integration's launch reference then names
// the same session, keeping its recorded ranges and story. A closed session
// keeps its sealed identity.
export function adoptLaunchReference(workspace, reference) {
  const kept = readSessionEvidence(workspace);
  if (!kept || kept.closedAt || kept.sessionId === reference) return kept;
  return writeSessionEvidence(workspace, { ...kept, sessionId: reference });
}

/**
 * Seals the session at its close: appends the accepted closure pair unless
 * delivery already recorded it, keeps `closing` (what the close already
 * settled, such as its completion receipt), and fixes the close time before
 * the first record publication attempt, so a rerun keeps the original close
 * time and ranges and resumes after what `closing` holds.
 * @param {string} workspace
 * @param {{ base: string, candidate: string }} closure
 * @param {object} closing
 * @param {() => Date} now
 */
export function sealSessionEvidence(
  workspace,
  closure,
  closing,
  now = () => new Date(),
) {
  const kept = readSessionEvidence(workspace);
  if (kept?.closedAt) return kept;
  const held = kept?.ranges.some(
    (range) => range.candidate === closure.candidate,
  );
  const evidence = held ? kept : recordAcceptedIncrement(workspace, closure);
  evidence.closing = closing;
  evidence.closedAt = now().toISOString();
  return writeSessionEvidence(workspace, evidence);
}
