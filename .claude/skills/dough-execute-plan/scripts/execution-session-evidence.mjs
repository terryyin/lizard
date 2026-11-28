// A workflow session's local evidence: one per-worktree file at the Git
// path `open-dough/session-record` holding the session UUID, workflow, story
// and the ordered accepted increment pairs. A fresh native claim, admission or
// preparation announcement starts a new UUID; a resumed start keeps the file.
// An accepted pair with a bound Dashboard launch reference is also retained
// under that reference, so another session in the checkout can replace the
// current record without losing the earlier launch's comparison.
import { randomUUID } from "node:crypto";
import { readGitPathJson, replaceGitPathJson } from "./git-path-json-store.mjs";
import {
  executionSessionWorkflow,
  hasSessionStory,
  isSessionId,
} from "./session-record.mjs";

const sessionEvidencePath = "open-dough/session-record";

export function readSessionEvidence(workspace) {
  return readGitPathJson(workspace, sessionEvidencePath);
}

const acceptedLaunchPath = (reference) =>
  `open-dough/accepted-launches/${reference}.json`;

export function readAcceptedLaunchEvidence(workspace, reference) {
  if (isSessionId(reference))
    return readGitPathJson(workspace, acceptedLaunchPath(reference));
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

// Adopt already verified ranges from a borrowed native checkout.
export function retainAdmittedSession(workspace, { sessionId, story, ranges }) {
  return writeSessionEvidence(workspace, {
    ...freshSession(sessionId, story),
    ranges,
  });
}

/**
 * Appends one accepted increment's exact pair. `reference` is the Dashboard
 * launch's landing-context reference when this publication carried one.
 * @param {string} workspace
 * @param {{ base: string, candidate: string, reference?: string, story?: string, uncertainty?: string }} accepted
 */
export function recordAcceptedIncrement(
  workspace,
  { base, candidate, reference, story, uncertainty },
) {
  const range = { base, candidate, ...(uncertainty && { uncertainty }) };
  if (isSessionId(reference) && hasSessionStory(story)) {
    const launch =
      readAcceptedLaunchEvidence(workspace, reference) ??
      freshSession(reference, story);
    if (launch.sessionId === reference && launch.story === story) {
      if (!launch.ranges.some((prior) => sameAcceptedPair(prior, range)))
        launch.ranges.push(range);
      replaceGitPathJson(workspace, acceptedLaunchPath(reference), launch);
    }
  }
  const kept = readSessionEvidence(workspace);
  const evidence =
    reference && kept?.sessionId !== reference
      ? freshSession(reference, story ?? kept?.story)
      : (kept ?? freshSession(randomUUID()));
  if (!sameAcceptedPair(evidence.ranges.at(-1), range))
    evidence.ranges.push(range);
  return writeSessionEvidence(workspace, evidence);
}

function sameAcceptedPair(prior, range) {
  return prior?.base === range.base && prior?.candidate === range.candidate;
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
