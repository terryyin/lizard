import { existsSync } from "node:fs";
import { join } from "node:path";
import { isFullGitRevision } from "./ci-revisions.mjs";
import { publishJson } from "./ci-mailbox-json-file.mjs";
import {
  attemptState,
  classifyUndiscoveredRevision,
  findAncestorRun,
  observedRevision,
  preferredAttempt,
} from "./ci-revision-applicability-classification.mjs";
import {
  candidateLookupUnavailable,
  integratedStateJudge,
} from "./ci-integrated-state.mjs";
import { queueProgressReader } from "./ci-mailbox-queue-progress.mjs";
import {
  effectiveRevisionState,
  freshPendingRevision,
  publishRevisionRecord,
  readRevisionCoverage,
} from "./ci-mailbox-revision-records.mjs";

// States that, once recorded, are not reclassified on a later poll unless an
// exact attempt for that revision's own SHA appears (checked first, always).
// Inherited coverage is reclassified: a nearer ancestor may appear, or its
// retained attempt may disappear. Only exact verdicts remain terminal.
const terminalRevisionStates = ["success", "failure", "incomplete"];

export const discoveryDelayBoundMs = 10 * 60 * 1000;
export const discoveryAdvisoryMarkerName = "discovery-advisory.json";

function discoveryAdvisoryMarkerPath(directory) {
  return join(directory, discoveryAdvisoryMarkerName);
}

export function discoveryAdvisoryEmitted(directory) {
  return existsSync(discoveryAdvisoryMarkerPath(directory));
}

// `discoverAncestorCandidates`, when supplied, returns run-shaped candidates
// (see ci-runs.mjs's discoverApplicabilityCandidateRuns), not bare SHAs: this
// function derives classification's SHA list from them, and separately keeps
// the run-shaped list itself so a `not_required` basis can be resolved to its
// actual attempt for failure inspection below — the ancestor A is frequently
// not registered in this mailbox at all (see resolveAncestorRun).
//
// Under a declared `integratedPolicy`, each eligible revision also records
// `integrated`: its descendant-candidate judgement (ci-integrated-state.mjs)
// from this poll's runs, the same bounded history lookup and
// `readCurrentAttempt` under the observation's `signal`. It is replaced every
// poll, never retained.
export async function observeRevisionCoverage(
  directory,
  runs,
  request,
  observedAt = Date.now(),
  {
    repoDir = request.root,
    event = "push",
    workflowPath,
    discoverAncestorCandidates,
    discoverQueue,
    readCurrentAttempt,
    integratedPolicy,
    signal,
  } = {},
) {
  const events = [];
  const queue = (await discoverQueue?.()) ?? [];
  const queueProgress = queueProgressReader(queue, {
    branch: request.branch,
    integratedPolicy,
    repoDir,
    observedAt,
  });
  const localCandidateShas = [
    ...new Set(
      runs
        .map(({ headSha }) => headSha?.toLowerCase())
        .filter((sha) => isFullGitRevision(sha)),
    ),
  ];
  let broadenedCandidateRuns;
  const getBroadenedCandidateRuns = discoverAncestorCandidates
    ? async () => {
        broadenedCandidateRuns ??= (await discoverAncestorCandidates()) ?? [];
        return broadenedCandidateRuns;
      }
    : undefined;
  const getBroadenedCandidateShas = discoverAncestorCandidates
    ? async () => [
        ...new Set(
          (await getBroadenedCandidateRuns())
            .map((run) => run.headSha?.toLowerCase())
            .filter((sha) => isFullGitRevision(sha)),
        ),
      ]
    : undefined;

  // A `not_required` revision's own coverage record intentionally carries
  // only its basis SHA (see registerPushedRevision/the not_required branch
  // below) — its ancestor A is ordinary branch history, usually never
  // registered in this mailbox itself. To keep A's eventual failure
  // actionable for every ignored-only descendant that shares it, resolve A's
  // real run object (local `runs` first, then the broadened/bounded-history
  // list) once per poll and hand the deduplicated result back to the caller,
  // which feeds it into the same failure-acquisition call used for ordinary
  // registered revisions — reusing that call's existing per-run/job dedup so
  // a shared failure is still delivered exactly once.
  //
  // The same resolved run also lets this poll record A's real lifecycle
  // state (pending/success/failure/incomplete, via attemptState) onto the
  // not_required revision's own `basis.state` below, so a shutdown/terminal
  // report reading only the on-disk coverage record (no live `gh` call, see
  // ci-mailbox-store.mjs) can tell a still-pending applicable ancestor apart
  // from a proved terminal one without ever inventing a verdict for the
  // registered revision itself.
  const ancestorRunsBySha = new Map();
  async function resolveAncestorRun(sha) {
    if (!sha) return undefined;
    if (ancestorRunsBySha.has(sha)) return ancestorRunsBySha.get(sha);
    const found = await findAncestorRun({
      sha,
      runs,
      getBroadenedCandidateRuns,
    });
    if (found) ancestorRunsBySha.set(sha, found);
    return found;
  }
  const judgeIntegratedState = integratedStateJudge(integratedPolicy, {
    repo: request.repo,
    remote: request.remote,
    branch: request.branch,
    repoDir,
    listCandidates: async () => [
      ...runs,
      ...((await getBroadenedCandidateRuns?.()) ?? []),
    ],
    readCurrentAttempt,
    signal,
  });

  for (const revision of readRevisionCoverage(directory)) {
    const matches = runs.filter(
      ({ headSha }) => headSha?.toLowerCase() === revision.sha,
    );
    const registeredAt = revision.registeredAt ?? observedAt;
    let next;
    const attempt = preferredAttempt(matches);
    if (attempt) {
      next = observedRevision({ ...revision, registeredAt }, attempt);
    } else if (terminalRevisionStates.includes(revision.state)) {
      next = revision;
    } else if (repoDir) {
      const classification = await classifyUndiscoveredRevision({
        repoDir,
        event,
        workflowPath,
        registeredSha: revision.sha,
        localCandidateShas,
        getBroadenedCandidateShas,
      });
      next =
        classification.result === "not_required"
          ? {
              sha: revision.sha,
              state: "not_required",
              basis: classification.basis,
              registeredAt,
            }
          : { sha: revision.sha, state: "undiscovered", registeredAt };
    } else {
      next = {
        sha: revision.sha,
        state: "undiscovered",
        registeredAt,
      };
    }
    if (next.state === "not_required") {
      const ancestorRun = await resolveAncestorRun(next.basis?.sha);
      // Only this poll's resolved attempt can support a completion verdict.
      next = {
        ...next,
        basis: {
          sha: next.basis.sha,
          ...(ancestorRun ? { state: attemptState(ancestorRun) } : {}),
        },
      };
    }
    next = await judgeIntegratedState(next);
    // Every successful poll replaces progress, including unsupported/empty
    // queue snapshots, and renews only actual attempts observed this poll.
    delete next.queue;
    delete next.observedAt;
    if (effectiveRevisionState(next) === "pending")
      next.observedAt = observedAt;
    const progress = await queueProgress(next);
    if (progress) next.queue = progress;
    publishRevisionRecord(directory, next);
  }

  if (!discoveryAdvisoryEmitted(directory)) {
    const undiscovered = readRevisionCoverage(directory).filter(
      (revision) =>
        revision.state === "undiscovered" &&
        !freshPendingRevision(revision, observedAt),
    );
    const overdue = undiscovered.some(
      ({ registeredAt }) =>
        registeredAt !== undefined &&
        observedAt - registeredAt > discoveryDelayBoundMs,
    );
    if (overdue) {
      events.push({
        type: "CI_DISCOVERY_DELAYED",
        repo: request.repo,
        branch: request.branch,
        revisions: undiscovered.map(({ sha }) => sha).sort(),
      });
      publishJson(directory, discoveryAdvisoryMarkerName, { emitted: true });
    }
  }
  return { events, ancestorRuns: [...ancestorRunsBySha.values()] };
}

// A failed discovery withdraws pending progress and integrated-state evidence.
// Actual attempt states remain recorded, but cannot explain fresh progress
// until a later successful poll observes them again.
export function withdrawIntegratedState(directory, error) {
  for (const revision of readRevisionCoverage(directory)) {
    const next = { ...revision };
    delete next.queue;
    delete next.observedAt;
    if (revision.integrated)
      next.integrated = candidateLookupUnavailable(
        revision.integrated.policy,
        error,
      );
    publishRevisionRecord(directory, next);
  }
}
