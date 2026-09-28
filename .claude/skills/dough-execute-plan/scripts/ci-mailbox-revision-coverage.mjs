import { existsSync, mkdirSync, readFileSync, readdirSync } from "node:fs";
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

// States that, once recorded, are not reclassified on a later poll unless an
// exact attempt for that revision's own SHA appears (checked first, always).
// `not_required` joins the existing terminal verdicts here: its basis is a
// proved fact about the registered revision's own tree, not something a
// later poll needs to keep re-deriving.
const terminalRevisionStates = [
  "success",
  "failure",
  "incomplete",
  "not_required",
];

export const discoveryDelayBoundMs = 10 * 60 * 1000;
export const discoveryAdvisoryMarkerName = "discovery-advisory.json";

const revisionDirectory = (directory) => join(directory, "coverage");

function revisionPath(directory, sha) {
  if (!isFullGitRevision(sha))
    throw new Error("Expected a full Git revision SHA");
  return join(revisionDirectory(directory), `${sha.toLowerCase()}.json`);
}

export function registerPushedRevision(directory, sha) {
  const path = revisionPath(directory, sha);
  const normalized = sha.toLowerCase();
  mkdirSync(revisionDirectory(directory), { recursive: true, mode: 0o700 });
  if (!existsSync(path))
    publishJson(revisionDirectory(directory), `${normalized}.json`, {
      sha: normalized,
      state: "undiscovered",
    });
  return readRevisionCoverage(directory).find(
    (revision) => revision.sha === normalized,
  );
}

function revisionRecordNames(directory) {
  const coverage = revisionDirectory(directory);
  if (!existsSync(coverage)) return [];
  return readdirSync(coverage)
    .filter(
      (name) =>
        name.toLowerCase().endsWith(".json") &&
        isFullGitRevision(name.slice(0, -5)),
    )
    .sort();
}

// Registered SHAs from record names alone, cheap enough to check often.
export function listRegisteredRevisions(directory) {
  return revisionRecordNames(directory).map((name) => name.slice(0, -5));
}

export function readRevisionCoverage(directory) {
  const coverage = revisionDirectory(directory);
  return revisionRecordNames(directory).map((name) =>
    JSON.parse(readFileSync(join(coverage, name), "utf8")),
  );
}

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
  } = {},
) {
  const events = [];
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
      // Refresh the basis's resolved state every poll (never sticky): an
      // ancestor found pending on an earlier poll and now terminal must not
      // keep reporting stale evidence. When the ancestor cannot currently be
      // resolved at all, leave any previously recorded basis.state alone
      // rather than erasing known evidence.
      if (ancestorRun)
        next = {
          ...next,
          basis: { sha: next.basis.sha, state: attemptState(ancestorRun) },
        };
    }
    publishJson(revisionDirectory(directory), `${revision.sha}.json`, next);
  }

  if (!discoveryAdvisoryEmitted(directory)) {
    const undiscovered = readRevisionCoverage(directory).filter(
      ({ state }) => state === "undiscovered",
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
