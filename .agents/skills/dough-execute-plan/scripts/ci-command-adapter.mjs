import {
  answeredCommandAttempts,
  currentCommandAttempt,
  latestCommandAttempts,
  targetCommandAttempts,
} from "./ci-command-attempts.mjs";
import {
  defaultAdapterTimeoutMs,
  runAdapter,
  runAdapterForAttempt,
  truncateUtf8,
} from "./ci-command-run.mjs";
import { ciAttemptKey } from "./ci-failures.mjs";
import { createStartupRunFilter } from "./ci-runs.mjs";
import { isFullGitRevision } from "./ci-revisions.mjs";

export const diagnosticExcerptBytes = 16 * 1024;
const unavailableReasonBytes = 600;

const isObject = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);

// Retained attempts remain evidence for registered revisions, and candidates
// for applicable ancestors and integrated state, even when the ordinary
// notification stream suppresses completed startup history. Integrated-state
// facts come only from the poll's own discovery: none survive a failed one.
export function createCommandAcquisition(options) {
  const discover = createCommandDiscovery(options);
  const currentRuns = createStartupRunFilter();
  let retained = [];
  let polled;
  let queue;
  return {
    acquireRuns: async (signal) => {
      polled = undefined;
      queue = undefined;
      const snapshot = await discover(signal);
      const { attempts } = snapshot;
      queue = snapshot.queue;
      retained = latestCommandAttempts(attempts);
      polled = attempts;
      const current = currentRuns(retained);
      const registered = new Set(
        ((await options.registeredRevisions?.()) ?? []).map((sha) =>
          sha.toLowerCase(),
        ),
      );
      return retained.filter(
        (run) =>
          current.includes(run) || registered.has(run.headSha.toLowerCase()),
      );
    },
    acquireFailure: createCommandFailureAcquisition(options),
    // Coverage owns workflow-path applicability; candidates retain their real
    // attempt state and never enter the ordinary failure stream by themselves.
    discoverAncestorCandidates: async () => retained,
    // Every attempt this poll's discover answered, before newest-per-check
    // retention: CI status counts attemptId ambiguity over these.
    discoverAnsweredAttempts: async () => polled ?? [],
    discoverQueue: async () => queue,
    readCurrentAttempt: async (run) => {
      if (!polled) throw new Error("CI adapter discovery is unavailable");
      return currentCommandAttempt(polled, run);
    },
  };
}

function createCommandDiscovery({
  command,
  repo,
  branch,
  root,
  timeoutMs = defaultAdapterTimeoutMs,
  includeOtherBranches = false,
}) {
  return async (signal) => {
    const response = await runAdapter(
      command,
      { operation: "discover", check: { repo, branch } },
      { signal, cwd: root, timeoutMs },
    );
    if (!response || !Array.isArray(response.attempts))
      throw new Error("CI adapter discovery must return an attempts array");
    return {
      attempts: (includeOtherBranches
        ? answeredCommandAttempts
        : targetCommandAttempts)(response.attempts, branch),
      queue: targetCommandQueue(response.queue, branch),
    };
  };
}

// Queue progress has no attempt identity or verdict. An omitted queue means
// unsupported; an empty array is an authoritative empty snapshot.
function targetCommandQueue(queue, branch) {
  if (queue === undefined) return;
  if (!Array.isArray(queue))
    throw new Error("CI adapter queue must be an array");
  return queue
    .map((entry) => {
      if (
        !entry ||
        !isFullGitRevision(entry.sha) ||
        typeof entry.branch !== "string" ||
        !entry.branch ||
        !["queued", "running", "retrying"].includes(entry.state) ||
        ![entry.check, entry.context].every(
          (value) =>
            value === undefined ||
            (typeof value === "string" && value.length > 0),
        )
      )
        throw new Error("CI adapter returned an invalid queue entry");
      return {
        sha: entry.sha.toLowerCase(),
        branch: entry.branch,
        state: entry.state,
        ...(entry.check === undefined ? {} : { check: entry.check }),
        ...(entry.context === undefined ? {} : { context: entry.context }),
      };
    })
    .filter((entry) => entry.branch === branch);
}

function createCommandFailureAcquisition({
  command,
  repo,
  branch,
  root,
  timeoutMs = defaultAdapterTimeoutMs,
}) {
  const reported = new Set();
  let awaitingDiagnostic;
  return async (runs, signal) => {
    const failed =
      awaitingDiagnostic ??
      runs.find(
        (run) =>
          run.status === "completed" &&
          run.conclusion === "failure" &&
          !reported.has(ciAttemptKey(run.databaseId, run.attempt)),
      );
    if (!failed) return {};
    try {
      const response = await runAdapterForAttempt(
        command,
        "diagnose",
        { repo, branch },
        {
          runId: failed.databaseId,
          attemptId: failed.attempt,
          sha: failed.headSha,
        },
        { signal, cwd: root, timeoutMs },
      );
      const event = commandFailureEvent(repo, failed, response);
      reported.add(ciAttemptKey(failed.databaseId, failed.attempt));
      awaitingDiagnostic = undefined;
      return { event };
    } catch (error) {
      awaitingDiagnostic = failed;
      const reason = `Could not retrieve custom CI diagnostics: ${error instanceof Error ? error.message : error}`;
      return {
        observationError: reason,
        deferredFailureEvent: commandFailureEvent(repo, failed, {
          unavailable: reason,
        }),
      };
    }
  };
}

// The dashboard reads proof or jobs only for a selected attempt. This
// boundary checks the response envelope; the CI proof read checks a proof's
// contents (`../../dough-test-optimization/scripts/ci-proof-read.mjs`).
export async function readCommandProof(command, check, attempt, options) {
  const response = await runAdapterForAttempt(
    command,
    "proof",
    check,
    attempt,
    options,
  );
  const { summary, unavailable } = isObject(response) ? response : {};
  if (isObject(summary) && unavailable === undefined) return { summary };
  if (typeof unavailable === "string" && unavailable && summary === undefined)
    return {
      unavailable: truncateUtf8(unavailable, unavailableReasonBytes).text,
    };
  throw new Error("CI adapter proof must return summary or unavailable");
}

export async function readCommandJobs(command, check, attempt, options) {
  const response = await runAdapterForAttempt(
    command,
    "jobs",
    check,
    attempt,
    options,
  );
  const { jobs, unavailable } = isObject(response) ? response : {};
  if (Array.isArray(jobs) && unavailable === undefined) return { jobs };
  if (typeof unavailable === "string" && unavailable && jobs === undefined)
    return {
      unavailable: truncateUtf8(unavailable, unavailableReasonBytes).text,
    };
  throw new Error("CI adapter jobs must return jobs or unavailable");
}

function commandFailureEvent(repo, run, response) {
  if (!response || typeof response !== "object")
    throw new Error("CI adapter diagnostic must return an object");
  const diagnostic = boundedDiagnostic(response);
  return {
    type: "CI_FAILURE",
    repo,
    sha: run.headSha,
    branch: run.headBranch,
    runId: run.databaseId,
    attempt: run.attempt,
    conclusion: run.conclusion,
    ...(run.url === undefined ? {} : { url: run.url }),
    diagnostic,
  };
}

function boundedDiagnostic(response) {
  const unavailable = response.unavailable;
  if (unavailable !== undefined) {
    if (typeof unavailable !== "string" || !unavailable)
      throw new Error("CI adapter diagnostic unavailability must be a reason");
    const bounded = truncateUtf8(unavailable, unavailableReasonBytes);
    return {
      unavailable: bounded.text,
      truncated: bounded.truncated,
    };
  }
  if (typeof response.excerpt !== "string")
    throw new Error("CI adapter diagnostic must return excerpt or unavailable");
  if (
    response.truncated !== undefined &&
    typeof response.truncated !== "boolean"
  )
    throw new Error("CI adapter diagnostic truncation must be boolean");
  const bounded = truncateUtf8(response.excerpt, diagnosticExcerptBytes);
  return {
    excerpt: bounded.text,
    truncated: Boolean(response.truncated) || bounded.truncated,
  };
}
