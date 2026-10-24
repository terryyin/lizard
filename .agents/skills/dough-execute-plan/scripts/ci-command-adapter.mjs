import { execFile } from "node:child_process";
import {
  currentCommandAttempt,
  latestCommandAttempts,
  targetCommandAttempts,
} from "./ci-command-attempts.mjs";
import { ciAttemptKey } from "./ci-failures.mjs";
import { createStartupRunFilter } from "./ci-runs.mjs";
import { isFullGitRevision } from "./ci-revisions.mjs";

const defaultAdapterTimeoutMs = 20_000;
const adapterResponseBytes = 64 * 1024;
export const diagnosticExcerptBytes = 16 * 1024;

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
      attempts: targetCommandAttempts(response.attempts, branch),
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
      const response = await runAdapter(
        command,
        {
          operation: "diagnose",
          check: { repo, branch },
          attempt: {
            runId: failed.databaseId,
            attemptId: failed.attempt,
            sha: failed.headSha,
          },
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

function runAdapter(command, request, { signal, cwd, timeoutMs }) {
  return new Promise((resolve, reject) => {
    const child = execFile(
      command[0],
      command.slice(1),
      {
        cwd,
        signal,
        timeout: timeoutMs,
        maxBuffer: adapterResponseBytes,
        encoding: "utf8",
      },
      (error, stdout) => {
        if (error) return reject(error);
        try {
          resolve(JSON.parse(stdout));
        } catch (parseError) {
          reject(
            new Error(
              `CI adapter returned invalid JSON: ${parseError.message}`,
            ),
          );
        }
      },
    );
    child.stdin.end(`${JSON.stringify(request)}\n`);
  });
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
    const bounded = truncateUtf8(unavailable, 600);
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

function truncateUtf8(value, limit) {
  let text = "";
  let bytes = 0;
  for (const character of value) {
    const size = Buffer.byteLength(character);
    if (bytes + size > limit) return { text, truncated: true };
    text += character;
    bytes += size;
  }
  return { text, truncated: false };
}
