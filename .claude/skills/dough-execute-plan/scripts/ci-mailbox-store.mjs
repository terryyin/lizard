import {
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  renameSync,
  rmSync,
  watch,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";

const eventFilePattern = /^(\d{12})\.json$/;
const terminalResultDeadlineMs = 5_000;
const missingRevisionPollLimit = 3;
export const terminalResultDeadlineCode = "CI_OBSERVER_TERMINAL_DEADLINE";
export const terminalResultDeadlineReason =
  "CI observer terminal result was not published before its lifecycle deadline";

function publishJson(directory, name, value) {
  const temporary = join(directory, `${name}.tmp`);
  writeFileSync(temporary, JSON.stringify(value), { mode: 0o600 });
  renameSync(temporary, join(directory, name));
}
const revisionDirectory = (directory) => join(directory, "coverage");

function revisionPath(directory, sha) {
  if (!/^[0-9a-f]{40}$/i.test(sha))
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
      state: "unchecked",
      missingPolls: 0,
    });
  return readRevisionCoverage(directory).find(
    (revision) => revision.sha === normalized,
  );
}

export function readRevisionCoverage(directory) {
  const coverage = revisionDirectory(directory);
  if (!existsSync(coverage)) return [];
  return readdirSync(coverage)
    .filter((name) => /^[0-9a-f]{40}\.json$/i.test(name))
    .sort()
    .map((name) => JSON.parse(readFileSync(join(coverage, name), "utf8")));
}

function preferredAttempt(attempts) {
  return (
    attempts.find(
      ({ status, conclusion }) =>
        status === "completed" && conclusion === "success",
    ) ??
    attempts.find(({ status }) => status !== "completed") ??
    attempts[0]
  );
}

function observedRevision(revision, attempt) {
  let state = "failure";
  if (attempt.status !== "completed") state = "pending";
  else if (attempt.conclusion === "success") state = "success";
  else if (attempt.conclusion === "cancelled") state = "incomplete";
  return {
    ...revision,
    state,
    missingPolls: 0,
    checkedBy: { runId: attempt.databaseId, attemptId: attempt.attempt },
  };
}

export function observeRevisionCoverage(directory, runs, request) {
  const events = [];
  for (const revision of readRevisionCoverage(directory)) {
    const matches = runs.filter(
      ({ headSha }) => headSha?.toLowerCase() === revision.sha,
    );
    let next;
    const attempt = preferredAttempt(matches);
    if (attempt) {
      next = observedRevision(revision, attempt);
    } else if (["success", "failure", "incomplete"].includes(revision.state)) {
      next = revision;
    } else {
      const missingPolls = (revision.missingPolls ?? 0) + 1;
      next = {
        ...revision,
        state:
          missingPolls >= missingRevisionPollLimit
            ? "uncovered"
            : revision.state,
        missingPolls,
      };
      if (next.state === "uncovered" && revision.state !== "uncovered")
        events.push({
          type: "CI_COVERAGE_UNAVAILABLE",
          repo: request.repo,
          branch: request.branch,
          sha: revision.sha,
          reason: `No CI attempt for pushed revision after ${missingRevisionPollLimit} discovery polls.`,
        });
    }
    publishJson(revisionDirectory(directory), `${revision.sha}.json`, next);
  }
  return events;
}

export function readMailboxEvents(directory, after = 0) {
  return readdirSync(join(directory, "events"))
    .filter((name) => eventFilePattern.test(name))
    .sort()
    .map((name) => JSON.parse(readFileSync(join(directory, "events", name))))
    .filter(({ sequence }) => sequence > after);
}

export function publishMailboxEvent(directory, event) {
  const sequence = (readMailboxEvents(directory).at(-1)?.sequence ?? 0) + 1;
  publishJson(
    join(directory, "events"),
    `${String(sequence).padStart(12, "0")}.json`,
    { sequence, event },
  );
  return sequence;
}

export function readDeliveryProgress(directory) {
  const path = join(directory, "delivery.json");
  return existsSync(path)
    ? JSON.parse(readFileSync(path, "utf8"))
    : { deliveredThrough: 0 };
}

export function recordDeliveryProgress(directory, deliveredThrough) {
  publishJson(directory, "delivery.json", { deliveredThrough });
}

export function recordWorkerIdentity(directory, identity) {
  publishJson(directory, "worker.json", identity);
}

export function readWorkerIdentity(directory) {
  return JSON.parse(readFileSync(join(directory, "worker.json"), "utf8"));
}

function mailboxEvidence(directory) {
  const records = readMailboxEvents(directory);
  const recordedThrough = records.at(-1)?.sequence ?? 0;
  const { deliveredThrough } = readDeliveryProgress(directory);
  const unread = records.filter(
    ({ sequence }) => sequence > deliveredThrough,
  ).length;
  return { recordedThrough, deliveredThrough, unread };
}

function terminalResult(directory, request, status) {
  if (!(request.mode === "execution" && status === "stopped"))
    return { status };
  const unproved = readRevisionCoverage(directory)
    .filter(({ state }) =>
      ["unchecked", "pending", "uncovered", "incomplete"].includes(state),
    )
    .map(({ sha, state }) => ({ sha, state }));
  return {
    status,
    coverage: {
      state: "ended",
      pendingCi: "unobserved",
      ...(unproved.length ? { unproved } : {}),
    },
    evidence: mailboxEvidence(directory),
  };
}

export function recordLostTerminalResult(directory) {
  const result = {
    status: "stopped",
    coverage: {
      state: "lost",
      pendingCi: "unobserved",
      reason: terminalResultDeadlineReason,
    },
    evidence: mailboxEvidence(directory),
  };
  rmSync(join(directory, "result.json.tmp"), { force: true });
  publishJson(directory, "result.json", result);
  return result;
}

export async function waitForTerminalResult(
  directory,
  { deadline = AbortSignal.timeout(terminalResultDeadlineMs) } = {},
) {
  const path = join(directory, "result.json");
  if (!existsSync(path))
    await new Promise((resolve, reject) => {
      let settled = false;
      const close = () => {
        if (settled) return false;
        settled = true;
        subscription?.close();
        deadline.removeEventListener("abort", atDeadline);
        return true;
      };
      const finished = () => {
        if (!existsSync(path)) return;
        if (close()) resolve();
      };
      const atDeadline = () => {
        if (existsSync(path)) {
          finished();
          return;
        }
        if (!close()) return;
        const error = new Error(terminalResultDeadlineReason, {
          cause: deadline.reason,
        });
        error.code = terminalResultDeadlineCode;
        reject(error);
      };
      deadline.addEventListener("abort", atDeadline, { once: true });
      const subscription = watch(directory, finished);
      if (deadline.aborted) atDeadline();
      finished();
    });
  return JSON.parse(readFileSync(path, "utf8"));
}

export function recordTerminalResult(directory, request, status) {
  publishJson(
    directory,
    "result.json",
    terminalResult(directory, request, status),
  );
}
