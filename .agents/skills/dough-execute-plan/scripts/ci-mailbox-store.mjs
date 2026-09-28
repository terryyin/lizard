import { existsSync, readFileSync, readdirSync, rmSync, watch } from "node:fs";
import { join } from "node:path";
import { publishJson } from "./ci-mailbox-json-file.mjs";
import { readRevisionCoverage } from "./ci-mailbox-revision-coverage.mjs";

const eventFilePattern = /^(\d{12})\.json$/;
const defaultTerminalResultDeadlineMs = 5_000;
export const terminalResultDeadlineCode = "CI_OBSERVER_TERMINAL_DEADLINE";
export const terminalResultDeadlineReason =
  "CI observer terminal result was not published before its lifecycle deadline";

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

const unresolvedRevisionStates = ["undiscovered", "pending", "incomplete"];
// A `not_required` revision's own state is never itself "pending" or
// "incomplete" (see ci-mailbox-revision-coverage.mjs); whether it is proved
// terminal is decided by its applicable ancestor's resolved `basis.state`.
// A proved success or failure ancestor makes it a proved terminal case, same
// as an ordinary registered revision, so it is fully omitted here. A still
// pending/incomplete (or not yet resolved) ancestor must not be reported as
// success, and must not silently vanish either: it stays visible with its
// applicable source (`basis`) so a reader can see why no run exists for this
// revision and what its effective attempt's real state is.
const provedApplicableAncestorStates = ["success", "failure"];

function unresolvedRevisions(directory) {
  return readRevisionCoverage(directory)
    .filter(
      ({ state, basis }) =>
        unresolvedRevisionStates.includes(state) ||
        (state === "not_required" &&
          !provedApplicableAncestorStates.includes(basis?.state)),
    )
    .map(({ sha, state, basis }) =>
      state === "not_required" ? { sha, state, basis } : { sha, state },
    );
}

function terminalResult(directory, request, status) {
  if (!(request.mode === "execution" && status === "stopped"))
    return { status };
  const unproved = unresolvedRevisions(directory);
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

// Distinct from terminalResultDeadlineReason: this records an unexpected
// worker death discovered by a liveness check, at an ordinary coordinator
// interaction or while stop awaits the result, not the stop command's own
// publication deadline.
export const workerLossReason =
  "CI observer worker exited without recording a normal terminal result";

export function recordLostTerminalResult(
  directory,
  reason = terminalResultDeadlineReason,
) {
  const unproved = unresolvedRevisions(directory);
  const result = {
    status: "stopped",
    coverage: {
      state: "lost",
      pendingCi: "unobserved",
      reason,
      ...(unproved.length ? { unproved } : {}),
    },
    evidence: mailboxEvidence(directory),
  };
  rmSync(join(directory, "result.json.tmp"), { force: true });
  publishJson(directory, "result.json", result);
  return result;
}

// Tests shorten the deadline through DOUGH_CI_TERMINAL_RESULT_DEADLINE_MS to
// observe it firing without paying the full wait; nothing else sets it.
export function terminalResultDeadlineMs() {
  const configured = Number(process.env.DOUGH_CI_TERMINAL_RESULT_DEADLINE_MS);
  return Number.isFinite(configured) && configured > 0
    ? configured
    : defaultTerminalResultDeadlineMs;
}

export function terminalResultDeadline() {
  return AbortSignal.timeout(terminalResultDeadlineMs());
}

export async function waitForTerminalResult(
  directory,
  { deadline = terminalResultDeadline() } = {},
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
