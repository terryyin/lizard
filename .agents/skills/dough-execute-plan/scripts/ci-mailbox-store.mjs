import { existsSync, readFileSync, readdirSync, rmSync, watch } from "node:fs";
import { join } from "node:path";
import { publishJson } from "./ci-mailbox-json-file.mjs";
import {
  effectiveRevisionState,
  readRevisionCoverage,
  revisionVerdicts,
} from "./ci-mailbox-revision-records.mjs";

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

// Merges an acknowledged update (`deliveredThrough`, `lossReported`) into the
// coordinator's durable delivery progress for this mailbox.
export function recordDeliveryProgress(directory, update) {
  publishJson(directory, "delivery.json", {
    ...readDeliveryProgress(directory),
    ...update,
  });
}

// Records that the coordinator received this mailbox's records through
// `sequence`. Only published records can be acknowledged, and progress never
// moves backward.
export function acknowledgeDelivery(directory, sequence) {
  const recordedThrough = readMailboxEvents(directory).at(-1)?.sequence ?? 0;
  if (!Number.isInteger(sequence) || sequence < 1 || sequence > recordedThrough)
    throw new Error(
      `Cannot acknowledge record ${sequence}; records end at ${recordedThrough}`,
    );
  const deliveredThrough = Math.max(
    readDeliveryProgress(directory).deliveredThrough,
    sequence,
  );
  recordDeliveryProgress(directory, { deliveredThrough });
  return deliveredThrough;
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

// A revision is proved by a verdict of its effective state (its own, or an
// ignored-only revision's applicable ancestor's `basis.state`), or by its
// declared integrated-state evidence (see ci-integrated-state.mjs). An
// unproved `not_required` revision stays visible with its `basis` so a reader
// can see why no run exists for it and what its effective attempt's state is.
function unresolvedRevisions(directory) {
  return readRevisionCoverage(directory)
    .filter(
      (revision) =>
        revision.integrated?.state !== "success" &&
        !revisionVerdicts.has(effectiveRevisionState(revision)),
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
