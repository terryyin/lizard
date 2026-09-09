import {
  existsSync,
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
export const terminalResultDeadlineCode = "CI_OBSERVER_TERMINAL_DEADLINE";
export const terminalResultDeadlineReason =
  "CI observer terminal result was not published before its lifecycle deadline";

function publishJson(directory, name, value) {
  const temporary = join(directory, `${name}.tmp`);
  writeFileSync(temporary, JSON.stringify(value), { mode: 0o600 });
  renameSync(temporary, join(directory, name));
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
  return {
    status,
    coverage: { state: "ended", pendingCi: "unobserved" },
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
