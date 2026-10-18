// Reading one `gh api --include` answer, for every Open Dough request to
// GitHub (`./github-requests.mjs`): GitHub's status line and headers come
// first, so a caller can decide from GitHub's own status and headers -- a
// `304`, or a rate limit's directed wait -- before treating `gh`'s exit as a
// failure, and otherwise the failure is classified from what `gh` printed.
// Failures expose fixed categories and HTTP statuses, never raw stderr.

import { ReadBoundReached } from "./github-request-outstanding.mjs";
import { longestDirectedWaitSeconds } from "./github-request-rules.mjs";
/** @import { OwnerUnavailableFailure } from "./github-request-owner-failure.mjs" */

// Why a `gh` call did not answer, as far as can be said without repeating
// anything `gh` printed. A `rate-limited` wait is how long the asking process
// waits before asking GitHub again (`./github-request-admission.mjs`): the
// wait GitHub directed, already validated and bounded, never a header value
// as GitHub sent it; or, marked `backoff`, the process's own when GitHub
// directed none. `held-back` was not asked of GitHub at all: a rate limit met
// earlier still holds back every request for these whole seconds. Never
// GitHub's status, so never an absence either.
/**
 * @typedef {| { readonly kind: "not-logged-in" }
 *   | { readonly kind: "http"; readonly status: number }
 *   | {
 *       readonly kind: "rate-limited";
 *       readonly status: number;
 *       readonly waitSeconds: number;
 *       readonly backoff?: true;
 *     }
 *   | { readonly kind: "held-back"; readonly waitSeconds: number }
 *   | { readonly kind: "unreachable" }
 *   | { readonly kind: "timed-out" }
 *   | { readonly kind: "not-installed" }
 *   | { readonly kind: "output-bound" }
 *   | OwnerUnavailableFailure
 *   | { readonly kind: "failed" }} GhRequestFailure
 */

// How one `gh` call failed, as it printed: a rate limit names a wait only
// when GitHub directed one, until admission gives every rate limit its wait.
/**
 * @typedef {| Exclude<
 *       GhRequestFailure,
 *       { readonly kind: "rate-limited" | "held-back" }
 *     >
 *   | {
 *       readonly kind: "rate-limited";
 *       readonly status: number;
 *       readonly waitSeconds?: number;
 *     }} GhPrintedFailure
 */

// One `gh api --include` answer, read once for every request sharing it: as
// admitted, unless `Failure` says it is still as `gh` printed it.
// - `status`, `headers`: GitHub's, when `gh` printed them; no status when no
//   answer came from GitHub (no login, no connection, no `gh`).
// - `body`: what `gh` printed after the head, filtered by any `--jq`.
// - `failure`: how the call failed, when `gh` exited non-zero; a successful
//   exit has none. A conditional caller still decides from `status` first (a
//   `304` exits non-zero too).
// - `askedAt`: when this answer was asked of GitHub, by this machine's clock:
//   the start of the one `gh` call it shares.
/**
 * @template [Failure=GhRequestFailure]
 * @typedef {{
 *   readonly status: number | undefined;
 *   readonly headers: ReadonlyMap<string, string>;
 *   readonly body: string;
 *   readonly failure: Failure | undefined;
 *   readonly askedAt: string;
 * }} GhAnswer
 */

// A `gh` call that did not answer. A caller reading an answer may name
// reasons of its own beside the request's.
/** @template {{ readonly kind: string }} [Reason=GhRequestFailure] */
export class GhFailure extends Error {
  /** @param {Reason} reason */
  constructor(reason) {
    super(`gh did not answer: ${reason.kind}`);
    /** @readonly @type {Reason} */
    this.reason = reason;
  }
}

// An answer that never came from GitHub: held back, or -- `failed` -- for a
// call no requester waits on any more, whether waiting its turn or already
// asked.
/**
 * @param {GhRequestFailure} failure
 * @param {string} [askedAt]
 * @returns {GhAnswer}
 */
export function unasked(failure, askedAt = new Date().toISOString()) {
  return { status: undefined, headers: new Map(), body: "", failure, askedAt };
}

// The shared call's own bound (`ReadBoundReached`) or a waiter's own bound
// ended it -- not ordinary departure or shutdown.
/** @param {AbortSignal} signal */
export function endedAtBound(signal) {
  return (
    signal.reason instanceof ReadBoundReached ||
    (signal.reason instanceof GhFailure &&
      signal.reason.reason.kind === "timed-out")
  );
}

const wholeNumber = /^\d+$/;

function secondsUntil(atMs, nowMs) {
  return Math.ceil((atMs - nowMs) / 1000);
}

// GitHub directs a wait in one of two ways: `Retry-After` (seconds, or an
// HTTP date), or `X-RateLimit-Reset` (epoch seconds) once
// `X-RateLimit-Remaining` has reached zero.
function askedWait(headers, nowMs) {
  const retryAfter = headers.get("retry-after");
  if (retryAfter !== undefined) {
    if (wholeNumber.test(retryAfter)) {
      return Number(retryAfter);
    }
    const at = Date.parse(retryAfter);
    return Number.isFinite(at) ? secondsUntil(at, nowMs) : undefined;
  }
  const reset = headers.get("x-ratelimit-reset");
  if (
    headers.get("x-ratelimit-remaining") === "0" &&
    reset !== undefined &&
    wholeNumber.test(reset)
  ) {
    return secondsUntil(Number(reset) * 1000, nowMs);
  }
  return undefined;
}

// Whole seconds from `nowMs` until GitHub allows another request, read from
// the headers of a refused answer and bounded to `longestDirectedWaitSeconds`,
// or undefined when the headers direct nothing usable, including a time not
// later than now.
/**
 * @param {ReadonlyMap<string, string>} headers
 * @param {number} nowMs
 * @returns {number | undefined}
 */
export function directedWaitSeconds(headers, nowMs) {
  const asked = askedWait(headers, nowMs);
  return asked === undefined || !Number.isFinite(asked) || asked <= 0
    ? undefined
    : Math.min(asked, longestDirectedWaitSeconds);
}

// What `gh api --include` printed: GitHub's status line and headers, then the
// body after the first blank line.
function parseIncluded(stdout) {
  const lines = stdout.split("\n");
  const statusLine = /^HTTP\/\S+\s+(\d{3})\b/.exec(lines[0] ?? "");
  if (statusLine?.[1] === undefined) {
    return undefined;
  }
  const headers = new Map();
  let at = 1;
  for (; at < lines.length; at += 1) {
    const line = (lines[at] ?? "").replace(/\r$/, "");
    if (line === "") {
      break;
    }
    const colon = line.indexOf(":");
    if (colon > 0) {
      headers.set(
        line.slice(0, colon).trim().toLowerCase(),
        line.slice(colon + 1).trim(),
      );
    }
  }
  return {
    status: Number(statusLine[1]),
    headers,
    body: lines.slice(at + 1).join("\n"),
  };
}

// A refused answer that says when to ask again is a rate limit, whatever
// else `gh` printed: GitHub directs a wait on its `403` and `429` answers.
// Only the validated, bounded wait leaves this module.
function limitedAsDirected(answer, nowMs) {
  if (answer?.status !== 403 && answer?.status !== 429) {
    return undefined;
  }
  const waitSeconds = directedWaitSeconds(answer.headers, nowMs);
  return waitSeconds === undefined
    ? undefined
    : { kind: "rate-limited", status: answer.status, waitSeconds };
}

// What `gh` printed about a failure, when GitHub's answer directed no wait:
// `gh` exits 4 when it has no usable login, and says so by pointing at
// `gh auth login`; an HTTP error ends its stderr with "(HTTP <status>)", and a
// rate limit or a failed connection names itself.
// Timing out is never inferred from `gh`'s output: it is a requester's own
// bound or the shared call's (`./github-requests.mjs`).
function classify(error, stderr) {
  if (error.code === "ERR_CHILD_PROCESS_STDIO_MAXBUFFER") {
    return { kind: "output-bound" };
  }
  if (error.code === "ENOENT") {
    return { kind: "not-installed" };
  }
  if (Number(error.code) === 4 || /\bgh auth login\b/.test(stderr)) {
    return { kind: "not-logged-in" };
  }
  const http = /\(HTTP (\d{3})\)/.exec(stderr);
  if (http?.[1] !== undefined) {
    const status = Number(http[1]);
    return /rate limit/i.test(stderr)
      ? { kind: "rate-limited", status }
      : { kind: "http", status };
  }
  if (/\berror connecting to\b/.test(stderr)) {
    return { kind: "unreachable" };
  }
  return { kind: "failed" };
}

// What one `gh` call printed and how it exited, read as its answer; `nowMs`
// is when it is read, from which a directed wait is counted.
/**
 * @param {{ readonly code?: string | number | null | undefined } | null} error
 * @param {string} stdout
 * @param {string} stderr
 * @param {string} askedAt
 * @param {number} [nowMs]
 * @returns {GhAnswer<GhPrintedFailure>}
 */
export function readAnswer(error, stdout, stderr, askedAt, nowMs = Date.now()) {
  const included = parseIncluded(stdout);
  return {
    status: included?.status,
    headers: included?.headers ?? new Map(),
    body:
      error?.code === "ERR_CHILD_PROCESS_STDIO_MAXBUFFER"
        ? ""
        : (included?.body ?? stdout),
    failure: error
      ? (limitedAsDirected(included, nowMs) ?? classify(error, stderr))
      : undefined,
    askedAt,
  };
}
