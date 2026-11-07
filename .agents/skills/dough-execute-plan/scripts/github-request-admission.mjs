// Whether this process -- the machine's request owner
// (`./github-request-owner.mjs`) -- may ask GitHub now, for the one `gh`
// invocation every consumer's request reaches (`./github-request-calls.mjs`):
// the one record of a rate limit, whichever request met it and whoever asks
// next, and the turns that bound how many requests are under way at GitHub
// at once. After any wait, directed or backed off, one request goes first:
// however it ends without a rate limit -- answered, GitHub unreachable, its
// requester gone, or at its own bound -- the turns reopen, while a limit it
// meets starts the wait again with nothing else asked. What a later owner
// must know of it to go on as this one would (`kept`) is given to it when it
// starts (`./github-request-kept-admissions.mjs`).

import { longestDirectedWaitSeconds } from "./github-request-rules.mjs";

/** @import { GhAnswer, GhPrintedFailure, GhRequestFailure } from "./github-request-answer.mjs" */
/** @import { KeptHold } from "./github-request-kept-admissions.mjs" */

// How many requests one process has under way at GitHub at once, whoever
// asks; further requests wait their turn in arrival order.
const requestsUnderWayLimit = 8;

// Whether GitHub answered the request itself: a success, or a `304` naming
// what the conditional request already holds.
function succeeded(answer) {
  return answer.failure === undefined || answer.status === 304;
}

export class ReadAdmission {
  // The time, by this machine's clock, before which nothing is asked of
  // GitHub. It only ever moves later, and nothing but its time ends it.
  #resumesAtMs = 0;

  // How many rate limits that directed no wait were met since a request last
  // succeeded: each one waits twice as long as the one before.
  #backoffStep = 0;

  // How many requests hold a turn, and the requests waiting for one, in
  // arrival order: each begins when given a turn.
  #underWay = 0;
  #waiting = new Set();

  // Whether a wait was set and no request asked once it was over has yet
  // ended without a rate limit: until then, one request at a time goes first.
  #resuming = false;

  #backoffBaseMs;

  // `backoffBaseMs` is the wait after the first rate limit that directs
  // none; `kept`, what an earlier owner knew of a wait when it ended.
  /**
   * @param {number} backoffBaseMs
   * @param {KeptHold} [kept]
   */
  constructor(backoffBaseMs, kept) {
    this.#backoffBaseMs = backoffBaseMs;
    this.#resumesAtMs = kept?.resumesAtMs ?? 0;
    this.#backoffStep = kept?.backoffStep ?? 0;
    this.#resuming = kept?.resuming ?? false;
  }

  // What an owner replacing this one must know to hold back, resume and
  // back off as this one would; undefined while that is nothing.
  /** @returns {KeptHold | undefined} */
  get kept() {
    return this.#resuming || this.#backoffStep > 0
      ? {
          resumesAtMs: this.#resumesAtMs,
          backoffStep: this.#backoffStep,
          resuming: this.#resuming,
        }
      : undefined;
  }

  // Why a request may not be asked now, carrying the whole seconds left,
  // rounded up so a requester that waits as told never asks before this
  // process allows it; undefined when it may be asked.
  /**
   * @param {number} nowMs
   * @returns {GhRequestFailure | undefined}
   */
  heldBack(nowMs) {
    return nowMs < this.#resumesAtMs
      ? { kind: "held-back", waitSeconds: this.#secondsLeft(nowMs) }
      : undefined;
  }

  // Waits for one of this process's turns at GitHub, resolving with the
  // turn's end, which the request calls once it is over. A request whose
  // `signal` ends while it waits leaves without a turn, rejected with that
  // signal's reason, and the next request takes its place.
  /**
   * @param {AbortSignal} signal
   * @returns {Promise<() => void>}
   */
  turn(signal) {
    if (signal.aborted) {
      return Promise.reject(signal.reason);
    }
    if (this.#waiting.size === 0 && this.#underWay < this.#turnsOpen()) {
      this.#underWay += 1;
      return Promise.resolve(this.#endOfTurn());
    }
    return new Promise((resolve, reject) => {
      const begin = () => {
        signal.removeEventListener("abort", leave);
        resolve(this.#endOfTurn());
      };
      const leave = () => {
        this.#waiting.delete(begin);
        reject(signal.reason);
      };
      this.#waiting.add(begin);
      signal.addEventListener("abort", leave, { once: true });
    });
  }

  // How many turns may be held at once: one while asking resumes after a
  // wait, otherwise the process's bound.
  #turnsOpen() {
    return this.#resuming ? 1 : requestsUnderWayLimit;
  }

  // A turn's end, which gives turns to the requests waiting longest while
  // turns are open. While a wait stands every waiting request is given one at
  // once: it is held back when its turn comes (`./github-request-calls.mjs`)
  // and never reaches GitHub, so a wait answers them all without delay.
  #endOfTurn() {
    let over = false;
    return () => {
      if (over) {
        return;
      }
      over = true;
      this.#underWay -= 1;
      const standing = this.heldBack(Date.now()) !== undefined;
      for (const next of this.#waiting) {
        if (!standing && this.#underWay >= this.#turnsOpen()) {
          return;
        }
        this.#waiting.delete(next);
        this.#underWay += 1;
        next();
      }
    };
  }

  // Learns how one `gh` call ended once it ran, answered by GitHub or not. A
  // rate limit holds back every request until the wait GitHub directed, or,
  // when it directed none, until this process's own backoff ends; a later
  // time already standing stays. A request that succeeds ends the backoff's
  // doubling. No answer lifts a wait; any other ending of a request asked
  // once the wait had ended, whether or not GitHub answered it, reopens the
  // turns.
  /**
   * @param {GhAnswer<GhPrintedFailure>} answer
   * @param {number} nowMs
   * @returns {GhAnswer}
   */
  answered(answer, nowMs) {
    const { failure } = answer;
    if (failure?.kind !== "rate-limited") {
      if (succeeded(answer)) {
        this.#backoffStep = 0;
      }
      this.#endedWithoutLimit(answer.askedAt);
      return { ...answer, failure };
    }
    this.#resuming = true;
    const { status, waitSeconds } = failure;
    const limited =
      waitSeconds === undefined
        ? this.#backedOff(status, nowMs)
        : { kind: /** @type {const} */ ("rate-limited"), status, waitSeconds };
    this.#resumesAtMs = Math.max(
      this.#resumesAtMs,
      nowMs + limited.waitSeconds * 1000,
    );
    return { ...answer, failure: limited };
  }

  // Learns that a request asked at `askedAt` ended with no answer -- at its
  // own bound, or because no requester waits on it any more: like any ending
  // without a rate limit, it reopens the turns.
  /** @param {string} askedAt */
  endedUnanswered(askedAt) {
    this.#endedWithoutLimit(askedAt);
  }

  // A request asked once the wait had ended shows nothing of a continuing
  // limit, so asking no longer resumes one request at a time.
  #endedWithoutLimit(askedAt) {
    if (Date.parse(askedAt) >= this.#resumesAtMs) {
      this.#resuming = false;
    }
  }

  // A rate limit that directed no wait, given this process's own: the base
  // wait, doubled for each such limit since a request last succeeded, up to
  // the longest wait ever passed on. A request that was already at GitHub
  // when a wait started reports what is left of it and doubles nothing: only
  // the first request asked after a wait can show the limit continues.
  /** @returns {Extract<GhRequestFailure, { readonly kind: "rate-limited" }>} */
  #backedOff(status, nowMs) {
    const ownWait = (waitSeconds) => ({
      kind: /** @type {const} */ ("rate-limited"),
      status,
      waitSeconds,
      backoff: /** @type {const} */ (true),
    });
    if (nowMs < this.#resumesAtMs) {
      return ownWait(this.#secondsLeft(nowMs));
    }
    const longestMs = longestDirectedWaitSeconds * 1000;
    const waitMs = Math.min(
      this.#backoffBaseMs * 2 ** this.#backoffStep,
      longestMs,
    );
    if (waitMs < longestMs) {
      this.#backoffStep += 1;
    }
    return ownWait(Math.ceil(waitMs / 1000));
  }

  #secondsLeft(nowMs) {
    return Math.ceil((this.#resumesAtMs - nowMs) / 1000);
  }
}
