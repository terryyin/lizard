// Every Open Dough request to GitHub, as one consumer process asks it: one
// `gh api` call that the one request owner of its API host and login runs
// (`./github-request-owner.mjs`, reached through
// `./github-request-owner-link.mjs`), so every dashboard process and CI
// observer of this user asking with them shares its turns at GitHub, any
// wait GitHub directed and each outstanding call, and shares none of them
// with a process asking with another login. Requesters in this process
// asking one call while it is outstanding wait on it together
// (`./github-request-outstanding.mjs`). Each call has a fixed argument array
// beginning with `api` -- never a shell string. What a caller may ask, what
// it keeps of an answer, and how it reports one stay with the caller.

import { endedAtBound, GhFailure, unasked } from "./github-request-answer.mjs";
import { OutstandingReads } from "./github-request-outstanding.mjs";
import { OwnerLink } from "./github-request-owner-link.mjs";
import { answerFromWire } from "./github-request-owner-wire.mjs";
import { ownerFailure } from "./github-request-owner-failure.mjs";

/** @import { GhAnswer, GhRequestFailure } from "./github-request-answer.mjs" */

// One failed call this process waited on, told once to its `failed`: why,
// when this process's wait on it started and -- once asked of GitHub --
// when, and with what answer.
/**
 * @typedef {{
 *   readonly cause: GhRequestFailure["kind"];
 *   readonly startedAtMs: number;
 *   readonly askedAt?: string;
 *   readonly answer?: GhAnswer;
 * }} FailedGhRequest
 */

export class GitHubRequests {
  // Every call this process still waits on, keyed by the API host and login
  // it is asked under -- named as each request names them, without a secret
  // -- and its argument array: the question asked of GitHub, any conditional
  // hint included. Its requesters here share one wait on the owner, which
  // ends once none of them waits or at its bound. No settled answer is kept
  // here.
  #outstanding;

  #owner;

  #failed;

  #bounds;

  // `boundMs` is how long one call may run from its start however many
  // requesters wait on it; `backoffBaseMs`, the first wait after a rate limit
  // that directs none, doubling while the limit continues, for an owner this
  // process starts. `maxBufferBytes` is the most one answer may print before
  // its call fails. `failed` hears once of each failed call this process
  // waited on -- GitHub's failed answer, a bound that ended it, or an owner
  // that could not be asked -- from the request that began this process's
  // wait on it; held-back and ordinary requester departures are not told.
  /**
   * @param {{
   *   readonly boundMs: number;
   *   readonly backoffBaseMs: number;
   *   readonly failed?: (request: FailedGhRequest) => void;
   *   readonly maxBufferBytes?: number;
   * }} options
   */
  constructor({
    boundMs,
    backoffBaseMs,
    failed = () => undefined,
    maxBufferBytes = 1024 * 1024,
  }) {
    this.#outstanding = new OutstandingReads(boundMs);
    this.#owner = new OwnerLink({ backoffBaseMs });
    this.#failed = failed;
    this.#bounds = { boundMs, maxBufferBytes };
  }

  // One `gh api` answer, settled whatever its exit: `gh` prints GitHub's
  // status line on stdout even when it exits non-zero, so a caller can decide
  // from that status before treating the exit as a failure. `signal` ends
  // only this requester's wait, which then rejects with its reason and gets
  // no answer; a call that reached its own bound is a timed-out `GhFailure`
  // for every requester waiting on it. A request the owner cannot be asked
  // settles with a safe owner-unavailable cause, never asked through a fallback.
  /**
   * @param {readonly string[]} args
   * @param {AbortSignal} signal
   * @returns {Promise<GhAnswer>}
   */
  async request(args, signal) {
    return this.#outstanding.waitFor(
      JSON.stringify([await this.#owner.scope(), args]),
      signal,
      (shared) => this.#asked(args, shared),
    );
  }

  // This process's one wait on the owner for a call, begun by its first
  // requester here. It ends with the owner's answer, at a bound, or when no
  // requester here waits any more -- which leaves the call running for any
  // other process still waiting on it.
  async #asked(args, signal) {
    const startedAtMs = Date.now();
    const reply = await this.#owner.ask({ args, ...this.#bounds }, signal, () =>
      endedAtBound(signal),
    );
    if (reply.left && !endedAtBound(signal)) {
      return unasked({ kind: "failed" });
    }
    if (reply.left || reply.timedOut) {
      this.#failed({ cause: "timed-out", startedAtMs });
      throw new GhFailure({ kind: "timed-out" });
    }
    if (reply.answer === undefined) {
      const answer = unasked(
        reply.failure ??
          (reply.refused ? { kind: "failed" } : ownerFailure(undefined)),
      );
      this.#failed({ cause: answer.failure.kind, startedAtMs, answer });
      return answer;
    }
    const answer = answerFromWire(reply.answer);
    if (reply.told && answer.failure !== undefined) {
      this.#failed({
        cause: answer.failure.kind,
        startedAtMs,
        askedAt: answer.askedAt,
        answer,
      });
    }
    return answer;
  }
}
