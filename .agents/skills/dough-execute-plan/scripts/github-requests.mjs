// Every Open Dough request to GitHub, as one consumer process asks it: one
// `gh api` call that the one request owner of its API host and login runs
// (`./github-request-owner.mjs`, reached through
// `./github-request-owner-link.mjs`), so every dashboard process and CI
// observer of this user asking with them shares its turns at GitHub, any
// wait GitHub directed and each outstanding read, and shares none of them
// with a process asking with another login. Reads share outstanding calls.
// Explicit full-workflow repeats are independently bounded and never resent.
// The owner validates fixed read arguments or constructs the narrow action's
// arguments itself; neither accepts a shell command.

import { endedAtBound, GhFailure, unasked } from "./github-request-answer.mjs";
import {
  atRequestBound,
  OutstandingReads,
} from "./github-request-outstanding.mjs";
import { OwnerLink } from "./github-request-owner-link.mjs";
import {
  answerFromWire,
  fixedWorkflowRerun,
  workflowRerunOperation,
} from "./github-request-owner-wire.mjs";
import { ownerFailure } from "./github-request-owner-failure.mjs";

/** @import { GhAnswer, GhRequestFailure } from "./github-request-answer.mjs" */
/** @import { OwnerUnavailableCause } from "./github-request-owner-failure.mjs" */

/** @typedef {| { readonly kind: "accepted"; readonly status: 201 }
 * | { readonly kind: "refused"; readonly reason: "provider-refused" | "owner-unavailable" | "not-submitted"; readonly status?: number; readonly ownerCause?: OwnerUnavailableCause; readonly cause?: "not-installed" | "not-logged-in" | "held-back" }
 * | { readonly kind: "uncertain"; readonly reason: "submission-unconfirmed"; readonly status?: number }} WorkflowRepeatSubmission */

// One failed call this process waited on, told once to its `failed`: why,
// when this process's wait on it started and -- once asked of GitHub --
// when, with what answer, and -- once sent to an owner -- that owner.
/**
 * @typedef {{
 *   readonly cause: GhRequestFailure["kind"];
 *   readonly startedAtMs: number;
 *   readonly askedAt?: string;
 *   readonly answer?: GhAnswer;
 *   readonly owner?: import("./github-request-owner-link.mjs").OwnerIdentity;
 * }} FailedGhRequest
 */

export class GitHubRequests {
  // Every read this process still waits on, keyed by the API host and login
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

  // How a request names the API host and login this process asks under now,
  // without a secret; undefined while their endpoint cannot be established.
  // The same name keys outstanding calls above, so a caller that keys its
  // own sharing by scope uses one source.
  /** @returns {Promise<string | undefined>} */
  scope() {
    return this.#owner.scope();
  }

  // Disconnect before ending all this consumer's waits, so cancellation is
  // atomic at the owner rather than freeing turns between cancel messages.
  disconnect() {
    this.#owner.disconnect();
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
  // How this process names the API host and login it asks under now, without
  async request(args, signal) {
    return this.#outstanding.waitFor(
      JSON.stringify([await this.scope(), args]),
      signal,
      (shared) => this.#asked({ args }, shared),
    );
  }

  // One authorized action: neither client nor owner shares or resends it.
  /**
   * @param {string} repository
   * @param {number} runId
   * @param {AbortSignal} signal
   * @returns {Promise<WorkflowRepeatSubmission>}
   */
  async repeatWorkflow(repository, runId, signal) {
    const request = { operation: workflowRerunOperation, repository, runId };
    if (
      fixedWorkflowRerun({ ...request, ...this.#bounds, scope: "validate" }) ===
      undefined
    )
      throw new TypeError(
        "A workflow repeat requires one repository and a positive original run ID",
      );
    try {
      const { status, failure } = await atRequestBound(
        signal,
        this.#bounds.boundMs,
        (bounded) => this.#asked(request, bounded),
      );
      if (status === 201) return { kind: "accepted", status };
      if (
        status !== undefined &&
        [400, 401, 403, 404, 405, 409, 410, 422, 429].includes(status)
      )
        return { kind: "refused", reason: "provider-refused", status };
      if (
        failure?.kind === "owner-unavailable" &&
        !["unknown", "connection-lost"].includes(failure.cause)
      )
        return {
          kind: "refused",
          reason: "owner-unavailable",
          ownerCause: failure.cause,
        };
      if (
        failure?.kind === "not-installed" ||
        failure?.kind === "not-logged-in" ||
        failure?.kind === "held-back"
      )
        return {
          kind: "refused",
          reason: "not-submitted",
          cause: failure.kind,
        };
      return {
        kind: "uncertain",
        reason: "submission-unconfirmed",
        ...(status === undefined ? {} : { status }),
      };
    } catch {
      return { kind: "uncertain", reason: "submission-unconfirmed" };
    }
  }

  // This process's one wait on the owner for a call, begun by its first
  // requester here. It ends with the owner's answer, at a bound, or when no
  // requester here waits any more -- which leaves the call running for any
  // other process still waiting on it.
  /**
   * @param {{ readonly args?: readonly string[]; readonly operation?: string; readonly repository?: string; readonly runId?: number }} request
   * @param {AbortSignal} signal
   * @returns {Promise<GhAnswer>}
   */
  async #asked(request, signal) {
    const startedAtMs = Date.now();
    const reply = await this.#owner.ask(
      { ...request, ...this.#bounds },
      signal,
      () => endedAtBound(signal),
    );
    if (reply.left && !endedAtBound(signal)) {
      return unasked({ kind: "failed" });
    }
    const { owner } = reply;
    if (reply.left || reply.timedOut) {
      this.#failed({ cause: "timed-out", startedAtMs, owner });
      throw new GhFailure({ kind: "timed-out" });
    }
    if (reply.answer === undefined) {
      const answer = unasked(
        reply.failure ??
          (reply.refused === "operation-unavailable"
            ? { kind: "owner-unavailable", cause: "operation-unavailable" }
            : undefined) ??
          (reply.refused ? { kind: "failed" } : ownerFailure(undefined)),
      );
      this.#failed({ cause: answer.failure.kind, startedAtMs, answer, owner });
      return answer;
    }
    const answer = answerFromWire(reply.answer);
    if (reply.told && answer.failure !== undefined) {
      this.#failed({
        cause: answer.failure.kind,
        startedAtMs,
        askedAt: answer.askedAt,
        answer,
        owner,
      });
    }
    return answer;
  }
}
