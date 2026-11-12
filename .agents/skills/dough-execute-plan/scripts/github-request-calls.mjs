// One request owner's fixed-argument gh calls, each with included status and
// headers. Reads share outstanding calls; explicit full-workflow repeats
// bypass sharing and never retry. Every call is admitted once and shares
// the owner's turns and retained rate-limit wait.

import { execFile } from "node:child_process";
import { endedAtBound, GhFailure, unasked } from "./github-request-answer.mjs";
import {
  atRequestBound,
  OutstandingReads,
} from "./github-request-outstanding.mjs";
import {
  printedAnswer,
  readRoutes,
  workflowRerunOperation,
} from "./github-request-owner-wire.mjs";

/** @import { GhAnswer } from "./github-request-answer.mjs" */
/** @import { KeptAdmissions } from "./github-request-kept-admissions.mjs" */
/** @import { OwnedCalls } from "./github-request-owned-calls.mjs" */

// How long one call may run from its start however many requesters wait on
// it, and the most its answer may print before it fails: its first
// requester's.
/**
 * @typedef {{
 *   readonly boundMs: number;
 *   readonly maxBufferBytes: number;
 * }} CallBounds
 */

// Every call asks for GitHub's status line and headers (`--include`), right
// after `api`, so a refusal's status and direction are read here whatever
// was asked.
function includedArgs(args) {
  return ["api", "--include", ...args.slice(1)];
}

// Which of GitHub's rate-limit resources a call counts against: its search
// and GraphQL allowances are apart from every other route's.
function resourceOf(args) {
  const [route = ""] = readRoutes(args) ?? [];
  const [first] = route.replace(/^\/+/, "").split(/[/?]/);
  return first === "search" || first === "graphql" ? first : "core";
}

export class AdmittedCalls {
  // Every `gh` call still outstanding, keyed by the API host and login it
  // is asked under, its argument array and its answer's bound: the question
  // asked of GitHub, any conditional hint included. Requesters asking it
  // while it is outstanding wait on that one call, which ends once none of
  // them waits or at its own bound: its first requester's, so none is set
  // here. No settled answer is kept here.
  #outstanding = new OutstandingReads(0);

  // Whether GitHub may be asked now, and the turns at GitHub, for each
  // rate-limit resource. Every call `#outstanding` starts is admitted once,
  // for all the requesters waiting on it, and takes one turn however many
  // wait on it.
  #admissions;

  #loginStands;

  #failed;

  #owned;

  // How many `gh` processes run now.
  #running = 0;

  // `admissions` says whether each resource may be asked, and keeps each
  // rate limit's wait for the owner that replaces this one; `owned` records
  // each `gh` process while it runs. `loginStands` says whether this
  // process's `gh` still asks with the API host and login these calls are
  // asked under. `failed` hears once of each failed answer `gh` gave; a
  // held-back request, a requester's departure, a bound that ends a call
  // and a call another login answered give none.
  /**
   * @param {{
   *   readonly admissions: KeptAdmissions;
   *   readonly owned: OwnedCalls;
   *   readonly loginStands: () => Promise<boolean>;
   *   readonly failed?: (answer: GhAnswer) => void;
   * }} options
   */
  constructor({ admissions, owned, loginStands, failed = () => undefined }) {
    this.#admissions = admissions;
    this.#owned = owned;
    this.#loginStands = loginStands;
    this.#failed = failed;
  }

  // One `gh api` answer, settled whatever its exit. `signal` ends only this
  // requester's wait, which then rejects with its reason and gets no answer;
  // a call that reached its own bound is a timed-out `GhFailure` for every
  // requester waiting on it.
  /**
   * @param {readonly string[]} args
   * @param {AbortSignal} signal
   * @param {CallBounds & { readonly scope: string; readonly operation?: string }} bounds
   * @returns {Promise<GhAnswer>}
   */
  request(args, signal, { boundMs, maxBufferBytes, scope, operation }) {
    const asked = includedArgs(args);
    const resource = resourceOf(args);
    if (operation === workflowRerunOperation)
      return atRequestBound(signal, boundMs, (bounded) =>
        this.#spawned(asked, bounded, maxBufferBytes, resource),
      );
    return this.#outstanding.waitFor(
      JSON.stringify([scope, asked, maxBufferBytes]),
      signal,
      (shared) => this.#spawned(asked, shared, maxBufferBytes, resource),
      boundMs,
    );
  }

  // How many `gh` processes this owner still runs: one it ended is counted
  // until it is gone.
  get running() {
    return this.#running;
  }

  // Why nothing of some resource may be asked of GitHub now, when a rate
  // limit's wait stands.
  /** @param {number} nowMs */
  heldBack(nowMs) {
    return this.#admissions.heldBack(nowMs);
  }

  // Holds one of `resource`'s turns at GitHub until `gone` resolves, for a
  // call at GitHub that is not one of this owner's own: it counts toward
  // the turns like any other.
  /**
   * @param {string} resource
   * @param {Promise<unknown>} gone
   */
  occupied(resource, gone) {
    void this.#admissions
      .of(resource)
      .turn(new AbortController().signal)
      .then((turnEnds) => gone.finally(turnEnds));
  }

  // Runs `gh` once, recorded until its process is gone and answered only
  // then: execFile answers a call `signal` ends as it signals that process,
  // which may still be exiting. One that cannot be recorded is ended at once,
  // and its call fails.
  #ran(args, signal, maxBufferBytes, resource) {
    return new Promise((resolve) => {
      let gone = () => undefined;
      let exited = Promise.resolve();
      const child = execFile(
        "gh",
        [...args],
        {
          signal,
          maxBuffer: maxBufferBytes,
          encoding: "buffer",
          env: { ...process.env, GH_PROMPT_DISABLED: "1" },
        },
        (...ended) => resolve(exited.then(() => ended)),
      );
      if (child.pid === undefined) return;
      exited = new Promise((exit) => child.once("exit", exit)).then(() =>
        gone(),
      );
      try {
        gone = this.#owned.started(child.pid, args, resource);
      } catch {
        child.kill("SIGKILL");
      }
    });
  }

  // One call `#outstanding` started: refused while a rate limit holds its
  // resource's requests back, otherwise run once it has a turn -- unless a
  // limit met while it waited holds it back then. Its bound runs from before
  // its turn, so waiting counts toward it; a call that ends while waiting
  // never reaches GitHub. A call is never run, and its answer never read,
  // once `gh` asks with another login than these calls are asked under:
  // that answer is nobody's here, teaches no limit and is told to no one.
  async #spawned(args, signal, maxBufferBytes, resource) {
    const admission = this.#admissions.of(resource);
    const refused = this.#admissions.refusal(resource);
    if (refused !== undefined) {
      return unasked(refused);
    }
    let turnEnds;
    try {
      turnEnds = await admission.turn(signal);
    } catch {
      if (endedAtBound(signal)) {
        throw new GhFailure({ kind: "timed-out" });
      }
      return unasked({ kind: "failed" });
    }
    const heldBack = this.#admissions.refusal(resource);
    if (heldBack !== undefined) {
      turnEnds();
      return unasked(heldBack);
    }
    // A turn can pass to this call while every requester is being ended, as
    // when their owner closes: a call already ended is never run.
    if (!(await this.#loginStands()) || signal.aborted) {
      turnEnds();
      return unasked({ kind: "failed" });
    }
    const askedAt = new Date().toISOString();
    this.#running += 1;
    const [error, stdout, stderr] = await this.#ran(
      args,
      signal,
      maxBufferBytes,
      resource,
    );
    this.#running -= 1;
    // However the call ended, admission learns of it before the turn passes
    // on, so a limit this answer starts holds back the request that takes
    // the turn next, and an ending without one reopens the turns.
    const answered = !signal.aborted && (await this.#loginStands());
    if (!answered || signal.aborted) {
      admission.endedUnanswered(askedAt);
      this.#admissions.keep(resource);
      turnEnds();
      if (endedAtBound(signal)) {
        throw new GhFailure({ kind: "timed-out" });
      }
      // Ordinary departure, shutdown or another login ended the call: not a
      // retained retryable failure, and not classified as an upstream answer.
      return unasked({ kind: "failed" }, askedAt);
    }
    const learned = admission.answered(
      printedAnswer(error, stdout, stderr, askedAt),
      Date.now(),
    );
    // A rate limit is answered only once its wait is kept: otherwise the
    // call fails, and tells nothing of the limit.
    const answer =
      this.#admissions.keep(resource) ||
      learned.failure?.kind !== "rate-limited"
        ? learned
        : unasked({ kind: "failed" }, askedAt);
    if (answer.failure !== undefined) {
      this.#failed(answer);
    }
    turnEnds();
    return answer;
  }
}
