// One process's private connection to the GitHub request owner of its host
// and login, found by its record and started when none answers. Reads may
// be asked again after a login change. An action requires the owner's
// explicit capability and is never resent. Unreachable or incompatible
// owners leave requests unanswered, never run through a fallback process.
import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import {
  EndpointBinding,
  standingRecord,
} from "./github-request-owner-endpoint.mjs";
import {
  greeted,
  scopeOf,
  supportsWorkflowRerun,
} from "./github-request-owner-endpoint-connection.mjs";
import { send } from "./github-request-owner-wire.mjs";
import {
  OwnerUnavailable,
  ownerFailure,
} from "./github-request-owner-failure.mjs";

/** @import { Socket } from "node:net" */

const ownerEntry = fileURLToPath(
  new URL("./github-request-owner.mjs", import.meta.url),
);

// How long finding or starting the owner may take before the requests
// still waiting for it go unanswered -- each gives up at its own bound
// before that.
const startLimitMs = 30_000;

const pause = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// What `read` reads, or undefined while it cannot be read: asked again.
function readable(read) {
  try {
    return read();
  } catch {
    return undefined;
  }
}

/**
 * @typedef {{
 *   readonly left?: true;
 *   readonly timedOut?: true;
 *   readonly answer?: any;
 *   readonly told?: boolean;
 *   readonly refused?: string;
 *   readonly failure?: import("./github-request-owner-failure.mjs").OwnerUnavailableFailure;
 *   readonly owner?: OwnerIdentity;
 * }} OwnerReply
 */

// The owner a request was sent to, as its standing record names it: its
// process, generation and the protocol it reports speaking, never its caller
// token or socket path.
/** @typedef {{ readonly pid?: number; readonly generation: number; readonly protocol?: number }} OwnerIdentity */

export class OwnerLink {
  #backoffBaseMs;

  #binding = new EndpointBinding();

  // The connection to the owner this process last asked, and each request
  // still waiting for that owner's reply, by its number.
  /** @type {{ readonly directory: string; connecting?: Promise<Socket>; owner?: OwnerIdentity; readonly waiting: Map<number, (reply: OwnerReply) => void> } | undefined} */
  #link;

  #asked = 0;

  // `backoffBaseMs` is given to an owner this process starts: its first
  // wait after a rate limit that directs none.
  /** @param {{ readonly backoffBaseMs: number }} options */
  constructor({ backoffBaseMs }) {
    this.#backoffBaseMs = backoffBaseMs;
  }

  // How a request names the API host and login this process asks under
  // now, without a secret; undefined while their endpoint cannot be
  // established.
  /** @returns {Promise<string | undefined>} */
  async scope() {
    const directory = await this.#binding.directory().catch(() => undefined);
    return directory && scopeOf(directory);
  }

  // The owner's reply to one request: its `answer`, and whether the call
  // failed (`told`); the `owner` it was sent to, once sent; `timedOut` when
  // the call reached its own bound; `left` once `signal` ended this wait,
  // which tells the owner so, at this requester's own bound when
  // `leavingAtBound` says so; a safe `failure` when the owner cannot be
  // asked. Reads may retry after a login change; actions never do.
  /**
   * @param {{ readonly args?: readonly string[]; readonly operation?: string; readonly repository?: string; readonly runId?: number; readonly boundMs: number; readonly maxBufferBytes: number }} request
   * @param {AbortSignal} signal
   * @param {() => boolean} leavingAtBound
   * @returns {Promise<OwnerReply>}
   */
  async ask(request, signal, leavingAtBound) {
    try {
      for (;;) {
        const directory = await this.#binding.directory();
        const asked = { ...request, scope: scopeOf(directory) };
        const reply = await this.#askedUnder(
          directory,
          asked,
          signal,
          leavingAtBound,
        );
        // Actions never enter the read loop that retries under a new login.
        if (request.operation !== undefined || reply.left || reply.timedOut)
          return reply;
        if (directory === (await this.#binding.directory())) return reply;
      }
    } catch (error) {
      return { failure: ownerFailure(error) };
    }
  }

  #askedUnder(directory, request, signal, leavingAtBound) {
    return new Promise((resolve) => {
      if (signal.aborted) {
        resolve({ left: true });
        return;
      }
      const link = this.#linked(directory);
      let sent;
      const settle = (reply) => {
        signal.removeEventListener("abort", leave);
        if (sent !== undefined) {
          link.waiting.delete(sent.id);
          if (link.waiting.size === 0) sent.socket.unref();
        }
        resolve(sent?.owner ? { ...reply, owner: sent.owner } : reply);
      };
      const leave = () => {
        if (sent !== undefined)
          send(sent.socket, {
            id: sent.id,
            cancel: leavingAtBound() ? "bound" : "left",
          });
        settle({ left: true });
      };
      signal.addEventListener("abort", leave, { once: true });
      this.#connected(directory).then(
        (socket) => {
          if (signal.aborted) return;
          if (
            request.operation !== undefined &&
            !supportsWorkflowRerun(socket)
          ) {
            settle({ refused: "operation-unavailable" });
            return;
          }
          this.#asked += 1;
          sent = { id: this.#asked, socket, owner: link.owner };
          link.waiting.set(sent.id, settle);
          socket.ref();
          send(socket, { id: sent.id, request });
        },
        (error) => settle({ failure: ownerFailure(error) }),
      );
    });
  }

  // The link to the owner standing in `directory`. The link to another
  // login's owner ends first, which leaves each request waiting there
  // without a reply.
  #linked(directory) {
    if (this.#link?.directory !== directory) {
      this.#link?.connecting?.then(
        (socket) => socket.destroy(),
        () => undefined,
      );
      this.#link = { directory, waiting: new Map() };
    }
    return this.#link;
  }

  // The standing owner's connection, shared by this process's requests.
  #connected(directory) {
    const link = this.#linked(directory);
    link.connecting ??= this.#connect(link).then(
      ({ socket, record: { pid, generation, protocol } }) => {
        link.owner = { pid, generation, protocol };
        socket.unref();
        socket.once("close", () => {
          link.connecting = undefined;
          for (const settle of [...link.waiting.values()])
            settle({
              failure: {
                kind: "owner-unavailable",
                cause: "connection-lost",
              },
            });
        });
        return socket;
      },
      (error) => {
        link.connecting = undefined;
        throw error;
      },
    );
    return link.connecting;
  }

  // The connection to the owner standing in `directory`, and its record.
  async #connect({ directory, waiting }) {
    const deadline = Date.now() + startLimitMs;
    let starting;
    for (;;) {
      const record = readable(() => standingRecord(directory));
      const socket =
        record?.socket === undefined
          ? undefined
          : await greeted(record, ({ id, ...reply }) =>
              waiting.get(id)?.(reply),
            );
      if (socket !== undefined) return { socket, record };
      if (starting?.failure) throw starting.failure;
      if (starting?.process.signalCode || starting?.process.exitCode)
        throw new OwnerUnavailable("start-failed");
      if (Date.now() > deadline) throw new OwnerUnavailable("unknown");
      if (starting === undefined || starting.process.exitCode !== null)
        starting = this.#started(directory);
      await pause(20);
    }
  }

  // A detached owner runs only its installed sibling, without this process's
  // output or Node options.
  #started(directory) {
    const env = { ...process.env };
    delete env.NODE_OPTIONS;
    const owner = spawn(
      process.execPath,
      [ownerEntry, directory, String(this.#backoffBaseMs)],
      { cwd: directory, env, detached: true, stdio: "ignore" },
    );
    const started = { process: owner, failure: undefined };
    owner.on("error", () => {
      started.failure = new OwnerUnavailable("start-failed");
    });
    owner.unref();
    return started;
  }
}
