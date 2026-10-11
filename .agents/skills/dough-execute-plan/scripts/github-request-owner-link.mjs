// One process's connection to the GitHub request owner
// (`./github-request-owner.mjs`) of the API host and login its `gh` asks
// with, over that owner's private endpoint
// (`./github-request-owner-endpoint.mjs`): found by its standing record,
// started when none answers, greeted with its caller token, and kept for
// every request this process asks while its login stands. Once the login
// changes, whatever was asked under the earlier one is asked again under
// this one, and no answer to the earlier is passed on. An owner that cannot
// be reached or started, speaks another protocol or refuses the greeting
// leaves each request unanswered -- never asked of GitHub by this process
// itself, so no second owner of the allowance appears.
import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import {
  EndpointBinding,
  standingRecord,
} from "./github-request-owner-endpoint.mjs";
import { greeted, scopeOf, send } from "./github-request-owner-wire.mjs";

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
 * }} OwnerReply
 */

export class OwnerLink {
  #backoffBaseMs;

  #binding = new EndpointBinding();

  // The connection to the owner this process last asked, and each request
  // still waiting for that owner's reply, by its number.
  /** @type {{ readonly directory: string; connecting?: Promise<Socket>; readonly waiting: Map<number, (reply: OwnerReply) => void> } | undefined} */
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
    const directory = await this.#endpoint();
    return directory && scopeOf(directory);
  }

  #endpoint() {
    return this.#binding.directory().catch(() => undefined);
  }

  // The owner's reply to one request: its `answer`, and whether the call
  // failed (`told`); `timedOut` when the call reached its own bound; `left`
  // once `signal` ended this wait, which tells the owner so, at this
  // requester's own bound when `leavingAtBound` says so; and nothing when
  // the owner refused the request or cannot be asked. A reply to what was
  // asked under a login this process has no more is dropped, and the
  // request asked again under the one it has.
  /**
   * @param {{ readonly args: readonly string[]; readonly boundMs: number; readonly maxBufferBytes: number }} request
   * @param {AbortSignal} signal
   * @param {() => boolean} leavingAtBound
   * @returns {Promise<OwnerReply>}
   */
  async ask(request, signal, leavingAtBound) {
    for (;;) {
      const directory = await this.#endpoint();
      if (directory === undefined) return {};
      const asked = { ...request, scope: scopeOf(directory) };
      const reply = await this.#askedUnder(
        directory,
        asked,
        signal,
        leavingAtBound,
      );
      if (reply.left || reply.timedOut) return reply;
      if (directory === (await this.#endpoint())) return reply;
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
        resolve(reply);
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
          this.#asked += 1;
          sent = { id: this.#asked, socket };
          link.waiting.set(sent.id, settle);
          socket.ref();
          send(socket, { id: sent.id, request });
        },
        () => settle({}),
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

  // The connection of the link to the owner standing in `directory`, made
  // once for every request asked over it.
  #connected(directory) {
    const link = this.#linked(directory);
    link.connecting ??= this.#connect(link).then(
      (socket) => {
        socket.unref();
        socket.once("close", () => {
          link.connecting = undefined;
          for (const settle of [...link.waiting.values()]) settle({});
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
      if (socket !== undefined) return socket;
      if (Date.now() > deadline)
        throw new Error("The GitHub request owner did not start.");
      if (starting === undefined || starting.exitCode !== null)
        starting = this.#started(directory);
      await pause(20);
    }
  }

  // An owner of its own process group, outliving this process, with no
  // output and nothing of this process's Node options: it runs only what is
  // installed beside this file.
  #started(directory) {
    const env = { ...process.env };
    delete env.NODE_OPTIONS;
    const owner = spawn(
      process.execPath,
      [ownerEntry, directory, String(this.#backoffBaseMs)],
      { cwd: directory, env, detached: true, stdio: "ignore" },
    );
    owner.on("error", () => undefined);
    owner.unref();
    return owner;
  }
}
