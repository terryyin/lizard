// The one owner of this user's requests to GitHub under one API host and
// login, for every Open Dough consumer whose `gh` asks with them: started by
// the first consumer that finds none (`./github-request-owner-link.mjs`), in
// its own process group, with that consumer's environment for every `gh` it
// runs. It holds the one record of a rate limit, the eight turns at GitHub
// and every outstanding call (`./github-request-calls.mjs`), so consumers in
// separate processes share them, and consumers under another host or login
// share none of them. It stands while its record is the highest in its
// endpoint directory (`./github-request-owner-endpoint.mjs`), asking nothing
// once it is not, and ends once no consumer is connected and no rate limit's
// wait stands for `DOUGH_GITHUB_REQUEST_OWNER_IDLE_MS` (five seconds), or
// ten seconds after its start when no consumer ever reached it. While its
// own `gh` asks with another login than its endpoint's it asks GitHub
// nothing, and stays only for a rate limit's wait. An owner that replaces
// one that was killed begins with the waits that one kept
// (`./github-request-kept-admissions.mjs`), and asks GitHub nothing before
// each `gh` that one left running is ended or counted as a turn of its own
// (`./github-request-owned-calls.mjs`).
//
// Usage: node github-request-owner.mjs <endpoint directory> <backoff base ms>
import { randomBytes, timingSafeEqual } from "node:crypto";
import { lstatSync, rmSync } from "node:fs";
import { createServer } from "node:net";
import { join } from "node:path";
import { GhFailure } from "./github-request-answer.mjs";
import { AdmittedCalls } from "./github-request-calls.mjs";
import { KeptAdmissions } from "./github-request-kept-admissions.mjs";
import {
  earlierCallsEnded,
  OwnedCalls,
} from "./github-request-owned-calls.mjs";
import {
  claimedRecord,
  EndpointBinding,
  newSocketName,
  privacyRefusal,
  standingRecord,
} from "./github-request-owner-endpoint.mjs";
import {
  fixedRequest,
  greetingLimitMs,
  protocol,
  reachable,
  receive,
  scopeOf,
  send,
  wireAnswer,
} from "./github-request-owner-wire.mjs";

const directory = process.argv[2] ?? "";
const backoffBaseMs = Number(process.argv[3]);
const idleMs = Number(process.env.DOUGH_GITHUB_REQUEST_OWNER_IDLE_MS ?? 5_000);
// The longest one consumer message may be.
const messageBytesLimit = 256 * 1024;

if (
  !(backoffBaseMs > 0) ||
  privacyRefusal(lstatSync(directory), process.getuid?.()) !== undefined
)
  process.exit(1);

const earlier = standingRecord(directory);
if (earlier?.socket !== undefined && (await reachable(earlier.socket)))
  process.exit(0);

// The endpoint of the API host and login this process's own `gh` asks with
// now; undefined while it cannot be established.
const binding = new EndpointBinding();
const ownEndpoint = () => binding.directory().catch(() => undefined);
const loginLeft = async () => {
  const own = await ownEndpoint();
  return own !== undefined && own !== directory;
};
const scope = scopeOf(directory);

const token = randomBytes(32).toString("hex");
const admitted = (offered) =>
  typeof offered === "string" &&
  offered.length === token.length &&
  timingSafeEqual(Buffer.from(offered), Buffer.from(token));

// Every connected consumer, and every requester waiting on a call.
const consumers = new Set();
const requesters = new Set();
const left = new Error("The requester left.");
let ending = false;

// One consumer process: greeted with this owner's token, then asking the one
// fixed operation or ending a wait it began. Its disconnection ends its own
// waits only. An owner already ending greets nobody, so the consumer finds or
// starts the next one.
function serve(socket) {
  if (ending) {
    socket.destroy();
    return;
  }
  const waits = new Map();
  let welcomed = false;
  consumers.add(socket);
  const greeting = setTimeout(() => socket.destroy(), greetingLimitMs);
  const refuse = (refused) => {
    send(socket, { refused });
    socket.end();
  };
  socket.on("error", () => undefined);
  socket.on("close", () => {
    clearTimeout(greeting);
    consumers.delete(socket);
    if (welcomed) idleSince = Date.now();
    for (const wait of waits.values()) wait.abort(left);
  });
  receive(
    socket,
    (message) => {
      if (!welcomed) {
        if (!admitted(message?.token)) refuse("unauthenticated");
        else if (message.hello !== protocol) refuse("incompatible");
        else {
          welcomed = true;
          clearTimeout(greeting);
          send(socket, { welcome: protocol });
        }
        return;
      }
      const { id, cancel, request } = message ?? {};
      if (!Number.isSafeInteger(id)) refuse("unknown operation");
      else if (cancel === "left" || cancel === "bound")
        waits
          .get(id)
          ?.abort(
            cancel === "left" ? left : new GhFailure({ kind: "timed-out" }),
          );
      else if (request === undefined || waits.has(id))
        refuse("unknown operation");
      else ask(socket, waits, id, fixedRequest(request));
    },
    messageBytesLimit,
  );
}

function ask(socket, waits, id, fixed) {
  if (fixed === undefined || fixed.scope !== scope) {
    const refused =
      fixed === undefined ? "not a GitHub API read" : "another login";
    send(socket, { id, refused });
    return;
  }
  const wait = new AbortController();
  waits.set(id, wait);
  requesters.add(wait);
  earlierEnded
    .then(() => calls.request(fixed.args, wait.signal, fixed))
    .then(
      (answer) =>
        send(socket, {
          id,
          answer: wireAnswer(answer),
          told: failedAnswers.has(answer),
        }),
      (error) => {
        if (wait.signal.aborted) return;
        const timedOut =
          error instanceof GhFailure && error.reason.kind === "timed-out";
        send(socket, timedOut ? { id, timedOut } : { id });
      },
    )
    .finally(() => {
      waits.delete(id);
      requesters.delete(wait);
    });
}

const socketFile = newSocketName();
const socketPath = join(directory, socketFile);
const server = createServer(serve);
await new Promise((resolve, reject) => {
  server.once("error", reject);
  server.listen(socketPath, resolve);
});

// Ends every call no later requester could reach, then -- once each `gh` it
// ran is gone, or two seconds on -- this process. Its record stays, so no
// later owner takes its generation.
function end() {
  if (ending) return;
  ending = true;
  for (const wait of requesters) wait.abort(left);
  server.close();
  for (const consumer of consumers) consumer.destroy();
  rmSync(socketPath, { force: true });
  const lastAt = Date.now() + 2_000;
  setInterval(() => {
    if (calls.running === 0 || Date.now() > lastAt) process.exit(0);
  }, 10);
}

// An owner that claimed the generation first stands instead: this one was
// never spoken to.
const generation = (earlier?.generation ?? 0) + 1;
const record = { pid: process.pid, protocol, token, socket: socketFile };
if (!claimedRecord(directory, generation, record)) {
  rmSync(socketPath, { force: true });
  process.exit(0);
}

// Every failed call's answer, so each consumer waiting on it is told so.
const failedAnswers = new WeakSet();
const calls = new AdmittedCalls({
  admissions: new KeptAdmissions(directory, backoffBaseMs),
  owned: new OwnedCalls(directory, generation),
  loginStands: async () => stands() && (await ownEndpoint()) === directory,
  failed: (answer) => failedAnswers.add(answer),
});
// No call is asked before each `gh` an earlier owner left running is ended,
// or holds one of this owner's turns until it is gone.
const earlierEnded = earlierCallsEnded(directory, generation).then((left) => {
  for (const { resource, gone } of left) calls.occupied(resource, gone);
});
earlierEnded.catch(end);

// Whether this owner's record is still the highest: withdrawn, overtaken or
// without its directory, it stands no more. A directory that cannot be read
// just now proves none of those.
function stands() {
  try {
    return standingRecord(directory)?.generation === generation;
  } catch (error) {
    return error.code !== "ENOENT";
  }
}

// The consumer that started this owner may take this long to reach it:
// until then it is not idle. Once a consumer leaves, it is idle from then.
const firstConsumerLimitMs = 10_000;
let idleSince = Date.now() + firstConsumerLimitMs;
setInterval(
  async () => {
    const now = Date.now();
    if (!stands()) end();
    else if (calls.heldBack(now) !== undefined) idleSince = now;
    else if (await loginLeft()) end();
    else if (consumers.size > 0) idleSince = now;
    else if (now - idleSince >= idleMs) end();
  },
  Math.min(idleMs, 1_000),
);
for (const signal of ["SIGTERM", "SIGINT", "SIGHUP"]) process.on(signal, end);
