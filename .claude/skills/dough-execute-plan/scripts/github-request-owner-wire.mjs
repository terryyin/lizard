// What a consumer process and this machine's GitHub request owner
// (`./github-request-owner.mjs`) say to each other over its private endpoint
// (`./github-request-owner-endpoint.mjs`): one JSON message a line. A
// consumer greets with the protocol and the owner's caller token, then asks
// the one fixed operation -- a numbered `gh api` read -- or ends a wait it
// began; the owner answers each numbered read once. Each read names the API
// host and login it is asked under as its endpoint directory is named. No
// credential, command or path is ever said.

import { lstatSync } from "node:fs";
import { connect } from "node:net";
import { basename } from "node:path";

/** @import { Socket } from "node:net" */
/** @import { GhAnswer } from "./github-request-answer.mjs" */

// What an owner and its consumers must agree on to be spoken to at all.
export const protocol = 2;

// How long a greeting may take, for the owner awaiting it and for the
// consumer awaiting its welcome, before either ends the connection.
export const greetingLimitMs = 5_000;

// The most one answer may print, and the longest one call may run.
const answerBytesLimit = 64 * 1024 * 1024;
const callBoundLimitMs = 10 * 60_000;

/** @param {Socket} socket */
export function send(socket, message) {
  if (!socket.destroyed) socket.write(`${JSON.stringify(message)}\n`);
}

// Reads each line `socket` receives as one JSON message; a line that is not
// JSON, or longer than `limitBytes`, ends the connection.
/**
 * @param {Socket} socket
 * @param {(message: any) => void} onMessage
 */
export function receive(socket, onMessage, limitBytes = Infinity) {
  let parts = [];
  let size = 0;
  socket.on("data", (chunk) => {
    let from = 0;
    for (
      let end = chunk.indexOf(10);
      end !== -1 && !socket.destroyed;
      end = chunk.indexOf(10, from)
    ) {
      parts.push(chunk.subarray(from, end));
      from = end + 1;
      let message;
      try {
        message = JSON.parse(Buffer.concat(parts).toString("utf8"));
      } catch {
        socket.destroy();
        return;
      }
      parts = [];
      size = 0;
      onMessage(message);
    }
    parts.push(chunk.subarray(from));
    size += chunk.length - from;
    if (size > limitBytes) socket.destroy();
  });
}

// Whether anything accepts a connection at `path`.
/** @param {string} path */
export const reachable = (path) =>
  new Promise((resolve) => {
    const socket = connect(path);
    socket.once("connect", () => {
      socket.destroy();
      resolve(true);
    });
    socket.once("error", () => resolve(false));
  });

// The owner `record` names, greeted: its connection, or undefined when
// nothing answers there. Throws when it is another user's or refuses this
// caller. `onMessage` hears what it says after its welcome.
export function greeted({ socket: path, token }, onMessage) {
  return new Promise((resolve, reject) => {
    try {
      if (lstatSync(path).uid !== process.getuid?.())
        throw new Error("The GitHub request owner is another user's.");
    } catch (error) {
      if (error.code === "ENOENT") resolve(undefined);
      else reject(error);
      return;
    }
    const socket = connect(path);
    const gone = () => {
      clearTimeout(limit);
      resolve(undefined);
    };
    const limit = setTimeout(() => socket.destroy(), greetingLimitMs);
    let welcomed = false;
    socket.on("error", () => undefined);
    socket.once("close", gone);
    socket.once("connect", () => send(socket, { hello: protocol, token }));
    receive(socket, (message) => {
      if (welcomed) {
        onMessage(message);
        return;
      }
      clearTimeout(limit);
      socket.off("close", gone);
      if (message.welcome === protocol) {
        welcomed = true;
        resolve(socket);
        return;
      }
      socket.destroy();
      reject(new Error("The GitHub request owner refused this caller."));
    });
  });
}

/** @param {GhAnswer} answer */
export const wireAnswer = (answer) => ({
  ...answer,
  headers: [...answer.headers],
});

/** @returns {GhAnswer} */
export const answerFromWire = ({
  status,
  headers,
  body,
  failure,
  askedAt,
}) => ({
  status,
  headers: new Map(headers),
  body,
  failure,
  askedAt,
});

// How a read names the API host and login whose endpoint `directory` is.
/** @param {string} directory */
export const scopeOf = (directory) => basename(directory);

const flagsWithValue = new Set(["-H", "--header", "-q", "--jq"]);
const bounded = (value, limit) =>
  Number.isSafeInteger(value) && value > 0 && value <= limit;

// The routes a `gh api` call's arguments name, past the flags a read may
// carry; undefined when they carry any other flag.
/** @param {readonly string[]} args */
export function readRoutes(args) {
  const routes = [];
  for (let at = 1; at < args.length; at += 1) {
    if (flagsWithValue.has(args[at])) at += 1;
    else if (args[at].startsWith("-")) return undefined;
    else routes.push(args[at]);
  }
  return routes;
}

// The one operation the endpoint accepts: a `gh api` read of one route of
// GitHub's API -- never another command, a local file, another host or a
// change -- with how long its call may run and how much it may print.
// `scope` names the API host and login it is asked under. Undefined for
// anything else.
export function fixedRequest(request) {
  const { args, boundMs, maxBufferBytes, scope } = request ?? {};
  if (
    typeof scope !== "string" ||
    !Array.isArray(args) ||
    args.length > 64 ||
    args.some((arg) => typeof arg !== "string" || arg.includes("\0")) ||
    args[0] !== "api" ||
    !bounded(boundMs, callBoundLimitMs) ||
    !bounded(maxBufferBytes, answerBytesLimit)
  )
    return undefined;
  const routes = readRoutes(args);
  if (
    routes?.length !== 1 ||
    routes[0].includes("://") ||
    routes[0].includes("{")
  )
    return undefined;
  return { args, boundMs, maxBufferBytes, scope };
}
