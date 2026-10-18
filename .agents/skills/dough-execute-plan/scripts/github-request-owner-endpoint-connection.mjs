// Connections to this user's private GitHub request owner endpoint: whether
// its socket answers, whether its owner admits this caller, and the host/login
// scope its directory names. Wire encoding owns what is said over the socket.
import { lstatSync } from "node:fs";
import { connect } from "node:net";
import { basename, dirname } from "node:path";
import { OwnerUnavailable } from "./github-request-owner-failure.mjs";
import { shownProcessSync } from "./process-table.mjs";
import { protocol, receive, send } from "./github-request-owner-wire.mjs";

// How long a greeting may take, for the owner awaiting it and for the
// consumer awaiting its welcome, before either ends the connection.
export const greetingLimitMs = 5_000;

// How a read names the API host and login whose endpoint `directory` is.
/** @param {string} directory */
export const scopeOf = (directory) => basename(directory);

// A bare listening socket is not an installed owner. Only its recorded live
// process may be recovered automatically after an unanswered greeting.
function knownOwner(pid, socket) {
  if (!Number.isSafeInteger(pid) || pid < 1) return false;
  try {
    return (
      shownProcessSync(pid)?.command.includes(
        `/github-request-owner.mjs ${dirname(socket)} `,
      ) ?? false
    );
  } catch {
    return false;
  }
}

// The owner `record` names, greeted: its connection, or undefined when
// nothing answers there. Throws when it is another user's or refuses this
// caller. `onMessage` hears what it says after its welcome. Startup alone treats
// an unanswered greeting as absence for fenced replacement. Readers recover
// only a verified installed owner; other endpoints retain the diagnostic
// failure. An actual refusal always remains a refusal.
export function greeted(
  { socket: path, token, pid },
  onMessage,
  inspect = lstatSync,
  { unansweredIsAbsent = false } = {},
) {
  return new Promise((resolve, reject) => {
    try {
      if (inspect(path).uid !== process.getuid?.())
        throw new OwnerUnavailable("another-user");
    } catch (error) {
      if (error.code === "ENOENT") resolve(undefined);
      else reject(error);
      return;
    }
    const socket = connect(path);
    const gone = () => {
      clearTimeout(limit);
      if (unansweredIsAbsent || knownOwner(pid, path)) resolve(undefined);
      else reject(new OwnerUnavailable("connection-failed"));
    };
    const limit = setTimeout(() => {
      socket.off("close", gone);
      socket.destroy();
      if (unansweredIsAbsent || knownOwner(pid, path)) resolve(undefined);
      else reject(new OwnerUnavailable("unknown"));
    }, greetingLimitMs);
    let welcomed = false;
    socket.on("error", (error) => {
      socket.off("close", gone);
      clearTimeout(limit);
      if (error.code === "ENOENT" || error.code === "ECONNREFUSED")
        resolve(undefined);
      else reject(new OwnerUnavailable("connection-failed"));
    });
    socket.once("close", gone);
    socket.once("connect", () => send(socket, { hello: protocol, token }));
    receive(socket, (message) => {
      if (welcomed) {
        onMessage(message);
        return;
      }
      clearTimeout(limit);
      socket.off("close", gone);
      if (message?.welcome === protocol) {
        welcomed = true;
        resolve(socket);
        return;
      }
      socket.destroy();
      const cause = message?.refused;
      reject(
        new OwnerUnavailable(
          cause === "incompatible" || cause === "unauthenticated"
            ? cause
            : "unknown",
        ),
      );
    });
  });
}
