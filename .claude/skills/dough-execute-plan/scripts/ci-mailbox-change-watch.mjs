import { existsSync, readdirSync, statSync } from "node:fs";
import { join } from "node:path";
import { listRegisteredRevisions } from "./ci-mailbox-revision-records.mjs";

const changePollMs = 100;

// What a reader of the mailbox can see change: each entry's name, size and
// modification time. A record directory's time moves as records are added.
function mailboxSnapshot(directory) {
  try {
    return readdirSync(directory)
      .sort()
      .map((name) => {
        try {
          const { size, mtimeMs } = statSync(join(directory, name));
          return `${name}:${size}:${mtimeMs}`;
        } catch {
          return name;
        }
      })
      .join("\n");
  } catch {
    return "";
  }
}

// Watches one mailbox for a stop request and for newly registered revisions.
// `stopSignal` aborts once stop is requested. Each `armRegistrationWake()`
// returns a signal that aborts when a revision unknown at arming time is
// registered, so a pending recheck pause ends early.
// `armChangeWake()` also wakes a local reader on any change to the mailbox;
// that reader supplies its own bounded fallback for a departed worker.
// The mailbox is polled, not watched: on macOS a busy fseventsd can hold or
// miss a directory notification, and closing a watch blocks the process for
// seconds, which would hold a stopped worker's exit.
export function watchMailboxChanges(directory) {
  const stop = new AbortController();
  let knownRevisions = new Set();
  let registrationWake = new AbortController();
  let changeWake = new AbortController();
  let snapshot = mailboxSnapshot(directory);
  const armChangeWake = () => {
    changeWake = new AbortController();
    return changeWake.signal;
  };
  // A listing that fails (the mailbox being removed, for example) wakes
  // nothing; the observer's own poll reads and reports coverage.
  const registeredNow = () => {
    try {
      return listRegisteredRevisions(directory);
    } catch {
      return [...knownRevisions];
    }
  };
  const armRegistrationWake = () => {
    knownRevisions = new Set(registeredNow());
    registrationWake = new AbortController();
    return registrationWake.signal;
  };
  const noticeChange = () => {
    const current = mailboxSnapshot(directory);
    if (current !== snapshot) {
      snapshot = current;
      changeWake.abort();
    }
    if (existsSync(join(directory, "stop"))) stop.abort();
    const registered = registeredNow();
    if (registered.some((sha) => !knownRevisions.has(sha))) {
      knownRevisions = new Set(registered);
      registrationWake.abort();
    }
  };
  const poll = setInterval(noticeChange, changePollMs);
  noticeChange();
  return {
    stopSignal: stop.signal,
    armRegistrationWake,
    armChangeWake,
    close: () => clearInterval(poll),
  };
}
