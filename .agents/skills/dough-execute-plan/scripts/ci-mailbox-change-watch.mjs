import { existsSync, watch } from "node:fs";
import { join } from "node:path";
import { listRegisteredRevisions } from "./ci-mailbox-revision-records.mjs";

// Watches one mailbox for a stop request and for newly registered revisions.
// `stopSignal` aborts once stop is requested. Each `armRegistrationWake()`
// returns a signal that aborts when a revision unknown at arming time is
// registered, so a pending recheck pause ends early.
// `armChangeWake()` also wakes a local reader on any filesystem notification;
// that reader supplies its own bounded fallback for missed notifications.
export function watchMailboxChanges(directory) {
  const stop = new AbortController();
  let knownRevisions = new Set();
  let registrationWake = new AbortController();
  let changeWake = new AbortController();
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
    if (existsSync(join(directory, "stop"))) stop.abort();
    const registered = registeredNow();
    if (registered.some((sha) => !knownRevisions.has(sha))) {
      knownRevisions = new Set(registered);
      registrationWake.abort();
    }
  };
  const subscription = watch(directory, () => {
    changeWake.abort();
    noticeChange();
  });
  const fallback = setInterval(noticeChange, 100);
  fallback.unref();
  noticeChange();
  return {
    stopSignal: stop.signal,
    armRegistrationWake,
    armChangeWake,
    close: () => {
      subscription.close();
      clearInterval(fallback);
    },
  };
}
