// Locate a live execution observer that already matches repository, target
// branch, and checkout. Ended or dead workers are not reusable owners.
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { join } from "node:path";
import {
  checkoutRoot,
  mailboxRoot,
  mailboxWorkerLoss,
  readMailbox,
  readWorkerIdentity,
} from "./ci-mailbox.mjs";
import { checkMailboxWorkerLiveness } from "./ci-mailbox-worker-process.mjs";

export function listMailboxDirectories(storage = mailboxRoot) {
  if (!existsSync(storage)) return [];
  return readdirSync(storage)
    .filter((name) => /^watch-/.test(name))
    .map((name) => join(storage, name));
}

function matchesExecutionContext(
  directory,
  { repo, branch, root = checkoutRoot, storage = mailboxRoot } = {},
) {
  let request;
  try {
    request = readMailbox(directory, root, storage);
  } catch {
    return false;
  }
  if (request.probe) return false;
  if (request.mode !== "execution") return false;
  return request.repo === repo && request.branch === branch;
}

export function readMailboxTerminal(directory) {
  const path = join(directory, "result.json");
  if (!existsSync(path)) return null;
  return JSON.parse(readFileSync(path, "utf8"));
}

export function isLiveMatchingMailbox(
  directory,
  { repo, branch, root = checkoutRoot, storage = mailboxRoot } = {},
) {
  if (!matchesExecutionContext(directory, { repo, branch, root, storage }))
    return false;
  if (existsSync(join(directory, "result.json"))) return false;
  if (mailboxWorkerLoss(directory)) return false;
  let identity;
  try {
    identity = readWorkerIdentity(directory);
  } catch {
    return false;
  }
  return checkMailboxWorkerLiveness(identity, directory) === "alive";
}

export function findLiveMatchingMailbox({
  repo,
  branch,
  root = checkoutRoot,
  storage = mailboxRoot,
} = {}) {
  return listMailboxDirectories(storage).find((directory) =>
    isLiveMatchingMailbox(directory, { repo, branch, root, storage }),
  );
}

function matchingDirectories({ repo, branch, root, storage }) {
  return listMailboxDirectories(storage).filter((directory) =>
    matchesExecutionContext(directory, { repo, branch, root, storage }),
  );
}

// Classify resume ownership without starting a replacement observer.
// Live coverage requires exactly one live match; ended/lost/ambiguous stop.
export function classifyMatchingObservationOwnership({
  repo,
  branch,
  root = checkoutRoot,
  storage = mailboxRoot,
} = {}) {
  const matches = matchingDirectories({ repo, branch, root, storage });
  if (matches.length === 0) {
    return {
      kind: "missing",
      reason: "no matching execution observer for resume",
    };
  }

  const live = matches.filter((directory) =>
    isLiveMatchingMailbox(directory, { repo, branch, root, storage }),
  );
  if (live.length === 1) {
    return { kind: "live", directory: live[0] };
  }
  if (live.length > 1) {
    return {
      kind: "ambiguous",
      reason: "ambiguous matching live observers for resume",
      directories: live,
    };
  }

  // Prefer an explicit ended terminal over inventing worker-loss language.
  for (const directory of matches) {
    const terminal = readMailboxTerminal(directory);
    if (terminal && terminal.coverage?.state !== "lost") {
      return {
        kind: "ended",
        directory,
        terminal,
        reason: `matching observer ended (${terminal.status})`,
      };
    }
  }

  for (const directory of matches) {
    const lost = mailboxWorkerLoss(directory);
    if (lost) {
      return {
        kind: "lost",
        directory,
        terminal: lost,
        reason: lost.coverage?.reason ?? "matching observer lost its worker",
      };
    }
  }

  return {
    kind: "unavailable",
    reason: "matching observer is not live for resume",
    directories: matches,
  };
}
