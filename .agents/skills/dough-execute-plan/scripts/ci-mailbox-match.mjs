// Locate execution observers that match repository, target branch, and
// checkout, and select among them by the coordinator's owner claim. Matching
// is discovery; only a verified claim selects an observer for its coordinator.
// Ended or dead workers are not reusable owners.
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { join, resolve } from "node:path";
import {
  checkoutRoot,
  mailboxRoot,
  mailboxWorkerLoss,
  readMailbox,
  readWorkerIdentity,
} from "./ci-mailbox.mjs";
import { checkMailboxWorkerLiveness } from "./ci-mailbox-worker-process.mjs";
import { readOwnerClaim } from "./ci-observer-owner.mjs";

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
  return observesTarget(request, { repo, branch });
}

function observesTarget(request, { repo, branch }) {
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

// Matching execution observers in any state: live, ended, or lost.
export function listMatchingMailboxes({ repo, branch, root, storage }) {
  return listMailboxDirectories(storage).filter((directory) =>
    matchesExecutionContext(directory, { repo, branch, root, storage }),
  );
}

// Matching execution observers `owner` holds the claim on, in any state. An
// unclaimed observer, or one another coordinator claimed, is never returned.
export function listOwnedMailboxes({ repo, branch, owner, root, storage }) {
  if (!owner) return [];
  return listMatchingMailboxes({ repo, branch, root, storage }).filter(
    (directory) => readOwnerClaim(directory) === owner,
  );
}

// Classify one coordinator's observation: its claim filters the candidates
// before any liveness is read, so a live sibling never stands in for it.
export function classifyOwnedObservation({
  repo,
  branch,
  owner,
  root = checkoutRoot,
  storage = mailboxRoot,
} = {}) {
  return classifyObservers(
    listOwnedMailboxes({ repo, branch, owner, root, storage }),
    { repo, branch, root, storage },
  );
}

// Classify the exact yielded stream a Codex coordinator retained. The
// directory must be an observer of this repository and target that a stream
// command runs and `owner` claimed; only then is its liveness read. A sibling
// stream, a detached worker, or a stream armed without a coordinator is never
// classified live for this owner.
export function classifyRetainedStream({
  directory,
  owner,
  repo,
  branch,
  root = checkoutRoot,
  storage = mailboxRoot,
} = {}) {
  const retained = resolve(directory);
  let request;
  try {
    request = readMailbox(retained, root, storage);
  } catch (error) {
    return { kind: error.code === "ENOENT" ? "missing" : "foreign" };
  }
  if (!observesTarget(request, { repo, branch }))
    return { kind: "wrong-target", observed: request };
  if (workerMode(retained) !== "stream") return { kind: "detached" };
  const claim = readOwnerClaim(retained);
  if (claim === undefined) return { kind: "unclaimed" };
  if (claim !== owner) return { kind: "foreign" };
  return {
    ...classifyObservers([retained], { repo, branch, root, storage }),
    directory: retained,
  };
}

function workerMode(directory) {
  try {
    return readWorkerIdentity(directory).mode;
  } catch {
    return undefined;
  }
}

// Exactly one live observer is usable. Several live ones are ambiguous;
// otherwise an explicit ended terminal, then a lost worker, describes the gap.
function classifyObservers(matches, { repo, branch, root, storage }) {
  if (matches.length === 0) {
    return { kind: "missing" };
  }

  const live = matches.filter((directory) =>
    isLiveMatchingMailbox(directory, { repo, branch, root, storage }),
  );
  if (live.length === 1) {
    return { kind: "live", directory: live[0] };
  }
  if (live.length > 1) {
    return { kind: "ambiguous", directories: live };
  }

  // Prefer an explicit ended terminal over inventing worker-loss language.
  for (const directory of matches) {
    const terminal = readMailboxTerminal(directory);
    if (terminal && terminal.coverage?.state !== "lost") {
      return { kind: "ended", directory, terminal };
    }
  }

  for (const directory of matches) {
    const lost = mailboxWorkerLoss(directory);
    if (lost) {
      return { kind: "lost", directory, terminal: lost };
    }
  }

  return { kind: "unavailable", directories: matches };
}
