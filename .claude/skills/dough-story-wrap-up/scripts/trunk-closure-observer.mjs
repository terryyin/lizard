// The execution's own CI observers for Trunk Mode `finish`. Closure consumes
// the owner evidence delivery and resume do: a Cursor or Claude Code
// coordinator's session, or a Codex coordinator's retained stream. That owner
// filters the observers before coverage or liveness is read, so closure never
// registers on, completes, or stops another coordinator's observer. The owner
// is computed from the repository's common Git directory, the identity every
// worktree of it shares, so a rerun after the execution worktree was retired
// names the same owner from the recorded management context.
import { realpathSync } from "node:fs";
import {
  hostSessionOwner,
  missingIdentityReason,
  resolveHostSession,
} from "../../dough-execute-plan/scripts/ci-host-bridge.mjs";
import {
  classifyOwnedObservation,
  isLiveMatchingMailbox,
  listOwnedMailboxes,
} from "../../dough-execute-plan/scripts/ci-mailbox-match.mjs";
import { listRegisteredRevisions } from "../../dough-execute-plan/scripts/ci-mailbox-revision-coverage.mjs";
import {
  coverageGap,
  retainedStream,
} from "../../dough-execute-plan/scripts/execution-increment-observation.mjs";
import { managementContext } from "../../dough-execute-plan/scripts/publication-git.mjs";

const notLive = {
  ended: "ended",
  lost: "lost its worker",
  unavailable: "is not live",
};

// Why none of a host coordinator's observers carries the final closure.
// `candidates` are the several that could, when the choice is ambiguous.
function hostGap({ target, owner, candidates }) {
  const where = `${target.repo} ${target.branch}`;
  if (candidates.length > 1) {
    return coverageGap(
      `this coordinator owns ${candidates.length} observers of ${where} that could carry the final closure (${candidates.join(", ")}); none is chosen for it`,
      { ownership: "ambiguous", directories: candidates },
    );
  }
  const owned = classifyOwnedObservation({ ...target, owner });
  if (owned.kind === "missing") {
    return coverageGap(
      `this coordinator holds no observer of ${where}; pass --session-json naming the session that armed this execution's observer when that is not this one`,
      { ownership: "missing" },
    );
  }
  const directories = owned.directories ?? [owned.directory];
  return coverageGap(
    `this coordinator's observer of ${where} at ${directories.join(", ")} ${notLive[owned.kind]} without registering the final closure`,
    { ownership: owned.kind, directories },
  );
}

// `directories` are the owner's observers in any state. `select` returns the
// one that covers `sha`, preferring a live one; else the owner's one live
// observer, which has yet to register it; otherwise the coverage `gap`.
function ownedObservers(directories, target, gap) {
  return {
    directories,
    select(sha) {
      const live = directories.filter((directory) =>
        isLiveMatchingMailbox(directory, target),
      );
      const covering = directories.filter((directory) =>
        listRegisteredRevisions(directory).includes(sha.toLowerCase()),
      );
      const candidates =
        [
          covering.filter((directory) => live.includes(directory)),
          covering,
          covering.length ? [] : live,
        ].find((found) => found.length > 0) ?? [];
      return candidates.length === 1
        ? { directory: candidates[0] }
        : { gap: gap(candidates) };
    },
  };
}

// The observers of `repo` and target `branch` this execution's owner evidence
// names. `inspection` is the execution worktree, or the recorded management
// context once that worktree is gone; `root` is the checkout the observers
// were started for.
export async function closureObservers({
  inspection,
  repo,
  branch,
  host,
  session,
  coordinator,
  observerDirectory,
  env,
  root,
  storage,
}) {
  const target = { repo, branch, root, storage };
  // A path that is not a checkout is its own identity, so the common Git
  // directory names the owner its worktrees' observers were claimed for.
  const ownerRoot = realpathSync(await managementContext(inspection));
  if (host === "codex") {
    const stream = retainedStream({
      ...target,
      ownerRoot,
      coordinator,
      observerDirectory,
      command: "finish",
    });
    // Only this coordinator's own stream carries a directory, in any state.
    return ownedObservers(
      stream.directory ? [stream.directory] : [],
      target,
      stream.gap,
    );
  }
  const owner = hostSessionOwner({
    host,
    session: resolveHostSession({ host, session, env }),
    root: ownerRoot,
  });
  if (!owner) {
    return ownedObservers([], target, () =>
      coverageGap(missingIdentityReason(host, "finish"), {
        ownership: "unidentified",
      }),
    );
  }
  return ownedObservers(
    listOwnedMailboxes({ ...target, owner }),
    target,
    (candidates) => hostGap({ target, owner, candidates }),
  );
}
