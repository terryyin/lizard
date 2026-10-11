// The execution's own CI observers for Trunk Mode `finish`. Closure consumes
// the owner evidence delivery and resume do: a Cursor or Claude Code
// coordinator's session, or a Codex coordinator's retained stream. That owner
// filters the observers before coverage or liveness is read, so closure never
// registers on, completes, or stops another coordinator's observer. The owner
// is computed from the execution checkout as delivery computes it. Once that
// worktree was retired, the repository's common Git directory, the identity
// its worktrees shared, names the same owner from the recorded management
// context, and reaches that owner's observer whichever worktree armed it.
import { existsSync, realpathSync } from "node:fs";
import {
  eventRecipient,
  hostSessionOwner,
  missingIdentityReason,
  resolveHostSession,
} from "../../dough-execute-plan/scripts/ci-host-bridge.mjs";
import { recordedVerdict } from "../../dough-execute-plan/scripts/ci-mailbox-await.mjs";
import {
  classifyOwnedObservation,
  listOwnedMailboxes,
} from "../../dough-execute-plan/scripts/ci-mailbox-match.mjs";
import { listRegisteredRevisions } from "../../dough-execute-plan/scripts/ci-mailbox-revision-coverage.mjs";
import {
  coverageGap,
  retainedStream,
} from "../../dough-execute-plan/scripts/execution-increment-observation.mjs";
import { ownerGapReason } from "../../dough-execute-plan/scripts/execution-increment-observation-gaps.mjs";
import { managementContext } from "../../dough-execute-plan/scripts/publication-git.mjs";

// One classification of a host coordinator's observers: those it finds
// `live`, and why none of them carries the final closure. `registered` are
// those that registered it and are no longer live.
function hostClassification({ target, host, owner }) {
  const owned = classifyOwnedObservation({ ...target, owner });
  return {
    live:
      { live: [owned.directory], ambiguous: owned.directories }[owned.kind] ??
      [],
    gap: (registered) =>
      coverageGap(
        ownerGapReason("finish", { ...target, host, owned, registered }),
        {
          ownership: owned.kind,
          directories:
            registered.length > 0
              ? registered
              : (owned.directories ?? (owned.directory && [owned.directory])),
        },
      ),
  };
}

// The one of `ended` observers to repeat completion of `sha` on: the first
// whose record holds its CI verdict, when every record that holds one holds
// the same. A record that holds none identifies no observer and counts
// against none; verdicts that differ identify none to trust.
function completedObserver(ended, sha) {
  const decided = ended
    .map((directory) => ({
      directory,
      verdict: recordedVerdict(directory, sha),
    }))
    .filter(({ verdict }) => verdict);
  return new Set(decided.map(({ verdict }) => verdict)).size === 1
    ? [decided[0].directory]
    : [];
}

// `directories` are the owner's observers in any state. `select` classifies
// them once, for the selection and its gap alike, and returns the live one
// that covers `sha`; else the owner's one live observer, which has yet to
// register it, whatever an ended one recorded; else, with none live, the
// `completedObserver` among the ended ones covering `sha`, one as several;
// otherwise the coverage `gap`, given those ended observers. An observer that
// went live after `directories` were listed is never selected.
// `notifies` is the host session that receives a selected observer's events.
function ownedObservers(directories, classify, notifies) {
  return {
    directories,
    select(sha) {
      const classified = classify();
      const live = directories.filter((directory) =>
        classified.live.includes(directory),
      );
      const covering = directories.filter((directory) =>
        listRegisteredRevisions(directory).includes(sha.toLowerCase()),
      );
      const ended = classified.live.length > 0 ? [] : covering;
      const candidates =
        [
          covering.filter((directory) => live.includes(directory)),
          live,
          completedObserver(ended, sha),
        ].find((found) => found.length > 0) ?? [];
      return candidates.length === 1
        ? { directory: candidates[0], ...(notifies && { notifies }) }
        : { gap: classified.gap(ended) };
    },
  };
}

// The checkout closure reads its observers through and computes their owner
// from. While the execution checkout exists it is that checkout, as in
// delivery. Once it is gone, `repository`'s common Git directory stands for
// the identity it shared with the repository's other worktrees, which is the
// identity the observers started there recorded.
export async function observerAccess(workspace, repository) {
  return realpathSync(
    existsSync(workspace) ? workspace : await managementContext(repository),
  );
}

// The observers of `repo` and target `branch` this execution's owner evidence
// names, read through and claimed for `root` as `observerAccess` gives it.
export function closureObservers({
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
  if (host === "codex") {
    const stream = retainedStream({
      ...target,
      coordinator,
      observerDirectory,
      command: "finish",
    });
    // Only this coordinator's own stream carries a directory, in any state.
    return ownedObservers(stream.directory ? [stream.directory] : [], () => ({
      live: stream.kind === "live" ? [stream.directory] : [],
      gap: stream.gap,
    }));
  }
  const owner = hostSessionOwner({
    host,
    session: resolveHostSession({ host, session, env }),
    root,
  });
  if (!owner) {
    return ownedObservers([], () => ({
      live: [],
      gap: () =>
        coverageGap(missingIdentityReason(host, "finish"), {
          ownership: "unidentified",
        }),
    }));
  }
  return ownedObservers(
    listOwnedMailboxes({ ...target, owner }),
    () => hostClassification({ target, host, owner }),
    eventRecipient({ host, session, env }),
  );
}
