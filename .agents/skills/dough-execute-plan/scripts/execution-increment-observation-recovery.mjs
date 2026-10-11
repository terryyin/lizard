// Recover the publishing coordinator's CI observation for an interrupted
// registration. Resume consumes the owner evidence delivery does: a Cursor or
// Claude Code coordinator's session, or a Codex coordinator's retained stream.
// That owner filters the observers before any liveness is read, so a live
// sibling never stands in for an owner that is missing, ended, lost, or
// ambiguous. Resume never starts or adopts an observer; each gap names what
// recovers observation.
import {
  eventRecipient,
  hostSessionOwner,
  missingIdentityReason,
  resolveHostSession,
} from "./ci-host-bridge.mjs";
import {
  classifyOwnedObservation,
  listMatchingMailboxes,
} from "./ci-mailbox-match.mjs";
import {
  coverageGap,
  retainedStreamObservation,
} from "./execution-increment-observation.mjs";
import { ownerGapReason } from "./execution-increment-observation-gaps.mjs";

// `notifies` is the host session that receives this observer's events.
const recovered = (directory, notifies) => ({
  state: "recovered",
  directory,
  reused: true,
  ...(notifies && { notifies }),
});

export function recoverObservationForResume({
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
  const target = { repo, branch };
  if (host === "codex") {
    const stream = retainedStreamObservation({
      ...target,
      coordinator,
      observerDirectory,
      root,
      storage,
      command: "resume",
    });
    return stream.state === "unobserved" ? stream : recovered(stream.directory);
  }

  const owner = hostSessionOwner({
    host,
    session: resolveHostSession({ host, session, env }),
    root,
  });
  if (!owner) {
    return coverageGap(missingIdentityReason(host, "resume"), {
      ownership: "unidentified",
    });
  }
  const owned = classifyOwnedObservation({ ...target, owner, root, storage });
  if (owned.kind === "live")
    return recovered(owned.directory, eventRecipient({ host, session, env }));
  const others =
    owned.kind === "missing"
      ? listMatchingMailboxes({ ...target, root, storage }).length
      : 0;
  return coverageGap(
    ownerGapReason("resume", { ...target, host, owned, others }),
    {
      ownership: owned.kind,
      directory: owned.directory,
      directories: owned.directories,
    },
  );
}
