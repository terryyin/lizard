// Recover the publishing coordinator's CI observation for an interrupted
// registration. Resume consumes the owner evidence delivery does: a Cursor or
// Claude Code coordinator's session, or a Codex coordinator's retained stream.
// That owner filters the observers before any liveness is read, so a live
// sibling never stands in for an owner that is missing, ended, lost, or
// ambiguous. Resume never starts or adopts an observer; each gap names what
// recovers observation.
import {
  hostSessionOwner,
  missingIdentityReason,
  resolveHostSession,
} from "./ci-host-bridge.mjs";
import {
  classifyOwnedObservation,
  listMatchingMailboxes,
} from "./ci-mailbox-match.mjs";
import {
  ambiguousOwnerReason,
  coverageGap,
  retainedStreamObservation,
} from "./execution-increment-observation.mjs";

const recovered = (directory) => ({
  state: "recovered",
  directory,
  reused: true,
});

// A host coordinator's observer is established only by its own `deliver`.
const hostRecovery =
  "resume starts no observer: this coordinator's next `deliver` establishes its own, and rerunning this resume then registers the accepted revision on it";

const ownedGaps = {
  missing: ({ target: { repo, branch }, others }) =>
    `this coordinator holds no observer of ${repo} ${branch}${
      others > 0
        ? `, and the ${others} unclaimed or other coordinators' observer${others > 1 ? "s" : ""} of it ${others > 1 ? "are" : "is"} not adopted`
        : ""
    }; pass --session-json naming the session that armed this execution's observer when that is not this one; otherwise ${hostRecovery}`,
  ended: ({ owned: { directory, terminal } }) =>
    `this coordinator's observer at ${directory} ended (${terminal.status}); ${hostRecovery}`,
  lost: ({ owned: { directory, terminal } }) =>
    `this coordinator's observer at ${directory} lost its worker (${terminal.coverage?.reason ?? "no terminal result"}); ${hostRecovery}`,
  unavailable: ({ owned: { directories } }) =>
    `this coordinator's observer at ${directories.join(", ")} is not live; ${hostRecovery}`,
  ambiguous: ({ target, owned: { directories } }) =>
    ambiguousOwnerReason(target, directories, "resume"),
};

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
  if (owned.kind === "live") return recovered(owned.directory);
  const others =
    owned.kind === "missing"
      ? listMatchingMailboxes({ ...target, root, storage }).length
      : 0;
  // Only this coordinator's own observers are named.
  return coverageGap(ownedGaps[owned.kind]({ target, owned, others }), {
    ownership: owned.kind,
    directory: owned.directory,
    directories: owned.directories,
  });
}
