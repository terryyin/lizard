// The coverage gap a Cursor or Claude Code coordinator's own observers leave,
// in one vocabulary for `deliver`, `resume`, and `finish`: what each
// classification of those observers means, then the step the command names to
// recover observation. Only that coordinator's observers are named.
import { ownObserverRecovery } from "./ci-host-bridge.mjs";

// A host coordinator's observer is established only by its own `deliver`,
// which does so itself wherever `resume` and `finish` report these gaps.
const establishesOwn = {
  deliver:
    "this coordinator's next `deliver` establishes its own and registers the accepted revision on it",
  resume:
    "resume starts no observer: this coordinator's next `deliver` establishes its own, and rerunning this resume then registers the accepted revision on it",
  finish:
    "this coordinator's next `deliver` from the execution worktree establishes its own, and rerunning this finish then completes the final closure on it; once that worktree is gone, report the final closure's coverage as lost",
};
const establish = (command) => establishesOwn[command];

// Each kind `classifyOwnedObservation` returns: its reason, and its recovery
// step for a command.
const ownerGaps = {
  missing: {
    reason: ({ repo, branch, host, others }) =>
      `this coordinator holds no observer of ${repo} ${branch}${
        others > 0
          ? `, and the ${others} unclaimed or other coordinators' observer${others > 1 ? "s" : ""} of it ${others > 1 ? "are" : "is"} not adopted`
          : ""
      }; ${ownObserverRecovery(host)}`,
    recovery: establish,
  },
  ended: {
    reason: ({ owned: { directory, terminal } }) =>
      `this coordinator's observer at ${directory} ended (${terminal.status})`,
    recovery: establish,
  },
  lost: {
    reason: ({ owned: { directory, terminal } }) =>
      `this coordinator's observer at ${directory} lost its worker (${terminal.coverage?.reason ?? "no terminal result"})`,
    recovery: establish,
  },
  unavailable: {
    reason: ({ owned: { directories } }) =>
      `this coordinator's observer at ${directories.join(", ")} is not live`,
    recovery: establish,
  },
  // One coordinator holding several live observers of a target cannot say
  // which one a registration belongs on; none is chosen for it.
  ambiguous: {
    reason: ({ repo, branch, owned: { directories } }) =>
      `this coordinator owns ${directories.length} live observers of ${repo} ${branch} (${directories.join(", ")})`,
    recovery: (command) =>
      `keep the one whose directory it retained, stop the others with \`ci-mailbox.mjs stop <directory>\`, and the next ${command} reuses it`,
  },
  // Classified live only after its command had found no live observer.
  live: {
    reason: ({ owned: { directory }, command }) =>
      `this coordinator's observer at ${directory} went live while this ${command} ran`,
    recovery: (command) => `rerunning this ${command} registers on it`,
  },
};

// Why `command` leaves the target `repo` `branch` unobserved when `owned`
// classifies its `host` coordinator's observers. `others` counts the
// observers of that target the coordinator does not own. `registered` are
// those of its observers that each registered the revision `command` closes
// and are no longer live, with none of them chosen for it: the one whose
// record holds no CI verdict for that revision, or the several.
export function ownerGapReason(
  command,
  { repo, branch, host, owned, others = 0, registered = [] },
) {
  const { reason, recovery } = ownerGaps[owned.kind];
  const unchosen =
    registered.length > 1
      ? `; ${registered.length} of its observers each registered this revision and none is live (${registered.join(", ")})`
      : registered.length === 1
        ? `; its observer at ${registered[0]} registered this revision and its record holds no CI verdict for it`
        : "";
  return `${reason({ repo, branch, host, owned, others, command })}${unchosen}; ${recovery(command)}`;
}
