// Establish or reuse the publishing coordinator's CI observation for managed
// delivery: its owner, the live observer that owner claimed, host-bridge
// readiness, start, and coordinator binding. A Cursor or Claude Code
// coordinator's owner is resolved before any observer is considered, so a
// sibling's observer of the same repository and target is never reused.
// Codex is observed only by the yielded stream its coordinator armed and
// retained: delivery verifies that exact directory against the coordinator's
// claim and never selects a stream by repository and target. Interrupted
// registration recovers through execution-increment-observation-recovery.mjs.
import {
  bindHostObserver,
  hostSessionOwner,
  resolveHostSession,
  verifyHostBridge,
} from "./ci-host-bridge.mjs";
import { receiptPrefix, startExecutionMailbox } from "./ci-mailbox.mjs";
import {
  classifyOwnedObservation,
  classifyRetainedStream,
} from "./ci-mailbox-match.mjs";
import { codexStreamOwner } from "./ci-observer-owner.mjs";

export function coverageGap(reason, extras = {}) {
  return {
    state: "unobserved",
    pendingCi: "unobserved",
    reason,
    ...extras,
  };
}

function observationAttached(directory, { reused = false } = {}) {
  return {
    state: reused ? "reused" : "attached",
    directory,
    reused,
  };
}

// `deliver`, `resume`, and `finish` never start, adopt, or replace a Codex
// observer: the stream this coordinator armed and retained is the only one
// any of them registers on. Each gap names the input that recovers observation.
function codexArming({ repo, branch }, command) {
  return `arm \`ci-mailbox.mjs stream --execution ${repo} ${branch} --coordinator <value>\` in a yielded cell as references/ci-notify-codex.md describes, retain its receipt directory in the observer note, and the next ${command} registers on it`;
}

const activities = { deliver: "delivery", resume: "resume", finish: "closure" };

const retainedStreamGaps = {
  missing: (directory) => `${directory} is not a CI observer`,
  foreign: (directory) =>
    `${directory} belongs to another coordinator or repository`,
  "wrong-target": (directory, { observed }) =>
    `${directory} observes ${observed.repo ?? "no repository"} ${observed.branch ?? ""}`.trimEnd(),
  detached: (directory) => `${directory} is not a yielded stream`,
  unclaimed: (directory) =>
    `the stream at ${directory} was armed without --coordinator, so its owner cannot be verified; consume its delivered failures and stop it before rearming`,
  ended: (directory, { terminal }) =>
    `this coordinator's stream at ${directory} ended (${terminal.status})`,
  lost: (directory) =>
    `this coordinator's stream at ${directory} lost its worker`,
  unavailable: (directory) =>
    `this coordinator's stream at ${directory} is not live`,
  // Classified live, then found without a live worker by its caller.
  live: (directory) =>
    `this coordinator's stream at ${directory} stopped during this command`,
};

// Classifies the stream a Codex coordinator retained, with the `gap` to
// report when `command` cannot use it. The owner is computed from
// `ownerRoot`, which may outlive the checkout `root` the stream was armed in.
export function retainedStream({
  repo,
  branch,
  coordinator,
  observerDirectory,
  root,
  ownerRoot = root,
  storage,
  command = "deliver",
}) {
  const target = { repo, branch };
  const absent = [
    ["--coordinator", coordinator],
    ["--observer-directory", observerDirectory],
  ]
    .filter(([, value]) => !value)
    .map(([flag]) => flag);
  if (absent.length > 0) {
    return {
      kind: "unidentified",
      gap: () =>
        coverageGap(
          `Codex ${activities[command]} registers only on the stream this coordinator retained, and ${absent.join(" and ")} ${absent.length > 1 ? "were" : "was"} not supplied; pass --coordinator <value> and --observer-directory <directory> from the observer note, or without a retained stream ${codexArming(target, command)}`,
          { ownership: "unidentified" },
        ),
    };
  }
  const stream = classifyRetainedStream({
    directory: observerDirectory,
    owner: codexStreamOwner({ root: ownerRoot, coordinator }),
    ...target,
    root,
    storage,
  });
  return {
    ...stream,
    gap: () =>
      coverageGap(
        `${retainedStreamGaps[stream.kind](observerDirectory, stream)}, not this coordinator's live stream of ${repo} ${branch}; pass the --observer-directory its observer note retained with its --coordinator, or without one ${codexArming(target, command)}`,
        // Only this coordinator's own stream carries a directory.
        { ownership: stream.kind, directory: stream.directory },
      ),
  };
}

export function retainedStreamObservation(request) {
  const stream = retainedStream(request);
  return stream.kind === "live"
    ? observationAttached(stream.directory, { reused: true })
    : stream.gap();
}

// One coordinator holding several live observers of a target cannot say which
// one a registration belongs on; none is chosen for it.
export function ambiguousOwnerReason(
  { repo, branch },
  directories,
  command = "deliver",
) {
  return `this coordinator owns ${directories.length} live observers of ${repo} ${branch} (${directories.join(", ")}); keep the one whose directory it retained, stop the others with \`ci-mailbox.mjs stop <directory>\`, and the next ${command} reuses it`;
}

export async function establishObservation({
  repo,
  branch,
  host,
  session,
  coordinator,
  observerDirectory,
  workspace,
  runtime,
  maxDurationMs,
  env,
  root,
  storage,
}) {
  if (host === "codex") {
    return {
      observation: retainedStreamObservation({
        repo,
        branch,
        coordinator,
        observerDirectory,
        root,
        storage,
      }),
      startReceipt: null,
    };
  }

  // One resolved owner for selection, readiness, and binding.
  const hostSession = resolveHostSession({ host, session, env });
  const owner = hostSessionOwner({ host, session: hostSession, root });
  const owned = classifyOwnedObservation({
    repo,
    branch,
    owner,
    root,
    storage,
  });
  if (owned.kind === "live") {
    return {
      observation: observationAttached(owned.directory, { reused: true }),
      startReceipt: null,
    };
  }
  if (owned.kind === "ambiguous") {
    return {
      observation: coverageGap(
        ambiguousOwnerReason({ repo, branch }, owned.directories),
        { ownership: owned.kind, directories: owned.directories },
      ),
      startReceipt: null,
    };
  }

  const bridge = await verifyHostBridge({
    host,
    session: hostSession,
    workspace,
    hookPath: runtime.hookEntrypoint,
    env,
    root,
    storage,
  });
  if (!bridge.ready) {
    return {
      observation: coverageGap(
        bridge.reason ?? "host bridge unavailable",
        owner ? {} : { ownership: "unidentified" },
      ),
      startReceipt: null,
    };
  }

  const directory = await startExecutionMailbox(
    {
      mode: "execution",
      repo,
      branch,
      maxDurationMs,
    },
    { root, storage, env },
  );
  const startReceipt = `${receiptPrefix}${JSON.stringify({ directory })}\n`;
  const binding = await bindHostObserver({
    host,
    session: hostSession,
    receipt: startReceipt,
    workspace,
    hookPath: runtime.hookEntrypoint,
    env,
  });
  if (!binding.attached) {
    return {
      observation: coverageGap(
        binding.context || "host bridge did not attach the observer",
      ),
      directory,
      startReceipt,
    };
  }
  return {
    observation: observationAttached(directory),
    startReceipt,
  };
}
