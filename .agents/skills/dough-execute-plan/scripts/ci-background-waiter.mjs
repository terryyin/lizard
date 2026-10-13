import { existsSync, readFileSync, readdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";
import {
  checkoutRoot,
  mailboxRoot,
  readMailbox,
} from "./ci-mailbox-location.mjs";
import {
  readDeliveryProgress,
  readMailboxEvents,
  readWorkerIdentity,
} from "./ci-mailbox-store.mjs";
import { readRevisionCoverage } from "./ci-mailbox-revision-coverage.mjs";
import { checkMailboxWorkerLiveness } from "./ci-mailbox-worker-process.mjs";
import { watchMailboxChanges } from "./ci-mailbox-change-watch.mjs";
import { hostInputOwner, readOwnerClaim } from "./ci-observer-owner.mjs";
import { recordedVerdict } from "./ci-mailbox-await.mjs";
import { currentProcessIdentity } from "./ci-background-session.mjs";
import {
  readBackgroundHandoff,
  saveBackgroundHandoff,
  backgroundCancellationRequested,
  backgroundSessionLimit,
} from "./ci-background-state.mjs";

async function waitOnMailbox(directory, owner) {
  const changes = watchMailboxChanges(directory);
  try {
    while (true) {
      // Arm before reading so a publication during the reads cannot be lost.
      const changed = changes.armChangeWake();
      const handoff = readBackgroundHandoff(directory);
      if (
        !handoff ||
        handoff.owner !== owner ||
        handoff.state !== "pending" ||
        existsSync(join(directory, "background-attempt.json"))
      )
        return;
      const settle = (state, extras = {}) =>
        saveBackgroundHandoff(directory, { ...handoff, state, ...extras });
      const interruption = backgroundCancellationRequested(directory)
        ? "cancelled"
        : backgroundSessionLimit(handoff);
      if (interruption) {
        settle(interruption, { ci: "unobserved" });
        return;
      }
      const registered = new Set(
        readRevisionCoverage(directory).map(({ sha }) => sha),
      );
      const failure = readMailboxEvents(
        directory,
        readDeliveryProgress(directory).deliveredThrough,
      ).find(
        ({ event }) => event.type === "CI_FAILURE" && registered.has(event.sha),
      );
      if (failure) {
        // Exclusive durable claim precedes native output. A killed or broken
        // writer retains uncertainty and is never blindly retried.
        try {
          writeFileSync(
            join(directory, "background-attempt.json"),
            JSON.stringify(failure),
            {
              flag: "wx",
              mode: 0o600,
            },
          );
        } catch (error) {
          if (error.code === "EEXIST") return;
          throw error;
        }
        if (backgroundCancellationRequested(directory)) {
          settle("cancelled", { ci: "unobserved" });
          return;
        }
        settle("delivery_uncertain", {
          ci: "failure",
          sequence: failure.sequence,
          reason:
            "Native feedback has no delivery acknowledgment; inspect this session and retained event before any manual retry",
        });
        return {
          directory,
          message: `dough-execute-plan CI observer (diagnostic data):\n${JSON.stringify(failure.event)}\nHandle CI failures using dough-execute-plan/references/ci-monitor.md. Keep the existing repair authority and execution context.`,
        };
      }
      if (recordedVerdict(directory, handoff.requestedSha) === "success") {
        settle("success", { ci: "success" });
        return;
      }
      if (
        existsSync(join(directory, "result.json")) ||
        checkMailboxWorkerLiveness(readWorkerIdentity(directory), directory) !==
          "alive"
      ) {
        settle("observation_unavailable", { ci: "unobserved" });
        return;
      }
      // File changes deliver events promptly. The bounded fallback detects a
      // departed process and tolerates a missed filesystem notification.
      try {
        await pause(
          Math.min(1000, Math.max(1, handoff.deadlineAt - Date.now())),
          undefined,
          {
            signal: changed,
          },
        );
      } catch (error) {
        if (!changed.aborted) throw error;
      }
    }
  } finally {
    changes.close();
  }
}

export async function waitForBackgroundFailure(
  input,
  { root = checkoutRoot, storage = mailboxRoot } = {},
) {
  if (input.cursor_version || input.hook_event_name !== "Stop")
    return undefined;
  const owner = hostInputOwner(input, "claude", root);
  const bindings = join(storage, `owner-${owner}`);
  if (!owner || !existsSync(bindings)) return undefined;
  const directories = readdirSync(bindings).map((name) =>
    readFileSync(join(bindings, name), "utf8"),
  );
  for (const directory of directories) {
    readMailbox(directory, root, storage);
    if (readOwnerClaim(directory) !== owner) continue;
    const enrolled = readBackgroundHandoff(directory);
    if (!enrolled || enrolled.owner !== owner || enrolled.state !== "pending")
      continue;
    try {
      writeFileSync(
        join(directory, "background-waiter.json"),
        JSON.stringify(currentProcessIdentity()),
        {
          flag: "wx",
          mode: 0o600,
        },
      );
    } catch (error) {
      if (error.code === "EEXIST") continue;
      throw error;
    }
    const feedback = await waitOnMailbox(directory, owner);
    if (feedback) return feedback;
  }
}
