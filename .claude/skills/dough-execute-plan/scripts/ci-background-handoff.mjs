// One explicit live-Claude handoff on the existing owner-bound mailbox.
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
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
import { hostInputOwner, readOwnerClaim } from "./ci-observer-owner.mjs";
import { recordedVerdict } from "./ci-mailbox-await.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import {
  liveClaudeAncestor,
  liveClaudeStillRunning,
} from "./ci-background-session.mjs";
import {
  handoffPath,
  readBackgroundHandoff,
  saveBackgroundHandoff,
  backgroundCancellationRequested,
  backgroundSessionLimit,
} from "./ci-background-state.mjs";

export {
  currentProcessIdentity,
  liveClaudeAncestor,
  liveClaudeStillRunning,
} from "./ci-background-session.mjs";
export {
  readBackgroundHandoff,
  saveBackgroundHandoff,
} from "./ci-background-state.mjs";

export const handoffWindowMs = 10 * 60 * 1000;
export const backgroundReceiptPrefix = "CI_BACKGROUND_HANDOFF ";
export function validateBackgroundOwner(
  directory,
  session,
  { root = checkoutRoot, storage = mailboxRoot } = {},
) {
  const request = readMailbox(directory, root, storage);
  const owner = hostInputOwner(session, "claude", root);
  if (!owner || readOwnerClaim(directory) !== owner)
    throw new Error(
      "Background CI handoff requires this Claude coordinator's existing owner claim",
    );
  return { request, owner };
}
function verifyInstalledWaiter(root) {
  const settings = JSON.parse(
    readFileSync(join(root, ".claude/settings.json"), "utf8"),
  );
  if (
    settings.disableAllHooks ||
    !settings.hooks?.Stop?.some(
      (entry) =>
        !entry.matcher &&
        entry.hooks?.some(
          (hook) =>
            hook.type === "command" &&
            hook.asyncRewake === true &&
            hook.timeout >= 610 &&
            hook.command ===
              'node "$CLAUDE_PROJECT_DIR/.claude/skills/dough-execute-plan/scripts/ci-background-hook.mjs"',
        ),
    )
  )
    throw new Error(
      "The installed live-Claude asyncRewake Stop waiter is unavailable; use complete-revision",
    );
}
export function handoffPendingCi(directory, sha, session, options = {}) {
  const { root = checkoutRoot } = options;
  const { request, owner } = validateBackgroundOwner(
    directory,
    session,
    options,
  );
  const publication = readDeliveryProgress(directory).acceptedPublication;
  const revision = readRevisionCoverage(directory).find(
    (entry) => entry.sha === sha,
  );
  if (
    !publication ||
    publication.mode !== "story-branch" ||
    publication.sha !== sha ||
    publication.targetRef !== `refs/heads/${request.branch}` ||
    !revision
  )
    throw new Error(
      "Background handoff requires the exact latest accepted registered Story Branch publication",
    );
  if (
    request.mode !== "execution" ||
    readWorkerIdentity(directory).mode !== undefined ||
    existsSync(join(directory, "stop")) ||
    existsSync(join(directory, "result.json")) ||
    checkMailboxWorkerLiveness(readWorkerIdentity(directory), directory) !==
      "alive"
  )
    throw new Error(
      "Background handoff requires this coordinator's live detached observer",
    );
  const unreadFailure = readMailboxEvents(
    directory,
    readDeliveryProgress(directory).deliveredThrough,
  ).some(({ event }) => event.type === "CI_FAILURE");
  if (recordedVerdict(directory, sha) || unreadFailure)
    throw new Error(
      "CI is already resolved or has an unread failure; use the existing completion/repair path",
    );
  verifyInstalledWaiter(root);
  const liveSession = liveClaudeAncestor();
  if (!liveSession)
    throw new Error(
      "Background handoff requires the original still-live interactive Claude process",
    );
  const prior = readBackgroundHandoff(directory);
  if (prior) {
    if (prior.owner === owner && prior.requestedSha === sha) return prior;
    throw new Error(
      "This mailbox already has a background handoff; automatic rearming is unavailable",
    );
  }
  const handoff = {
    owner,
    requestedSha: sha,
    state: "pending",
    ci: "pending",
    deadlineAt: Date.now() + handoffWindowMs,
    liveSession,
    resources: "retained",
  };
  writeFileSync(handoffPath(directory), JSON.stringify(handoff), {
    flag: "wx",
    mode: 0o600,
  });
  return handoff;
}
export function backgroundStatus(directory, session, options = {}) {
  validateBackgroundOwner(directory, session, options);
  const handoff = readBackgroundHandoff(directory);
  if (!handoff) return { state: "not_enrolled", resources: "retained" };
  if (handoff.state !== "pending") return handoff;
  let state;
  if (backgroundCancellationRequested(directory)) state = "cancelled";
  else if (existsSync(join(directory, "background-attempt.json")))
    state = "delivery_uncertain";
  else state = backgroundSessionLimit(handoff);
  if (
    !state &&
    existsSync(join(directory, "background-waiter.json")) &&
    !liveClaudeStillRunning(
      JSON.parse(
        readFileSync(join(directory, "background-waiter.json"), "utf8"),
      ),
    )
  )
    state = "waiter_lost";
  if (state) {
    const settled = { ...handoff, state, ci: "unobserved" };
    saveBackgroundHandoff(directory, settled);
    return settled;
  }
  return handoff;
}
if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const [command, directory, sha, ...remaining] = process.argv.slice(2);
    const args =
      command === "handoff" ? remaining : [sha, ...remaining].filter(Boolean);
    const index = args.indexOf("--session-json");
    const session =
      index >= 0
        ? JSON.parse(args[index + 1])
        : { session_id: process.env.CLAUDE_CODE_SESSION_ID };
    let result;
    if (command === "handoff")
      result = handoffPendingCi(directory, sha, session);
    else if (command === "status")
      result = backgroundStatus(directory, session);
    else if (command === "cancel") {
      result = backgroundStatus(directory, session);
      if (result.state === "pending") {
        writeFileSync(join(directory, "background-cancel"), "", {
          mode: 0o600,
        });
        result = { ...result, state: "cancelled", ci: "unobserved" };
        saveBackgroundHandoff(directory, result);
      }
    } else
      throw new Error(
        "Usage: ci-background-handoff.mjs handoff DIRECTORY SHA | status DIRECTORY | cancel DIRECTORY [--session-json JSON]",
      );
    process.stdout.write(
      `${backgroundReceiptPrefix}${JSON.stringify({ directory, ...result })}\n`,
    );
  } catch (error) {
    process.stderr.write(
      `CI background handoff unavailable: ${String(error).slice(0, 600)}\n`,
    );
    process.exitCode = 1;
  }
}
