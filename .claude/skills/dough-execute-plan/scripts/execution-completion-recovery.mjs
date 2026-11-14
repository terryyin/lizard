// Recover completion for an accepted SHA when this coordinator's observer of
// the authorized target ended before a completion receipt. Composes live
// recovery or establishObservation, registers the SHA when needed, and runs
// completeRevision for one ordinary CI_OBSERVER receipt. Same owner input as
// resume; never starts a Codex stream.
import { resolve } from "node:path";
import { resolveCheckoutRuntime } from "./ci-checkout-runtime.mjs";
import {
  hostSessionOwner,
  missingIdentityReason,
  resolveHostSession,
  withExplicitSession,
} from "./ci-host-bridge.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import {
  completeRevision,
  mailboxRoot,
  receiptPrefix,
  registerPushedRevision,
} from "./ci-mailbox.mjs";
import {
  isLiveMatchingMailbox,
  listOwnedMailboxes,
} from "./ci-mailbox-match.mjs";
import { writePredecessors } from "./ci-mailbox-predecessors.mjs";
import { listRegisteredRevisions } from "./ci-mailbox-revision-records.mjs";
import { codexStreamOwner } from "./ci-observer-owner.mjs";
import {
  coverageGap,
  establishObservation,
} from "./execution-increment-observation.mjs";
import { targetBranchName } from "./publication-git.mjs";
import { executionBudgetMs } from "./watch-ci-execution.mjs";

function endedPredecessors({ repo, branch, owner, root, storage }) {
  return listOwnedMailboxes({ repo, branch, owner, root, storage }).filter(
    (directory) =>
      !isLiveMatchingMailbox(directory, { repo, branch, root, storage }),
  );
}

// Records ended predecessors for a replacement, including a fresh stream
// that the Codex coordinator has already armed.
async function recoverObservationForCompletion(request) {
  const {
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
  } = request;

  const hostSession = resolveHostSession({ host, session, env });
  const owner =
    host === "codex"
      ? codexStreamOwner({ root, coordinator })
      : hostSessionOwner({ host, session: hostSession, root });
  if (!owner && host !== "codex") {
    return coverageGap(missingIdentityReason(host, "complete"), {
      ownership: "unidentified",
    });
  }
  const predecessors = endedPredecessors({
    repo,
    branch,
    owner,
    root,
    storage,
  }).filter((directory) => directory !== observerDirectory);

  const established = await establishObservation({
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
    command: "complete",
  });
  if (
    established.observation.state !== "unobserved" &&
    established.observation.directory &&
    (host === "codex" || !established.observation.reused) &&
    predecessors.length > 0
  ) {
    writePredecessors(established.observation.directory, predecessors);
  }
  return established.observation;
}

export async function recoverExecutionCompletion(request) {
  const {
    workspace,
    acceptedSha,
    targetRef,
    repo,
    host = "cursor",
    preferredAlias,
    session,
    coordinator,
    observerDirectory,
    env = process.env,
    root,
    storage,
    maxDurationMs = executionBudgetMs,
    now,
    sleep,
    deadlineMs,
    cancellation,
  } = request;

  for (const field of ["workspace", "acceptedSha", "targetRef", "repo"]) {
    if (!request[field]) {
      return {
        ok: false,
        error: `missing ${field}`,
        receipt: null,
        observation: null,
      };
    }
  }

  const runtime = resolveCheckoutRuntime(workspace, { host, preferredAlias });
  const observerRoot = root ?? runtime.checkout;
  const observerStorage = storage ?? mailboxRoot;
  const targetBranch = targetBranchName(targetRef);
  const observation = await recoverObservationForCompletion({
    repo,
    branch: targetBranch,
    host,
    session,
    coordinator,
    observerDirectory,
    workspace,
    runtime,
    maxDurationMs,
    env,
    root: observerRoot,
    storage: observerStorage,
  });
  if (observation.state === "unobserved" || !observation.directory) {
    return { ok: false, observation, receipt: null };
  }

  const directory = observation.directory;
  if (!listRegisteredRevisions(directory).includes(acceptedSha.toLowerCase())) {
    registerPushedRevision(directory, acceptedSha);
  }

  const receipt = await completeRevision(directory, acceptedSha, {
    root: observerRoot,
    storage: observerStorage,
    ...(now ? { now } : {}),
    ...(sleep ? { sleep } : {}),
    ...(deadlineMs !== undefined ? { deadlineMs } : {}),
    ...(cancellation ? { cancellation } : {}),
  });
  return {
    ok: true,
    observation,
    receipt,
    runtime: {
      alias: runtime.alias,
      skillRoot: runtime.skillRoot,
      entrypoint: runtime.entrypoint,
    },
  };
}

function argumentsOf(argv) {
  if (argv[0] !== "complete") {
    throw new Error(
      "usage: execution-completion-recovery.mjs complete --workspace PATH --accepted-sha SHA --target-ref REF --repo OWNER/REPO [--host cursor|claude|codex] [--preferred-alias .agents|.claude] [--session-json JSON] [--coordinator VALUE --observer-directory PATH] [--max-duration-ms MS]",
    );
  }
  const result = {};
  for (let index = 1; index < argv.length; index += 1) {
    const flag = argv[index];
    if (!flag.startsWith("--") || index + 1 >= argv.length) {
      throw new Error(`invalid argument ${flag}`);
    }
    const key = flag
      .slice(2)
      .replace(/-[a-z]/g, (match) => match[1].toUpperCase());
    result[key] = argv[++index];
  }
  if (result.maxDurationMs !== undefined) {
    result.maxDurationMs = Number(result.maxDurationMs);
  }
  return withExplicitSession(result);
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const args = argumentsOf(process.argv.slice(2));
    const cancellation = new AbortController();
    const cancel = () => cancellation.abort();
    process.once("SIGINT", cancel);
    process.once("SIGTERM", cancel);
    try {
      const result = await recoverExecutionCompletion({
        ...args,
        workspace: resolve(args.workspace),
        cancellation: cancellation.signal,
      });
      if (result.receipt) {
        process.stdout.write(
          `${receiptPrefix}${JSON.stringify(result.receipt)}\n`,
        );
      } else {
        process.stdout.write(`${JSON.stringify(result)}\n`);
        process.exitCode = 1;
      }
    } finally {
      process.removeListener("SIGINT", cancel);
      process.removeListener("SIGTERM", cancel);
    }
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
