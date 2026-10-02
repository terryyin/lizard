// Resume an accepted managed delivery without duplicate push or guessed
// observation. Verifies remote acceptance, recovers only a matching live
// owner, and reports an actionable gap when coverage cannot be restored.
import { resolve } from "node:path";
import { resolveCheckoutRuntime } from "./ci-checkout-runtime.mjs";
import {
  mailboxRoot,
  readRevisionCoverage,
  registerPushedRevision,
} from "./ci-mailbox.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { recoverObservationForResume } from "./execution-increment-observation.mjs";
import { stopped } from "./applicable-candidate-proof.mjs";
import { resumeInterruptedPublication } from "./publication-resume.mjs";
import { targetBranchName } from "./publication-git.mjs";

function hasCoverage(directory, sha) {
  const normalized = sha.toLowerCase();
  return readRevisionCoverage(directory).some(
    (entry) => entry.sha === normalized,
  );
}

export function observerAdapter(directory, targetRef) {
  const receipts = readRevisionCoverage(directory).map((entry) => ({
    sha: entry.sha,
    target: targetRef,
  }));
  return {
    bound: true,
    receipts,
    register(sha, target) {
      registerPushedRevision(directory, sha);
      receipts.push({ sha, target });
    },
  };
}

export async function resumeManagedExecutionIncrement(request) {
  const {
    workspace,
    candidateSha,
    targetRef,
    repo,
    supersededShas = [],
    publishedRevisions = [],
    defaultCheckout,
    host = "cursor",
    preferredAlias,
    root,
    storage,
    oneShotIdentity,
  } = request;

  for (const field of ["workspace", "candidateSha", "targetRef", "repo"]) {
    if (!request[field]) {
      return {
        ok: false,
        publication: "refused",
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
  const recovered = recoverObservationForResume({
    repo,
    branch: targetBranch,
    root: observerRoot,
    storage: observerStorage,
  });

  const liveDirectory =
    recovered.ownership.kind === "live" ? recovered.ownership.directory : null;
  const observer = liveDirectory
    ? observerAdapter(liveDirectory, targetRef)
    : null;

  // Queued one-shot work keeps its ownership guard when resume must push.
  const onFetchedTarget = oneShotIdentity
    ? (await import("./one-shot-ownership.mjs")).queuedOwnershipGuard({
        workspace,
        identity: oneShotIdentity,
      })
    : undefined;
  const published = await resumeInterruptedPublication({
    ownedWorkspace: workspace,
    defaultCheckout,
    candidateSha,
    supersededShas,
    publishedRevisions,
    observer,
    targetRef,
    onFetchedTarget,
  });
  if (published.held)
    return stopped(published.held.status, {
      candidate: candidateSha,
      pushCount: 0,
      classification: published.classification,
      ...published.held.fields,
      observation: recovered.observation,
    });

  // Ensure the accepted SHA is on the recovered live owner when resume's
  // classification completed registration or coverage was already present.
  if (liveDirectory && !hasCoverage(liveDirectory, published.acceptedSha)) {
    registerPushedRevision(liveDirectory, published.acceptedSha);
  }

  return {
    ok: true,
    publication: "accepted",
    report: "accepted",
    pushCount: published.pushCount,
    completedObligation: published.completedObligation,
    classification: published.classification,
    receipt: {
      sha: published.acceptedSha,
      target: targetRef,
    },
    observation: recovered.observation,
    runtime: {
      alias: runtime.alias,
      skillRoot: runtime.skillRoot,
      entrypoint: runtime.entrypoint,
    },
    maintenance: published.maintenance ?? null,
    registration: published.registration,
  };
}

function argumentsOf(argv) {
  if (argv[0] !== "resume") {
    throw new Error(
      "usage: execution-increment-resume.mjs resume --workspace PATH --candidate-sha SHA --target-ref REF --repo OWNER/REPO [--host cursor|claude|codex] [--preferred-alias .agents|.claude] [--default-checkout PATH] [--superseded-sha SHA]... [--one-shot-identity ID]",
    );
  }
  const result = { supersededShas: [], publishedRevisions: [] };
  for (let index = 1; index < argv.length; index += 1) {
    const flag = argv[index];
    if (flag === "--superseded-sha") {
      result.supersededShas.push(argv[++index]);
      continue;
    }
    if (!flag.startsWith("--") || index + 1 >= argv.length) {
      throw new Error(`invalid argument ${flag}`);
    }
    const key = flag
      .slice(2)
      .replace(/-[a-z]/g, (match) => match[1].toUpperCase());
    result[key] = argv[++index];
  }
  return result;
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const args = argumentsOf(process.argv.slice(2));
    const result = await resumeManagedExecutionIncrement({
      ...args,
      workspace: resolve(args.workspace),
      defaultCheckout: args.defaultCheckout
        ? resolve(args.defaultCheckout)
        : undefined,
    });
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
