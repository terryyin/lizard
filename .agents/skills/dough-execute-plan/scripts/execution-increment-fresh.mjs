// One fresh attempt: preparation and commit must succeed before managed
// delivery can establish observation. Recovery consumes retained Git evidence.
import { commitPreparedExecutionIncrement } from "./execution-increment-preparation.mjs";
import { managedIncrementRequestRefusal } from "./execution-increment-request.mjs";
import { targetBranchName } from "./publication-git.mjs";

function publicationRefusal(request) {
  if (!["publish", "local-only"].includes(request.authority))
    return "fresh work requires explicit publish or local-only authority";
  if (request.authority === "local-only") return null;
  const refusal = managedIncrementRequestRefusal(request);
  if (refusal) return refusal;
  try {
    targetBranchName(request.targetRef);
  } catch (error) {
    return error.message;
  }
  return null;
}

function recoveryContext(request, candidateSha, suffixBase) {
  const context = { candidateSha, suffixBase };
  for (const field of [
    "workspace",
    "branch",
    "mode",
    "tracking",
    "remote",
    "targetRef",
    "repo",
    "host",
    "preferredAlias",
    "session",
    "coordinator",
    "observerDirectory",
    "defaultCheckout",
    "oneShotIdentity",
    "landingContext",
  ]) {
    if (request[field] !== undefined) context[field] = request[field];
  }
  return context;
}

export async function prepareAndDeliverFreshIncrement(request, deliver) {
  const refusal = publicationRefusal(request);
  if (refusal)
    return {
      ok: false,
      status: "preparation-refused",
      operation: "publication context",
      error: refusal,
      publication: "pending",
      commit: null,
      receipt: null,
      observation: null,
    };
  const commit = await commitPreparedExecutionIncrement(request);
  if (!commit.ok || request.authority === "local-only")
    return {
      ...commit,
      publication: "pending",
      report: commit.ok ? "committed-pending-publication" : commit.status,
      commit: commit.ok ? commit : null,
      receipt: null,
      observation: null,
      ...(!commit.ok && {
        recovery:
          "Repair the named operation, preserve owned edits and index content, and retry fresh preparation; renew only proof invalidated by the repair. Do not deliver an older HEAD after this failed attempt.",
      }),
    };

  // Retain each actual pre-push pair, including a proof-governed rewrite. A
  // later exception must not lose the only usable candidate/base for resume.
  let recovery = recoveryContext(
    request,
    commit.sha,
    request.previouslyPublishedBase,
  );
  let validationFailure;
  try {
    const delivered = await deliver({
      ...request,
      validatedCandidate: commit.sha,
      ...(request.validate && {
        validate: async (candidate, context) => {
          recovery = recoveryContext(request, candidate, context.suffixBase);
          try {
            return await request.validate(candidate, context);
          } catch (error) {
            validationFailure = { interrupted: true, error: error.message };
            throw error;
          }
        },
      }),
      beforePush: async (comparison, ...args) => {
        recovery = recoveryContext(
          request,
          comparison.candidate,
          comparison.suffixBase,
        );
        await request.beforePush?.(comparison, ...args);
      },
    });
    return {
      ...delivered,
      commit,
      recovery: {
        ...recovery,
        candidateSha:
          delivered.status === "candidate-mismatch"
            ? commit.sha
            : (delivered.receipt?.sha ??
              delivered.candidate ??
              recovery.candidateSha),
        suffixBase: delivered.suffixBase ?? recovery.suffixBase,
        observerDirectory:
          delivered.observation?.directory ?? recovery.observerDirectory,
      },
    };
  } catch (error) {
    return {
      ok: false,
      status: validationFailure ? "validation-failed" : "delivery-interrupted",
      report: validationFailure ? "validation-failed" : "delivery-interrupted",
      publication: validationFailure ? "stopped" : "unknown",
      ...(validationFailure && { validation: validationFailure }),
      error: error.message,
      commit,
      recovery,
      receipt: null,
      observation: null,
    };
  }
}
