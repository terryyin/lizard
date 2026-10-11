// Standalone launch-bound capture. Retained input survives the publication workspace.
import { readFile, writeFile, rename } from "node:fs/promises";
import { randomUUID } from "node:crypto";
import { fileURLToPath } from "node:url";
import path from "node:path";
import {
  deliverRetainedReport,
  loopbackReportingOrigin,
} from "./dashboard-completion.mjs";
const commit = /^(?:[a-f0-9]{40}|[a-f0-9]{64})$/;
function validate(submission) {
  const allowed = new Set([
    "origin",
    "source",
    "host",
    "reference",
    "identity",
    "remote",
    "target",
    "base",
    "revision",
    "delivery",
  ]);
  if (Object.keys(submission).some((key) => !allowed.has(key)))
    throw new Error("The retained landing is malformed.");
  for (const key of allowed)
    if (typeof submission[key] !== "string" || !submission[key])
      throw new Error(`Missing landing ${key}.`);
  if (
    !["claude", "codex", "cursor"].includes(submission.host) ||
    !commit.test(submission.base) ||
    !commit.test(submission.revision) ||
    !submission.target.startsWith("refs/heads/")
  )
    throw new Error(
      "A landing needs full commit IDs and its authorized branch target.",
    );
  loopbackReportingOrigin(submission.origin);
}
export async function retainLandingComparison(
  contextFile,
  { candidate, suffixBase },
  { remote, targetRef },
) {
  const context = JSON.parse(await readFile(contextFile, "utf8"));
  if (context.remote !== remote || context.target !== targetRef)
    throw new Error("Publication does not match the supplied landing context.");
  const submission = {
    ...context,
    base: suffixBase,
    revision: candidate,
    delivery: randomUUID(),
  };
  validate(submission);
  const directory = path.dirname(contextFile);
  const pending = path.join(directory, `landing-${submission.delivery}.json`);
  await writeFile(pending, `${JSON.stringify(submission, null, 2)}\n`, {
    flag: "wx",
    mode: 0o600,
  });
  // Replace the current handoff only after its complete exact request is durable.
  const current = path.join(directory, "landing-current.json");
  const temporary = `${current}.${submission.delivery}.tmp`;
  await writeFile(temporary, `${JSON.stringify({ pending })}\n`, {
    flag: "wx",
    mode: 0o600,
  });
  await rename(temporary, current);
  await submitLandingInput(pending, {}, true);
  return pending;
}
export async function currentLandingInput(contextFile) {
  return JSON.parse(
    await readFile(
      path.join(path.dirname(contextFile), "landing-current.json"),
      "utf8",
    ),
  ).pending;
}
export async function submitLandingInput(
  pending,
  expected = {},
  prepare = false,
) {
  const submission = JSON.parse(await readFile(pending, "utf8"));
  validate(submission);
  for (const name of [
    "origin",
    "source",
    "host",
    "reference",
    "identity",
    "remote",
    "target",
    "base",
    "revision",
  ])
    if (expected[name] !== undefined && expected[name] !== submission[name])
      throw new Error(`The retained landing does not match ${name}.`);
  return deliverRetainedReport(
    pending,
    submission,
    prepare ? "landing-prepare" : "landing",
    (receipt) =>
      [
        "delivery",
        "reference",
        "identity",
        "remote",
        "target",
        "base",
        "revision",
      ].every((key) => receipt[key] === submission[key]) &&
      Boolean(receipt.receipt) &&
      (prepare
        ? ["prepared", "recorded"]
        : ["recorded", "pending-native-session"]
      ).includes(receipt.state),
    // Publication imports this module from the checkout; its retained input
    // belongs beside the surviving dashboard-prepared reporting executable.
    path.join(path.dirname(pending), "dashboard-completion.mjs"),
  );
}
export async function captureAcceptedLanding(
  contextFile,
  { base, revision, remote, target },
) {
  try {
    const pending = await currentLandingInput(contextFile);
    return {
      state: "recorded",
      receipt: await submitLandingInput(pending, {
        base,
        revision,
        remote,
        target,
      }),
    };
  } catch (error) {
    // Git acceptance stands independently of evidence transport or persistence.
    return { state: "unacknowledged", error: error.message };
  }
}
export async function reportLanding(values) {
  if (values.outcome || values["message-file"] || values.session)
    throw new Error(
      "Landing capture is independent of completion and native session disposition.",
    );
  if (values.retry) {
    if (
      ["base", "revision", "remote", "target", "identity"].some(
        (key) => values[key] !== undefined,
      )
    )
      throw new Error(
        "Retry uses the retained landing comparison; do not supply replacements.",
      );
    const expected = { ...values };
    delete expected.retry;
    delete expected.operation;
    return submitLandingInput(
      path.resolve(values.retry),
      expected,
      values.operation === "landing-prepare",
    );
  }
  const submission = { ...values, delivery: randomUUID() };
  delete submission.operation;
  validate(submission);
  const pending = path.join(
    path.dirname(fileURLToPath(import.meta.url)),
    `landing-${submission.delivery}.json`,
  );
  await writeFile(pending, `${JSON.stringify(submission, null, 2)}\n`, {
    flag: "wx",
    mode: 0o600,
  });
  return submitLandingInput(
    pending,
    {},
    values.operation === "landing-prepare",
  );
}
