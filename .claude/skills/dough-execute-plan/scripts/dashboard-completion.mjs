#!/usr/bin/env node
// Standalone installed reporting operation: no dependency on a retired CWD.
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { readFile, writeFile } from "node:fs/promises";
import { randomUUID } from "node:crypto";
import { fileURLToPath } from "node:url";
import path from "node:path";

const quote = (part) => `'${part.replaceAll("'", "'\\''")}'`;
export function loopbackReportingOrigin(value) {
  const origin = new URL(value);
  if (
    origin.protocol !== "http:" ||
    !["127.0.0.1", "localhost", "[::1]"].includes(origin.hostname) ||
    origin.origin !== value ||
    origin.username ||
    origin.password
  )
    throw new Error("The reporting origin must be a loopback HTTP origin.");
  return origin;
}

// Both operations deliver immutable retained input and require a matching receipt.
export async function deliverRetainedReport(
  pending,
  submission,
  operation,
  matchesReceipt,
  retryScript = fileURLToPath(import.meta.url),
) {
  const origin = loopbackReportingOrigin(submission.origin);
  const landing = operation !== "completion";
  const endpoint = landing
    ? `/__agent-launch/landing${operation === "landing-prepare" ? "/prepare" : ""}`
    : "/__agent-launch/completion";
  try {
    const report = { ...submission };
    delete report.origin;
    const response = await fetch(`${origin.origin}${endpoint}`, {
      method: "POST",
      headers: { "Content-Type": "application/json", Origin: origin.origin },
      body: JSON.stringify(report),
      signal: AbortSignal.timeout(10000),
    });
    const receipt = await response.json();
    if (!response.ok)
      throw new Error(
        receipt.error ??
          (landing ? "Landing capture was refused." : "Reporting was refused."),
      );
    if (!matchesReceipt(receipt))
      throw new Error(
        `No matching ${landing ? "landing" : "completion"} receipt was received.`,
      );
    return receipt;
  } catch (error) {
    const flags = landing ? ["--operation", operation] : [];
    const retry = [process.execPath, retryScript, ...flags, "--retry", pending]
      .map(quote)
      .join(" ");
    throw new Error(
      `${error.message}\nRetained ${landing ? "landing" : "completion"}: ${pending}\nRetry reporting only: ${retry}`,
      { cause: error },
    );
  }
}

export async function reportCompletion(argv) {
  const values = {};
  const allowed = new Set([
    "operation",
    "base",
    "revision",
    "identity",
    "remote",
    "target",
    "origin",
    "source",
    "host",
    "reference",
    "session",
    "outcome",
    "message-file",
    "retry",
  ]);
  for (let index = 0; index < argv.length; index += 2) {
    const name = argv[index]?.replace(/^--/, "");
    if (
      !argv[index]?.startsWith("--") ||
      !allowed.has(name) ||
      values[name] !== undefined ||
      !argv[index + 1]
    )
      throw new Error("Malformed reporting arguments.");
    values[name] = argv[index + 1];
  }
  if (["landing", "landing-prepare"].includes(values.operation))
    return (await import("./dashboard-landing.mjs")).reportLanding(values);
  if (
    values.operation !== undefined ||
    ["base", "revision", "identity", "remote", "target"].some(
      (key) => values[key] !== undefined,
    )
  )
    throw new Error("Malformed reporting operation.");
  let pending;
  let submission;
  if (values.retry) {
    if (values.outcome || values["message-file"])
      throw new Error(
        "Retry uses the retained outcome and message; do not supply replacements.",
      );
    pending = path.resolve(values.retry);
    submission = JSON.parse(await readFile(pending, "utf8"));
    for (const name of ["origin", "source", "host", "reference", "session"])
      if (values[name] !== undefined && values[name] !== submission[name])
        throw new Error(`The retained completion does not match --${name}.`);
  } else {
    submission = {
      ...values,
      delivery: randomUUID(),
      message: values["message-file"]
        ? await readFile(values["message-file"], "utf8")
        : "",
    };
    delete submission["message-file"];
  }
  for (const name of [
    "origin",
    "source",
    "host",
    "reference",
    "outcome",
    "delivery",
  ])
    if (typeof submission[name] !== "string" || !submission[name])
      throw new Error(`Missing --${name}.`);
  if (!["completed", "unfinished"].includes(submission.outcome))
    throw new Error("The outcome must be completed or unfinished.");
  if (
    !["claude", "codex", "cursor"].includes(submission.host) ||
    typeof submission.message !== "string" ||
    submission.message.length > 16000
  )
    throw new Error("The retained completion is malformed.");
  if (
    (submission.outcome === "unfinished" || submission.message !== "") &&
    !submission.message.trim()
  )
    throw new Error("An attention message must contain useful text.");
  loopbackReportingOrigin(submission.origin);
  if (pending === undefined) {
    pending = path.join(
      path.dirname(fileURLToPath(import.meta.url)),
      `completion-${submission.delivery}.json`,
    );
    // Retain the exact context and message before the first network side effect.
    await writeFile(pending, `${JSON.stringify(submission, null, 2)}\n`, {
      flag: "wx",
      mode: 0o600,
    });
  }
  return deliverRetainedReport(
    pending,
    submission,
    "completion",
    (receipt) =>
      Boolean(receipt.receipt) &&
      receipt.delivery === submission.delivery &&
      receipt.reference === submission.reference &&
      receipt.outcome === submission.outcome &&
      receipt.message === submission.message &&
      ["recorded", "pending-native-session"].includes(receipt.state),
  );
}
if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  reportCompletion(process.argv.slice(2)).then(
    (receipt) => process.stdout.write(`${JSON.stringify(receipt)}\n`),
    (error) => {
      process.stderr.write(
        `${process.argv.includes("--operation") ? "Landing capture" : "Completion delivery"} was not acknowledged: ${error.message}\nKeep the message and retry without repeating the work.\n`,
      );
      process.exitCode = 1;
    },
  );
}
