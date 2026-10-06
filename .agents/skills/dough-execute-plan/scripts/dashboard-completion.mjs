#!/usr/bin/env node
// Standalone installed reporting operation: no dependency on a retired CWD.
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { readFile, writeFile } from "node:fs/promises";
import { randomUUID } from "node:crypto";
import { fileURLToPath } from "node:url";
import path from "node:path";

const quote = (part) => `'${part.replaceAll("'", "'\\''")}'`;
export async function reportCompletion(argv) {
  const values = {};
  const allowed = new Set([
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
  const origin = new URL(submission.origin);
  if (
    origin.protocol !== "http:" ||
    !["127.0.0.1", "localhost", "[::1]"].includes(origin.hostname) ||
    origin.origin !== submission.origin ||
    origin.username ||
    origin.password
  )
    throw new Error("The reporting origin must be a loopback HTTP origin.");
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
  try {
    const report = { ...submission };
    delete report.origin;
    const response = await fetch(`${origin.origin}/__agent-launch/completion`, {
      method: "POST",
      headers: { "Content-Type": "application/json", Origin: origin.origin },
      body: JSON.stringify(report),
      signal: AbortSignal.timeout(10000),
    });
    const receipt = await response.json();
    if (!response.ok)
      throw new Error(receipt.error ?? "Reporting was refused.");
    if (
      !receipt.receipt ||
      receipt.delivery !== submission.delivery ||
      receipt.reference !== submission.reference ||
      receipt.outcome !== submission.outcome ||
      receipt.message !== submission.message ||
      !["recorded", "pending-native-session"].includes(receipt.state)
    )
      throw new Error("No matching completion receipt was received.");
    return receipt;
  } catch (error) {
    throw new Error(
      `${error.message}\nRetained completion: ${pending}\nRetry reporting only: ${[process.execPath, fileURLToPath(import.meta.url), "--retry", pending].map(quote).join(" ")}`,
      { cause: error },
    );
  }
}
if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  reportCompletion(process.argv.slice(2)).then(
    (receipt) => process.stdout.write(`${JSON.stringify(receipt)}\n`),
    (error) => {
      process.stderr.write(
        `Completion delivery was not acknowledged: ${error.message}\nKeep the message and retry without repeating the work.\n`,
      );
      process.exitCode = 1;
    },
  );
}
