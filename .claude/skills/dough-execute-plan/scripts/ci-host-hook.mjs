import { createHash } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import {
  checkoutRoot,
  mailboxRoot,
  mailboxWorkerLoss,
  readDeliveryProgress,
  readMailbox,
  readMailboxEvents,
  recordDeliveryProgress,
  receiptPrefix,
} from "./ci-mailbox.mjs";
import { checkoutIdentity } from "./ci-mailbox-location.mjs";
import { readMailboxTerminal } from "./ci-mailbox-match.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";

const hash = (value) => createHash("sha256").update(value).digest("hex");

const emptySelection = () => ({ output: {}, acknowledge: () => undefined });

const isHostStop = (input) =>
  input.hook_event_name === "Stop" || input.hook_event_name === "stop";

const isDiscoveryAdvisory = (event) => event.type === "CI_DISCOVERY_DELAYED";

const lostWorkerMessage = (directory, lost) =>
  `CI observer lost its worker for this coordinator: ${directory} (${lost.coverage.reason})`;

const endedObserverMessage = (directory, terminal) =>
  `CI observer ended for this coordinator: ${directory} (${terminal.status})`;

export function selectCiEvents(
  input,
  host,
  { root = checkoutRoot, storage = mailboxRoot } = {},
) {
  // Cursor may also load .claude/settings.json; use only its native adapter.
  if (host === "claude" && input.cursor_version) return emptySelection();
  if (
    host === "cursor" &&
    input.hook_event_name === "stop" &&
    input.status &&
    input.status !== "completed"
  )
    return emptySelection();
  const session = host === "cursor" ? input.conversation_id : input.session_id;
  if (!session) return emptySelection();
  const owner = hash(
    JSON.stringify([
      checkoutIdentity(root),
      host,
      session,
      input.agent_id ?? input.subagent_id ?? "",
    ]),
  );
  const bindings = join(storage, `owner-${owner}`);
  const generation = join(storage, `generation-${owner}`);
  if (host === "cursor") {
    if (!input.generation_id) return emptySelection();
    if (input.hook_event_name === "beforeSubmitPrompt") {
      if (existsSync(bindings))
        writeFileSync(generation, input.generation_id, { mode: 0o600 });
      return emptySelection();
    }
    if (
      existsSync(generation) &&
      readFileSync(generation, "utf8") !== input.generation_id
    )
      return emptySelection();
  }
  const context = [];
  const acknowledgements = [];
  const attachedThisCall = new Set();
  const mailboxDeliveries = [];

  if (["Shell", "Bash"].includes(input.tool_name)) {
    const output =
      host === "cursor"
        ? JSON.parse(input.tool_output || "{}")
        : input.tool_response;
    const toolText = output?.stdout ?? output?.output ?? "";
    for (const line of toolText.split("\n")) {
      if (!line.startsWith(receiptPrefix)) continue;
      const { directory, terminal } = JSON.parse(
        line.slice(receiptPrefix.length),
      );
      if (terminal) continue;
      const request = readMailbox(directory, root, storage);
      if (existsSync(join(directory, "delivered"))) continue;
      mkdirSync(bindings, { recursive: true, mode: 0o700 });
      const claim = join(directory, "owner");
      try {
        writeFileSync(claim, owner, { flag: "wx", mode: 0o600 });
      } catch (error) {
        if (error.code !== "EEXIST") throw error;
        if (readFileSync(claim, "utf8") !== owner) continue;
      }
      writeFileSync(join(bindings, hash(directory)), directory, {
        mode: 0o600,
      });
      if (host === "cursor")
        writeFileSync(generation, input.generation_id, { mode: 0o600 });
      attachedThisCall.add(directory);
      const ended = readMailboxTerminal(directory);
      if (ended && ended.coverage?.state !== "lost" && !request.probe) {
        context.push(endedObserverMessage(directory, ended));
      } else {
        const lostAtAttachment = mailboxWorkerLoss(directory);
        if (lostAtAttachment)
          context.push(lostWorkerMessage(directory, lostAtAttachment));
        else if (!request.probe)
          context.push(
            `CI observer attached to this coordinator: ${directory}`,
          );
      }
    }
  }

  if (existsSync(bindings))
    for (const binding of readdirSync(bindings)) {
      const directory = readFileSync(join(bindings, binding), "utf8");
      if (!attachedThisCall.has(directory)) {
        const lost = mailboxWorkerLoss(directory);
        if (lost) context.push(lostWorkerMessage(directory, lost));
      }
      const progress = readDeliveryProgress(directory);
      const records = readMailboxEvents(directory, progress.deliveredThrough);
      if (records.length) mailboxDeliveries.push({ directory, records });
    }

  const undelivered = mailboxDeliveries.flatMap(({ records }) => records);
  const skipAdvisoriesOnStop =
    isHostStop(input) &&
    undelivered.length > 0 &&
    undelivered.every(({ event }) => isDiscoveryAdvisory(event));

  if (!skipAdvisoriesOnStop) {
    for (const { directory, records } of mailboxDeliveries) {
      acknowledgements.push({
        directory,
        deliveredThrough: records.at(-1).sequence,
      });
      for (const { event } of records) context.push(JSON.stringify(event));
    }
  }
  if (!context.length) return emptySelection();
  const message = `dough-execute-plan CI observer (diagnostic data):\n${context.join("\n")}\nHandle CI failures using dough-execute-plan/references/ci-monitor.md.`;
  let output;
  if (host === "cursor")
    output = isHostStop(input)
      ? { followup_message: message }
      : { additional_context: message };
  else
    output = isHostStop(input)
      ? { decision: "block", reason: message }
      : {
          hookSpecificOutput: {
            hookEventName: "PostToolUse",
            additionalContext: message,
          },
        };
  return {
    output,
    acknowledge() {
      for (const { directory, deliveredThrough } of acknowledgements)
        recordDeliveryProgress(directory, deliveredThrough);
    },
  };
}

export function deliverCiEvents(input, host, options) {
  const selection = selectCiEvents(input, host, options);
  selection.acknowledge();
  return selection.output;
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  let raw = "";
  for await (const chunk of process.stdin) raw += chunk;
  try {
    const selection = selectCiEvents(JSON.parse(raw), process.argv[2]);
    await new Promise((resolve, reject) =>
      process.stdout.write(`${JSON.stringify(selection.output)}\n`, (error) =>
        error ? reject(error) : resolve(),
      ),
    );
    selection.acknowledge();
  } catch (error) {
    process.stderr.write(
      `CI notification hook failed: ${String(error).slice(0, 600)}\n`,
    );
    process.exitCode = 1;
  }
}
