// Invoke the installed host notification bridge: the hook script Cursor and
// Claude Code coordinators share.
import { spawn } from "node:child_process";
import { once } from "node:events";
import { fileURLToPath } from "node:url";
import { managedDeliveryGeneration } from "./ci-mailbox-location.mjs";
import { probeMailbox, receiptPrefix } from "./ci-mailbox.mjs";
import { hostInputOwner } from "./ci-observer-owner.mjs";

const defaultHook = fileURLToPath(
  new URL("./ci-host-hook.mjs", import.meta.url),
);

// Explicit session input is authoritative, metadata included. Without it, a
// Claude Code or Cursor coordinator is identified by its own host's session
// variable from the supplied environment; no host uses another host's variable.
const hostIdentity = {
  claude: {
    name: "Claude Code session",
    variable: "CLAUDE_CODE_SESSION_ID",
    field: "session_id",
    tool: "Bash",
  },
  cursor: {
    name: "Cursor conversation",
    variable: "CURSOR_CONVERSATION_ID",
    field: "conversation_id",
    tool: "Shell",
  },
};

export function resolveHostSession({ host, session, env = process.env }) {
  if (session !== undefined && session !== null) return session;
  const identity = hostIdentity[host];
  const ambient = identity && env?.[identity.variable];
  return ambient ? { [identity.field]: ambient } : session;
}

export function missingIdentityReason(host, command = "deliver") {
  const identity = hostIdentity[host];
  if (!identity)
    return "host session identity is required to verify the notification bridge";
  return `${identity.name} identity is unavailable: ${identity.variable} is unset and no --session-json was supplied; run ${command} from the coordinator's own ${identity.tool} tool or pass --session-json with its ${identity.field}`;
}

function hookInput(host, session, receipt = "") {
  return {
    session_id: session.session_id ?? session.conversation_id,
    conversation_id: session.conversation_id ?? session.session_id,
    generation_id: session.generation_id ?? managedDeliveryGeneration,
    agent_id: session.agent_id,
    subagent_id: session.subagent_id,
    transcript_path:
      session.transcript_path ?? "/tmp/dough-managed-delivery.jsonl",
    hook_event_name: host === "cursor" ? "postToolUse" : "PostToolUse",
    tool_name: host === "cursor" ? "Shell" : "Bash",
    tool_output: JSON.stringify({
      stdout: receipt,
      output: receipt,
      exitCode: 0,
    }),
    tool_response: { stdout: receipt },
    ...(host === "cursor"
      ? { cursor_version: session.cursor_version ?? "0.0.0" }
      : {}),
  };
}

// The owner the installed hook claims for this session's managed hook input,
// so selection and binding name the same coordinator. Undefined without the
// host's session identity.
export function hostSessionOwner({ host, session, root }) {
  if (!session) return undefined;
  return hostInputOwner(hookInput(host, session), host, root);
}

function bridgeContext(output) {
  return (
    output.additional_context ??
    output.followup_message ??
    output.hookSpecificOutput?.additionalContext ??
    output.reason ??
    ""
  );
}

export async function invokeHostHook(
  host,
  input,
  { hookPath = defaultHook, cwd, env } = {},
) {
  const child = spawn(process.execPath, [hookPath, host], {
    cwd,
    env,
    stdio: ["pipe", "pipe", "pipe"],
  });
  let stdout = "";
  let stderr = "";
  child.stdout.on("data", (chunk) => {
    stdout += chunk;
  });
  child.stderr.on("data", (chunk) => {
    stderr += chunk;
  });
  child.stdin.end(JSON.stringify(input));
  const [code] = await once(child, "exit");
  if (code !== 0) {
    throw new Error(
      `CI host bridge failed (${code}): ${stderr.slice(0, 600) || stdout.slice(0, 600)}`,
    );
  }
  return stdout.trim() ? JSON.parse(stdout) : {};
}

export async function verifyHostBridge({
  host,
  session,
  workspace,
  hookPath = defaultHook,
  env,
  root,
  storage,
}) {
  if (!session?.conversation_id && !session?.session_id) {
    return {
      ready: false,
      reason: missingIdentityReason(host),
    };
  }
  const directory = probeMailbox({ root, storage });
  const receipt = `${receiptPrefix}${JSON.stringify({ directory })}\n`;
  const output = await invokeHostHook(host, hookInput(host, session, receipt), {
    hookPath,
    cwd: workspace,
    env,
  });
  const context = bridgeContext(output);
  if (!/CI_MONITOR_READY/.test(context)) {
    return {
      ready: false,
      reason: "host bridge did not confirm CI_MONITOR_READY",
      context,
    };
  }
  return { ready: true, context, probeDirectory: directory };
}

export async function bindHostObserver({
  host,
  session,
  receipt,
  workspace,
  hookPath = defaultHook,
  env,
}) {
  const output = await invokeHostHook(host, hookInput(host, session, receipt), {
    hookPath,
    cwd: workspace,
    env,
  });
  const context = bridgeContext(output);
  return {
    attached: /CI observer attached to this coordinator/.test(context),
    context,
    output,
  };
}

export function deliverHostBoundary({
  host,
  session,
  workspace,
  hookPath = defaultHook,
  env,
}) {
  return invokeHostHook(host, hookInput(host, session, ""), {
    hookPath,
    cwd: workspace,
    env,
  });
}
