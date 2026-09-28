// Invoke the installed host notification bridge. Cursor and Claude use the
// hook script; Codex readiness is supplied by the caller when tools exist.
import { spawn } from "node:child_process";
import { once } from "node:events";
import { fileURLToPath } from "node:url";
import { probeMailbox, receiptPrefix } from "./ci-mailbox.mjs";

const defaultHook = fileURLToPath(
  new URL("./ci-host-hook.mjs", import.meta.url),
);

// Explicit session input is authoritative, metadata included. Without it, a
// Claude Code coordinator is identified by its documented session variable
// from the supplied environment; other hosts never use that variable.
export function resolveHostSession({ host, session, env = process.env }) {
  if (session !== undefined && session !== null) return session;
  const claudeSession = host === "claude" && env?.CLAUDE_CODE_SESSION_ID;
  return claudeSession ? { session_id: claudeSession } : session;
}

const missingIdentityReason = {
  claude:
    "Claude Code session identity is unavailable: CLAUDE_CODE_SESSION_ID is unset and no --session-json was supplied; run deliver from the coordinator's own Bash tool or pass --session-json with its session_id",
};

function hookInput(host, session, receipt = "") {
  return {
    session_id: session.session_id ?? session.conversation_id,
    conversation_id: session.conversation_id ?? session.session_id,
    generation_id: session.generation_id ?? "managed-delivery",
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
  codexBridgeAvailable,
}) {
  if (host === "codex") {
    return {
      ready: codexBridgeAvailable === true,
      reason:
        codexBridgeAvailable === true
          ? undefined
          : "Codex yielded-cell bridge is unavailable",
    };
  }
  if (!session?.conversation_id && !session?.session_id) {
    return {
      ready: false,
      reason:
        missingIdentityReason[host] ??
        "host session identity is required to verify the notification bridge",
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
  if (host === "codex") {
    return {
      attached: true,
      context: "codex stream binding retained by caller",
    };
  }
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
