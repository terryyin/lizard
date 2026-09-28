#!/usr/bin/env node
// Native PreToolUse / preToolUse guard: denies a Claude Code Edit/Write/
// MultiEdit/NotebookEdit call, a Cursor Write/StrReplace/Delete call, or a
// Codex apply_patch call whose target is this project's resolved whole
// product backlog file, so an agent uses the installed dough-product-backlog
// scripts (product-backlog.mjs and its Git adapters) instead of a direct
// hand-edit.
//
// Each host registers this hook only for its native editing boundary (see
// ../assets/*-hooks-guard.json), so it never runs for, and never affects, a
// read, a Bash-invoked script (including one that writes the backlog via
// shell redirection), an edit to any other file, or a human editing the file
// outside an agent tool call. Cursor may also load Claude's PreToolUse
// fragment; that path no-ops when cursor_version is present so only the
// native Cursor preToolUse fragment decides.
//
// Delivered through src/install/open-dough-register-hooks.mjs, the same safe
// JSON merge mechanism dough-execute-plan's own CI hooks already use.

import { realpathSync } from "node:fs";
import { basename, resolve } from "node:path";
import { defaultBacklogPath } from "./product-backlog-store.mjs";

const GUARDED_TOOLS = new Set([
  "Edit",
  "Write",
  "MultiEdit",
  "NotebookEdit",
  "StrReplace",
  "Delete",
]);

function codexPatchPaths(command) {
  if (typeof command !== "string") {
    return [];
  }
  const paths = [];
  for (const line of command.split(/\r?\n/u)) {
    const fileDirective = line.match(
      /^\*\*\* (?:Add|Delete|Update) File: (.+)$/u,
    );
    if (fileDirective) {
      paths.push(fileDirective[1]);
      continue;
    }
    const moveDirective = line.match(/^\*\*\* Move to: (.+)$/u);
    if (moveDirective) {
      paths.push(moveDirective[1]);
    }
  }
  return paths;
}

function candidatePaths(toolName, toolInput) {
  if (!toolInput || typeof toolInput !== "object") {
    return [];
  }
  if (toolName === "apply_patch") {
    return codexPatchPaths(toolInput.command);
  }
  const paths = [];
  if (typeof toolInput.file_path === "string") {
    paths.push(toolInput.file_path);
  }
  if (typeof toolInput.path === "string") {
    paths.push(toolInput.path);
  }
  // NotebookEdit's own field name is unconfirmed against every Claude Code
  // version; checking both keeps this guard correct if it differs, at no
  // cost when it does not.
  if (
    toolName === "NotebookEdit" &&
    typeof toolInput.notebook_path === "string"
  ) {
    paths.push(toolInput.notebook_path);
  }
  return paths;
}

function resolvedPath(projectDir, path) {
  const absolute = resolve(projectDir, path);
  try {
    return realpathSync(absolute);
  } catch {
    return absolute;
  }
}

// Exported for direct, native-process-free testing of the decision itself.
export function evaluateGuard(input, projectDir) {
  const toolName = input?.tool_name;
  if (!GUARDED_TOOLS.has(toolName) && toolName !== "apply_patch") {
    return null;
  }
  const protectedPath = resolvedPath(projectDir, defaultBacklogPath);
  const targets = candidatePaths(toolName, input.tool_input).map((path) =>
    resolvedPath(projectDir, path),
  );
  if (!targets.includes(protectedPath)) {
    return null;
  }
  return {
    hookSpecificOutput: {
      hookEventName: "PreToolUse",
      permissionDecision: "deny",
      permissionDecisionReason:
        `Direct ${toolName} edits to ${defaultBacklogPath} are blocked. Use ` +
        "the installed dough-product-backlog scripts (product-backlog.mjs " +
        "and its Git merge/rebase/cherry-pick adapters) to change the " +
        "backlog instead.",
    },
  };
}

function cursorPermission(decision) {
  if (!decision) {
    return { permission: "allow" };
  }
  const reason = decision.hookSpecificOutput.permissionDecisionReason;
  return {
    permission: "deny",
    user_message: reason,
    agent_message: reason,
  };
}

async function writeJson(value) {
  await new Promise((res, reject) =>
    process.stdout.write(`${JSON.stringify(value)}\n`, (error) =>
      error ? reject(error) : res(),
    ),
  );
}

function isCliEntry() {
  return (
    Boolean(process.argv[1]) &&
    basename(process.argv[1]) === "product-backlog-guard-hook.mjs"
  );
}

if (isCliEntry()) {
  const hostArg = process.argv[3];
  const projectDir = process.argv[2] ?? process.cwd();
  let raw = "";
  for await (const chunk of process.stdin) {
    raw += chunk;
  }
  try {
    const input = raw.trim() ? JSON.parse(raw) : {};
    // Cursor may load Claude's PreToolUse fragment; skip it so only the
    // native Cursor preToolUse adapter denies. Emit Cursor's allow schema
    // so an empty/invalid permission response cannot block unrelated tools.
    if (hostArg !== "cursor" && input.cursor_version) {
      await writeJson({ permission: "allow" });
    } else {
      const decision = evaluateGuard(input, projectDir);
      if (hostArg === "cursor") {
        await writeJson(cursorPermission(decision));
      } else if (decision) {
        await writeJson(decision);
      }
    }
  } catch (error) {
    process.stderr.write(
      `product-backlog guard hook failed: ${String(error).slice(0, 600)}\n`,
    );
    process.exitCode = 1;
  }
}
