// Tells an agent, after its next tool call, that the slice running in its
// checkout has passed the limit. A host runs it after every tool call:
//
//   node slice-clock-hook.mjs <claude|cursor>     host hook input on stdin
//
// It prints the host's additional-context output once per overrun for each
// agent that makes a tool call, and `{}` otherwise. Seeing the slice at the
// limit counts the overrun exactly as a clock command does. It never fails a
// tool call: whatever goes wrong, it prints `{}` and exits 0.
//
// Both hosts deliver this additional context to a subagent as well as to the
// session's own agent: observed with Claude Code 2.1.325 (`PostToolUse`) and
// Cursor agent 2026.11.13 (`postToolUse`). A subagent's call carries its own
// `agent_id` on Claude Code and its own `conversation_id` on Cursor.
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { checkoutRoot } from "./ci-mailbox.mjs";
import {
  currentTime,
  readRecord,
  recordPath,
  updateRecord,
} from "./slice-clock-record.mjs";
import {
  overrunMessageFor,
  runningSliceReachedLimit,
} from "./slice-clock-slices.mjs";

// The agent whose tool call this is: a subagent has its own key, so the
// coordinator's call leaves the implementer's report unused.
function agentOf(input, host) {
  if (host === "cursor") return input.conversation_id;
  return input.agent_id ?? input.session_id;
}

// The checkout the tool call ran in. Cursor sends an empty `cwd` and names
// the checkout in `workspace_roots`; the installed script's own checkout
// serves when the input names none.
function checkoutOf(input) {
  return input.cwd || input.workspace_roots?.[0] || checkoutRoot;
}

// The overrun message for this agent, or undefined when it has none to hear.
// Reading comes first, so a tool call under the limit writes nothing.
function overrunMessage(file, agent) {
  if (!runningSliceReachedLimit(readRecord(file), currentTime()))
    return undefined;
  return updateRecord(file, (record) => {
    const named = record.running;
    if (!named) return undefined;
    return overrunMessageFor(record, named, agent, currentTime());
  });
}

// What the hook answers the host for one tool call.
export function sliceOverrunOutput(input, host) {
  // Cursor may also load .claude/settings.json; use only its native adapter.
  if (host === "claude" && input.cursor_version) return {};
  if (!["claude", "cursor"].includes(host)) return {};
  const agent = agentOf(input, host);
  if (!agent) return {};
  const message = overrunMessage(recordPath(checkoutOf(input)), agent);
  if (message === undefined) return {};
  const context = `dough-execute-plan slice clock: ${message}. Stop and return the unfinished work with the elapsed time.`;
  if (host === "cursor") return { additional_context: context };
  return {
    hookSpecificOutput: {
      hookEventName: "PostToolUse",
      additionalContext: context,
    },
  };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  let output = {};
  try {
    let raw = "";
    for await (const chunk of process.stdin) raw += chunk;
    output = sliceOverrunOutput(JSON.parse(raw), process.argv[2]);
  } catch {
    // A tool call is never failed by its clock.
  }
  process.stdout.write(`${JSON.stringify(output)}\n`);
}
