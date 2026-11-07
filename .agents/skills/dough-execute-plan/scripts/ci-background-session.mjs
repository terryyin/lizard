import { execFileSync } from "node:child_process";

function processInfo(pid) {
  const output = execFileSync(
    "ps",
    ["-p", String(pid), "-o", "ppid=", "-o", "command="],
    { encoding: "utf8" },
  ).trim();
  const match = /^(\d+)\s+(.+)$/.exec(output);
  return match && { pid, parent: Number(match[1]), command: match[2] };
}

export function currentProcessIdentity() {
  const { pid, command } = processInfo(process.pid);
  return { pid, command };
}

// Observe ancestry, never start or resume a session. Print-mode Claude cannot
// keep its asynchronous hooks live after its foreground result.
export function liveClaudeAncestor() {
  let pid = process.ppid;
  for (let depth = 0; pid > 1 && depth < 30; depth++) {
    const info = processInfo(pid);
    if (!info) return undefined;
    if (/(^|[/ ])claude( |$)/.test(info.command)) {
      if (/(^| )(-p|--print)( |$)/.test(info.command)) return undefined;
      return { pid, command: info.command };
    }
    pid = info.parent;
  }
}

export function liveClaudeStillRunning(identity) {
  try {
    return processInfo(identity.pid)?.command === identity.command;
  } catch {
    return false;
  }
}
