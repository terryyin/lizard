// The coordinator that owns a CI observer. One owner meaning serves the host
// hook, managed selection, and recovery: repository checkout identity, host,
// session or conversation, and child identity. A mailbox's `owner` file is
// that coordinator's exclusive claim; repository, branch, and liveness are
// access and matching facts, never ownership.
import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { checkoutIdentity } from "./ci-mailbox-location.mjs";

export function observerOwner({ root, host, session, child = "" }) {
  return createHash("sha256")
    .update(JSON.stringify([checkoutIdentity(root), host, session, child]))
    .digest("hex");
}

// The owner a host hook input names: Cursor's conversation or Claude Code's
// session, with the child agent that sent it. Undefined without that identity.
export function hostInputOwner(input, host, root) {
  const session = host === "cursor" ? input.conversation_id : input.session_id;
  if (!session) return undefined;
  return observerOwner({
    root,
    host,
    session,
    child: input.agent_id ?? input.subagent_id ?? "",
  });
}

// The owner of a Codex yielded stream: the coordinator value its arming cell
// passed, which that coordinator retains in its observer note.
export function codexStreamOwner({ root, coordinator }) {
  if (!coordinator) return undefined;
  return observerOwner({ root, host: "codex", session: coordinator });
}

export function readOwnerClaim(directory) {
  try {
    return readFileSync(join(directory, "owner"), "utf8");
  } catch (error) {
    if (error.code === "ENOENT") return undefined;
    throw error;
  }
}

// Claims an unclaimed mailbox for `owner`. Returns whether `owner` holds the
// claim afterwards; another owner's claim is never replaced.
export function claimMailbox(directory, owner) {
  try {
    writeFileSync(join(directory, "owner"), owner, { flag: "wx", mode: 0o600 });
    return true;
  } catch (error) {
    if (error.code !== "EEXIST") throw error;
    return readOwnerClaim(directory) === owner;
  }
}
