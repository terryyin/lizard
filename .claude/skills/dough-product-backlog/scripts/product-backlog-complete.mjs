// Removes exactly one explicitly named work item from whichever active list
// holds it, leaving every sibling, the order of both lists, the direction, and
// the text around them as they were.
//
// The caller has already decided that the work is done; this applies that one
// decision and nothing else. It never judges completion: a missing plan file,
// a missing seed, and a plan's status text establish nothing here, and none of
// them can make an entry removable. It never deletes a story or plan file
// either. Closing the work's canonical homes, with its seed, plan, and proof
// cleanup, stays with the wrap-up workflow that calls this. The one other file
// it removes is the execution agent profile that names the completed identity,
// so the name that held the work is released in the same change. A
// preparation assignment is never ended by completing work.

import { existsSync, readFileSync, readdirSync, rmSync } from "node:fs";
import { join } from "node:path";
import {
  agentIdentity,
  agentProfileDirectory,
  parseAgentProfile,
  profileAgentName,
} from "./product-backlog-agent-profile.mjs";
import { parseBacklog, renderBacklog } from "./product-backlog-document.mjs";
import { findEntry, removeEntryLine } from "./product-backlog-placement.mjs";

// What a caller can do when the named identity is in neither list. The script
// keeps no record of removed work, so it cannot tell an already applied
// removal from an identity that was never listed, and it says so rather than
// reporting a removal it did not make.
const absent =
  `Nothing was removed. Either an earlier run already removed it and the ` +
  `backlog already holds that outcome, or this is not the identity the ` +
  `backlog carries: read the two lists and name the entry to remove.`;

// Applies the removal to one backlog document and returns the backlog to
// publish alongside the entry that was removed.
export function completeEntry(source, request) {
  const document = parseBacklog(source);
  const entry = findEntry(document, request.identity, absent);

  removeEntryLine(document, entry.index);
  return { source: renderBacklog(document), entry };
}

// Removes every readable execution agent profile beside the backlog whose
// identity is the completed one, and returns the removed paths relative to
// that directory. An unreadable profile names no identity, so it is left for a
// person to read; a preparation profile ends only through its own release.
export function releaseAgentProfiles(backlogDirectory, identity) {
  const directory = join(backlogDirectory, agentProfileDirectory);
  if (!existsSync(directory)) return [];
  const released = [];
  for (const fileName of readdirSync(directory).sort()) {
    const name = profileAgentName(fileName);
    if (name === undefined) continue;
    const { path } = agentIdentity(name);
    const read = parseAgentProfile(
      readFileSync(join(backlogDirectory, path), "utf8"),
    );
    if (
      read.ok &&
      read.profile.activity === "execution" &&
      read.profile.identity === identity
    ) {
      rmSync(join(backlogDirectory, path));
      released.push(path);
    }
  }
  return released;
}
