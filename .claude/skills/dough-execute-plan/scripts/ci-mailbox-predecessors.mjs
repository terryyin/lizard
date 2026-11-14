// Predecessor observers a replacement records when completion recovery
// establishes a new mailbox for the same coordinator and target. Completion
// walks this chain for the earliest deadline and unread actionable failures.
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { isFullGitRevision } from "./ci-revisions.mjs";

const predecessorsFile = "predecessors.json";

export function writePredecessors(directory, predecessors) {
  writeFileSync(
    join(directory, predecessorsFile),
    JSON.stringify({ predecessors: [...predecessors] }),
    { mode: 0o600 },
  );
}

export function readPredecessors(directory) {
  try {
    const { predecessors = [] } = JSON.parse(
      readFileSync(join(directory, predecessorsFile), "utf8"),
    );
    return predecessors;
  } catch (error) {
    if (error.code === "ENOENT") return [];
    throw error;
  }
}

// This mailbox and every recorded predecessor, each directory once.
export function observationChain(directory) {
  const seen = new Set();
  const chain = [];
  const visit = (current) => {
    if (!current || seen.has(current)) return;
    seen.add(current);
    chain.push(current);
    for (const predecessor of readPredecessors(current)) visit(predecessor);
  };
  visit(directory);
  return chain;
}

export function earliestCompletionHandoff(directories, sha) {
  if (!isFullGitRevision(sha)) return undefined;
  const normalized = sha.toLowerCase();
  let earliest;
  for (const directory of directories) {
    const path = join(directory, "completion", `${normalized}.json`);
    if (!existsSync(path)) continue;
    const handoff = JSON.parse(readFileSync(path, "utf8"));
    if (!earliest || handoff.startedAt < earliest.startedAt) earliest = handoff;
  }
  return earliest;
}
