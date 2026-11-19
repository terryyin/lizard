// The Take a source-freshness comparison starts from, for
// `source-freshness.mjs`.
//
// Fetches the authorized remote and looks among the best common bases of
// fetched trunk and HEAD (`git merge-base --all`) for those at which the
// identity is Taken (the Take: Story Branch increments never reach trunk,
// while a merged sibling or contributor history can add a second best base
// without it). The Taken entry on fetched trunk carries the story's link and
// plan link. No best base with the Taken entry is refused
// `not-taken-at-base`, naming the bases. Several Taken bases serve as one
// when every compared record reads the same at each: the comparison starts
// from the first by revision name, whatever order Git lists them in. A
// record that differs between them is refused `ambiguous-base`, naming the
// bases and the record. Reads Git only and writes nothing beyond the fetch.
import {
  parseBacklog,
  takenHeading,
} from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { show } from "./execution-source.mjs";
import { fetchedTarget, git, originTrackingRef } from "./publication-git.mjs";
import { backlogPath } from "./workspace-publication-ownership.mjs";

export class FreshnessRefusal extends Error {
  constructor(reason, message, fields = {}) {
    super(message);
    this.reason = reason;
    this.fields = fields;
  }
}

// The Taken entry for `identity` in the backlog at `rev`, or undefined.
async function takenEntry(workspace, rev, identity) {
  const backlog = await show(workspace, rev, backlogPath);
  if (backlog === null) return undefined;
  return parseBacklog(backlog).entries.find(
    (item) => item.identity === identity && item.list === takenHeading,
  );
}

// The best common bases of HEAD and fetched trunk: the candidates for the Take.
async function sharedBases(repository, trunkRevision, trackingRef) {
  try {
    return (
      await git(repository, "merge-base", "--all", trunkRevision, "HEAD")
    ).stdout
      .trim()
      .split("\n");
  } catch {
    throw new FreshnessRefusal(
      "no-common-base",
      `HEAD shares no history with ${trackingRef}`,
    );
  }
}

// Those of `candidates` at which `identity` is Taken, in revision-name
// order; none is refused, naming every base examined.
async function takenBases(repository, candidates, identity) {
  const taken = [];
  for (const candidate of candidates)
    if (await takenEntry(repository, candidate, identity))
      taken.push(candidate);
  if (taken.length) return taken.sort();
  if (candidates.length === 1)
    throw new FreshnessRefusal(
      "not-taken-at-base",
      `${identity} is not Taken in the backlog at the branch's base ${candidates[0]}`,
    );
  throw new FreshnessRefusal(
    "not-taken-at-base",
    `${identity} is not Taken in the backlog at any of the branch's bases: ${candidates.join(", ")}`,
    { bases: candidates },
  );
}

// Refuses a `record` at `path` whose text (`textOf` its document) differs
// between the Taken `bases`, so any of them serves as the comparison's base.
async function agreeAcross(repository, bases, record, path, textOf) {
  const texts = [];
  for (const base of bases)
    texts.push(textOf(await show(repository, base, path)));
  if (texts.some((text) => text !== texts[0]))
    throw new FreshnessRefusal(
      "ambiguous-base",
      `${record} ${path} differs between the branch's bases that hold the Take: ${bases.join(", ")}`,
      { bases, record, path },
    );
}

// The fetched trunk revision, the base the comparison starts from, the Taken
// entry on fetched trunk, and `agree(record, path, textOf)`, which refuses a
// compared record that the Taken bases disagree about.
export async function resolveTake({ repository, identity, remote, targetRef }) {
  const trackingRef = originTrackingRef(targetRef, remote);
  try {
    await git(repository, "fetch", remote);
  } catch (error) {
    throw new FreshnessRefusal("fetch-failed", error.message.trim());
  }
  const trunkRevision = await fetchedTarget(repository, targetRef, remote);
  if (trunkRevision === null)
    throw new FreshnessRefusal("trunk-absent", `${trackingRef} is not fetched`);
  const candidates = await sharedBases(repository, trunkRevision, trackingRef);
  const entry = await takenEntry(repository, trunkRevision, identity);
  if (!entry)
    throw new FreshnessRefusal(
      "not-taken",
      `${identity} is not Taken in the backlog on fetched ${trackingRef}`,
    );
  const bases = await takenBases(repository, candidates, identity);
  const agree = (record, path, textOf = (document) => document) =>
    agreeAcross(repository, bases, record, path, textOf);
  return { trunkRevision, base: bases[0], entry, agree };
}
