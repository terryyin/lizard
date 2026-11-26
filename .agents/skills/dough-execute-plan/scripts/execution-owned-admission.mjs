// A start request can carry a draft before there is an originating draft
// checkout. Admission still reads and publishes it through the ordinary Take.
import { mkdir, mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import {
  defaultSeedDirectory,
  readIdentityEvidence,
} from "../../dough-product-backlog/scripts/product-backlog-identity-evidence.mjs";
import { doneCatalogPath } from "../../dough-product-backlog/scripts/product-backlog-done-catalog.mjs";
import { doneRecordDirectory } from "../../dough-product-backlog/scripts/product-backlog-done-record.mjs";
import { seedNumberOf } from "../../dough-product-backlog/scripts/product-backlog-identity-sources.mjs";
import { composeIdentity } from "../../dough-product-backlog/scripts/product-backlog-identity.mjs";
import { recordStoryState } from "../../dough-product-backlog/scripts/product-backlog-story-state.mjs";
import { renderEntry } from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { refused } from "./execution-admission-reconcile.mjs";
import { show } from "./execution-source.mjs";
import { git, revParse } from "./publication-git.mjs";
import {
  backlogPath,
  isAncestor,
  remoteOf,
  remoteRef,
  sourceStopped,
  stopped,
} from "./workspace-publication-ownership.mjs";

const anchor = "ad-hoc-work";
const planningDirectory = dirname(backlogPath);
const seedDirectory = `${planningDirectory}/${defaultSeedDirectory}`;
const identityPaths = [
  backlogPath,
  seedDirectory,
  `${planningDirectory}/${doneRecordDirectory}`,
  `${planningDirectory}/${doneCatalogPath}`,
];
const draftLink = (seed) =>
  `${defaultSeedDirectory}/${seed}-${anchor}.md#${anchor}`;
const oneLine = (value) =>
  typeof value === "string" && value.trim() !== "" && !/[\r\n]/.test(value);

// Reuse the allocation reader on a detached checkout with just its inputs.
async function publishedEvidence(repository, revision, identity) {
  const { stdout } = await git(
    repository,
    "ls-tree",
    "--name-only",
    "-z",
    revision,
    "--",
    ...identityPaths,
  );
  const paths = stdout.split("\0").filter(Boolean);
  if (!paths.includes(backlogPath))
    throw refused("fetched trunk has no product backlog");
  const snapshot = await mkdtemp(join(tmpdir(), "dough-identity-"));
  try {
    await git(
      repository,
      "worktree",
      "add",
      "--detach",
      "--no-checkout",
      snapshot,
      revision,
    );
    try {
      await git(
        snapshot,
        "restore",
        `--source=${revision}`,
        "--worktree",
        "--",
        ...paths,
      );
      // An absent tree in the exact revision is an empty seed directory.
      if (!paths.includes(seedDirectory))
        await mkdir(join(snapshot, seedDirectory));
      const evidence = await readIdentityEvidence(join(snapshot, backlogPath), {
        identity,
        includeSeedUse: identity !== undefined,
      });
      if (evidence.gaps.length > 0)
        throw refused(
          `identity evidence is incomplete: ${evidence.gaps.map(({ path, reason }) => `${path}: ${reason}`).join(", ")}`,
        );
      return evidence;
    } finally {
      await git(repository, "worktree", "remove", "--force", snapshot);
    }
  } finally {
    await rm(snapshot, { recursive: true, force: true });
  }
}

// Select the first available seed from the workflow's published evidence.
// The caller retains these coordinates before requesting any publication.
export async function draftAdmission(request) {
  if (
    !request.repository ||
    !request.target ||
    !oneLine(request.title) ||
    !oneLine(request.goal)
  )
    return stopped("invalid-request", {
      error: "draft requires repository, target, and a one-line title and goal",
    });
  const { repository, title, goal } = request;
  try {
    await git(repository, "fetch", remoteOf(request));
    const basis = await revParse(repository, remoteRef(request));
    const evidence = await publishedEvidence(repository, basis);
    const seed = evidence.next ?? "SEED-001";
    const identity = composeIdentity(seed, anchor);
    const link = draftLink(seed);
    renderEntry({ identity, href: link, title });
    return {
      ok: true,
      status: "drafted",
      identity,
      link,
      title,
      ownedDraft: { basis, goal },
    };
  } catch (error) {
    return sourceStopped(error, { error: error.stderr || error.message });
  }
}

export function parseOwnedDraft(input) {
  let draft;
  try {
    draft = typeof input === "string" ? JSON.parse(input) : input;
  } catch {
    return undefined;
  }
  return /^[0-9a-f]{40}(?:[0-9a-f]{24})?$/.test(draft?.basis) &&
    oneLine(draft.goal)
    ? { basis: draft.basis, goal: draft.goal }
    : undefined;
}

export async function readOwnedAdmissionText(request, homePath, candidate) {
  const { repository, identity, link, title, ownedDraft } = request;
  const seed = seedNumberOf(identity);
  if (
    !seed ||
    !/^[\w-]+$/.test(seed.seed) ||
    identity !== composeIdentity(seed.seed, anchor) ||
    link !== draftLink(seed.seed) ||
    !oneLine(title)
  )
    throw refused(
      "owned draft does not name a canonical new seed and one-line title",
    );
  renderEntry({ identity, href: link, title });
  const source = `---\nid: ${seed.seed}\nstatus: active\nscope: one story\n---\n\n# ${seed.seed} — ${title}\n\n<a id="${anchor}"></a>\n\n### ${title}\n\n**Identity:** ${identity}\n\n**Goal:** ${ownedDraft.goal}\n`;
  const { source: content } = recordStoryState(source, {
    href: link,
    identity,
    refinement: "not-refined",
    approach: "unselected",
  });
  if (candidate && (await show(repository, candidate, homePath)) !== content)
    throw refused(
      "owned draft disagrees with the retained admission candidate",
    );
  return content;
}

export async function ownedAdmissionDrafts(request, ref, homePath, content) {
  const {
    repository,
    identity,
    ownedDraft: { basis },
  } = request;
  const trunk = await revParse(repository, ref);
  if (!(await isAncestor(repository, basis, trunk)))
    throw refused("owned draft basis is not on fetched trunk");
  const { highest } = await publishedEvidence(repository, basis);
  if (
    seedNumberOf(identity).number <= (highest?.number ?? 0) ||
    (trunk !== basis &&
      (await publishedEvidence(repository, trunk, identity)).seedInUse)
  )
    throw refused(
      `owned draft identity ${identity} has already been used or is below its allocated range`,
    );
  return { base: basis, read: (path) => (path === homePath ? content : null) };
}
