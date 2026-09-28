// Published queued or continued Taken source, the selected story's section and
// declared plan shared with admission, and local unpublished selected-source
// checks.
import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join, posix, relative, resolve, sep } from "node:path";
import {
  parseBacklog,
  queueHeading,
  takenHeading,
} from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { readHome } from "../../dough-product-backlog/scripts/product-backlog-home-reader.mjs";
import { splitHref } from "../../dough-product-backlog/scripts/product-backlog-identity.mjs";
import {
  planFileOf,
  sameDocument,
} from "../../dough-product-backlog/scripts/product-backlog-plan.mjs";
import { readStoryState } from "../../dough-product-backlog/scripts/product-backlog-story-state.mjs";
import { BacklogError } from "../../dough-product-backlog/scripts/product-backlog-refusal.mjs";
import { git } from "./publication-git.mjs";
import {
  backlogPath,
  claimProvenance,
} from "./workspace-publication-ownership.mjs";

export async function show(cwd, rev, path) {
  try {
    return (await git(cwd, "show", `${rev}:${path}`)).stdout;
  } catch {
    return null;
  }
}

function within(root, path) {
  const project = resolve(root);
  const absolute = resolve(project, path);
  if (absolute !== project && !absolute.startsWith(project + sep))
    throw new Error(`source path escapes project: ${path}`);
  return relative(project, absolute).split(sep).join("/");
}

// The selected story's section of `source`, or undefined when `source` has
// none: its document, its region, and its text without the separator lines a
// following sibling section may add. Whitespace within the section, including
// trailing spaces on text, is kept.
export function sectionOf(source, href) {
  if (source === null) return undefined;
  let home;
  try {
    home = readHome(source, href);
  } catch (error) {
    if (error instanceof BacklogError) return undefined;
    throw error;
  }
  const { document, region } = home;
  const lines = document.lines.slice(region.start, region.end);
  while (lines.at(-1) === "") lines.pop();
  return { document, region, text: lines.join("\n") };
}

// The originating checkout's working-tree copy, or null when it has none.
export function worktreeSource(root, path) {
  try {
    return readFileSync(join(root, path), "utf8");
  } catch {
    return null;
  }
}

export async function mergeBase(integration, remoteRef) {
  return (
    await git(integration, "merge-base", "HEAD", remoteRef)
  ).stdout.trim();
}

// Where the selected story's preparation lives and what it declares. The
// canonical home's project path follows its backlog link; `declaredPlan`
// resolves the plan a recorded approach names, relative to that home, and the
// file `planPath` that plan link names. A plan naming the home's own document
// is canonical: it is read and digested as the home, and a Taken entry never
// links it. Any other plan is its own file, linked by `planTarget`. `read`
// reads the preparation recorded in a home's text, digesting the declared
// plan's `planSource` when one is given.
export function selectedPreparation(integration, href) {
  const homePath = within(
    integration,
    posix.join(dirname(backlogPath), splitHref(href).path),
  );
  return {
    homePath,
    declaredPlan(approach) {
      if (approach.kind !== "planned") return {};
      const declared = within(
        integration,
        posix.join(dirname(homePath), approach.plan),
      );
      const planPath = planFileOf(declared);
      const planHref = posix.relative(dirname(backlogPath), declared);
      if (sameDocument(planHref, href))
        return { planPath, planHref, planIsCanonical: true };
      return { planPath, planHref, planTarget: planHref };
    },
    read(source, { planIsCanonical } = {}, planSource) {
      return readStoryState(source, href, { planIsCanonical, planSource });
    },
  };
}

// A version of the selected source to compare: a missing file, the whole
// file, or the selected story's section (undefined when the file lacks it).
function versionOf(source, href) {
  if (source === null || !href) return source;
  return sectionOf(source, href)?.text;
}

// `git cat-file --batch` output for `names`, as raw bytes.
function catFileBatch(cwd, names) {
  return new Promise((resolve, reject) => {
    const child = spawn("git", ["cat-file", "--batch"], { cwd });
    const chunks = [];
    child.stdout.on("data", (chunk) => chunks.push(chunk));
    child.stderr.resume();
    child.stdin.on("error", () => {});
    child.on("error", reject);
    child.on("close", (code) =>
      code === 0
        ? resolve(Buffer.concat(chunks))
        : reject(new Error(`git cat-file --batch exited ${code}`)),
    );
    child.stdin.end(names.map((name) => `${name}\n`).join(""));
  });
}

// Blobs this small print identically through `git show`, far below its
// output limit.
const batchedBlobLimit = 256 * 1024;

// The contents of `rev:path` for each name, as `show` returns them. One
// batch answers each small blob; any other answer is read by `git show`.
async function showAll(cwd, names) {
  const results = names.map(() => undefined);
  if (!names.some((name) => name.includes("\n"))) {
    try {
      const output = await catFileBatch(cwd, names);
      let offset = 0;
      for (const [index, name] of names.entries()) {
        const end = output.indexOf(0x0a, offset);
        if (end < 0) throw new Error("truncated cat-file output");
        const header = output.toString("utf8", offset, end);
        offset = end + 1;
        if (header === `${name} missing` || header === `${name} ambiguous`)
          continue;
        const [, type, size] = header.split(" ");
        const length = Number(size);
        if (!/^\d+$/.test(size ?? "")) throw new Error("unexpected header");
        if (type === "blob" && length <= batchedBlobLimit)
          results[index] = output.toString("utf8", offset, offset + length);
        offset += length + 1;
      }
    } catch {
      results.fill(undefined);
    }
  }
  return Promise.all(
    names.map((name, index) => {
      if (results[index] !== undefined) return results[index];
      const split = name.indexOf(":");
      return show(cwd, name.slice(0, split), name.slice(split + 1));
    }),
  );
}

// Whether, for each source in order, the originating checkout's HEAD, index
// or worktree holds a version of its selected part (its `href` section, or
// the whole file) that was never published: one matching neither its merge
// base, fetched trunk, nor a `published` revision such as the claim that
// admitted it and left its draft there. One merge base and one batch read
// answer every source.
async function unpublishedSources(integration, remoteRef, sources, published) {
  const base = await mergeBase(integration, remoteRef);
  const knownRevs = [base, remoteRef, ...published];
  const revs = [...knownRevs, "HEAD", ""];
  const read = await showAll(
    integration,
    sources.flatMap(({ path }) => revs.map((rev) => `${rev}:${path}`)),
  );
  return sources.map(({ path, href }, position) => {
    const versions = read.slice(
      position * revs.length,
      (position + 1) * revs.length,
    );
    const known = new Set(
      versions
        .slice(0, knownRevs.length)
        .map((source) => versionOf(source, href)),
    );
    const local = [
      ...versions.slice(knownRevs.length),
      worktreeSource(integration, path),
    ];
    return local.some((source) => !known.has(versionOf(source, href)));
  });
}

export async function readPublishedExecutionSource(request, remoteRef) {
  const backlog = await show(request.integration, remoteRef, backlogPath);
  if (backlog === null) throw new Error("fetched trunk has no product backlog");
  const entry = parseBacklog(backlog).entries.find(
    (item) => item.identity === request.identity,
  );
  if (!entry || (entry.list !== queueHeading && entry.list !== takenHeading))
    throw new Error(
      "selected identity is not queued on fetched trunk; admit accepted work that no backlog list holds with --admit",
    );
  // Outside claim recovery, Taken work is a continuation: only the claim's
  // own publisher continues it, and only from ready published preparation.
  let claim;
  if (entry.list === takenHeading && !request.retained) {
    claim = await claimProvenance(
      request.integration,
      remoteRef,
      request.identity,
      backlogPath,
    );
    if (!claim?.publisher || claim.publisher !== request.publisherId)
      return { existing: entry, claim };
  }
  const selection = selectedPreparation(request.integration, entry.href);
  const { homePath } = selection;
  const home = await show(request.integration, remoteRef, homePath);
  if (home === null)
    throw new Error("selected canonical home is absent on fetched trunk");
  const preview = selection.read(home);
  if (preview.identity !== request.identity || preview.status !== "recorded")
    throw new Error("selected canonical preparation or identity is unresolved");
  const declaredPlan = selection.declaredPlan(preview.approach);
  const { planPath, planHref, planTarget, planIsCanonical } = declaredPlan;
  let plan;
  if (preview.approach.kind === "planned") {
    if (!planIsCanonical) {
      plan = await show(request.integration, remoteRef, planPath);
      if (plan === null) throw new Error("published plan is absent");
    }
    // A link to a section of the declared plan links that plan.
    if (
      entry.plan &&
      (planTarget === undefined || !sameDocument(entry.plan.target, planTarget))
    )
      throw new Error(
        `${entry.list === takenHeading ? "Taken" : "queued"} plan link ` +
          `disagrees with preparation`,
      );
    // Continuation writes nothing: the recorder links a Taken entry's plan.
    if (claim && planTarget && !entry.plan)
      throw new Error(
        `Taken entry does not link the plan ${planTarget} its published ` +
          `preparation declares; record the planned approach again with ` +
          `record-state and publish it`,
      );
    // A requested section of the declared plan requests that plan.
    if (request.plan && !sameDocument(request.plan, planHref))
      throw new Error("requested plan disagrees with published preparation");
  } else if (preview.approach.kind === "unselected") {
    throw new Error("published approach is unselected");
  } else if (
    preview.approach.kind !== "planless" ||
    request.plan ||
    entry.plan
  ) {
    throw new Error("queued planless authority or plan link is inconsistent");
  }
  const preparation = selection.read(home, declaredPlan, plan);
  if (preparation.assessment.status !== "ready")
    throw new Error(
      `published preparation is ${preparation.assessment.status}`,
    );
  const published = claim ? [claim.sha] : [];
  const [storyChanged, planChanged] = await unpublishedSources(
    request.integration,
    remoteRef,
    [
      { path: homePath, href: entry.href },
      ...(plan !== undefined ? [{ path: planPath, href: null }] : []),
    ],
    published,
  );
  if (storyChanged)
    throw new Error(
      "unpublished selected story source in originating checkout",
    );
  if (planChanged)
    throw new Error("unpublished selected plan in originating checkout");
  return {
    ...(claim ? { existing: entry, claim } : {}),
    entry,
    homePath,
    planPath,
    planTarget,
    preparation,
    selectedSource: sectionOf(home, entry.href).text,
    planSource: plan,
  };
}
