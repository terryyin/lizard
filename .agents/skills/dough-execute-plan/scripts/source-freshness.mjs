#!/usr/bin/env node
// Whether remote trunk amended the selected story section or its plan since
// the execution branch forked from it, for a Story Branch coordinator.
//
// `check` fetches the authorized remote, takes merge-base(<fetched trunk>,
// HEAD) as the base (the Take: Story Branch increments never reach trunk), and
// reads the Taken entry on fetched trunk for the story's link and plan link.
// The records are the story section and the linked plan; without a plan link,
// the home alone: the whole document when it reads as a plan (a plan-homed
// correction), otherwise the story section (a quick story).
// Each record is compared on trunk and in the workspace's working tree (the
// branch copy: committed increments plus any uncommitted edit of the slice
// being delivered, so absorbing never drops an edit in progress) from its own
// base, named as `from`: the newest trunk revision since the Take whose record
// text the branch copy already contains, or the Take, so an absorbed
// amendment is not mistaken for the branch's own edit. Trunk text equal to
// that base, or already contained in the branch copy, is `unchanged`.
// Otherwise trunk's change is applied three-way over the branch copy with
// `git merge-file`; an overlapping edit, or an application that
// changes or removes a slice the branch marks done, is a `conflict`, and any
// other change is `amended`. `absorb` writes the clean application of the
// amended records into the working tree and refuses a conflict, writing
// nothing. Neither commits, pushes, or writes anywhere else; comparison scratch
// lives briefly in the repository's own Git directory.
//
// Exit codes: 0 unchanged (or absorbed), 1 amended (check only), 2 conflict,
// 3 refused. Every run prints one JSON line.
import { readFile, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";
import {
  parseBacklog,
  takenHeading,
} from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { readPlanSlices } from "../../dough-product-backlog/scripts/product-backlog-plan-reader.mjs";
import { joinSource } from "../../dough-product-backlog/scripts/product-backlog-source.mjs";
import { flagValues, isDirectCliEntry } from "./ci-direct-entry.mjs";
import {
  compareRecord,
  compareSection,
} from "./source-freshness-comparison.mjs";
import { sectionOf, selectedPreparation, show } from "./execution-source.mjs";
import { fetchedTarget, git, originTrackingRef } from "./publication-git.mjs";
import { backlogPath } from "./workspace-publication-ownership.mjs";

const exitCodes = { unchanged: 0, amended: 1, conflict: 2, refused: 3 };

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

async function workingText(workspace, path) {
  try {
    return await readFile(join(workspace, path), "utf8");
  } catch (error) {
    if (error.code === "ENOENT") return null;
    throw error;
  }
}

// `text` in place of the branch copy's selected section, keeping every
// sibling section and the separator lines that follow it byte for byte.
function withSection(branchSection, text) {
  const { document, region } = branchSection;
  const kept = branchSection.text.split("\n").length;
  const lines = [
    ...document.lines.slice(0, region.start),
    ...text.split("\n"),
    ...document.lines.slice(region.start + kept),
  ];
  return joinSource({ ...document, lines });
}

// The comparison `check` prints and the working-tree writes `absorb` makes.
export async function inspectSourceFreshness({
  workspace,
  identity,
  remote,
  targetRef,
}) {
  const repository = resolve(workspace);
  const trackingRef = originTrackingRef(targetRef, remote);
  try {
    await git(repository, "fetch", remote);
  } catch (error) {
    throw new FreshnessRefusal("fetch-failed", error.message.trim());
  }
  const trunkRevision = await fetchedTarget(repository, targetRef, remote);
  if (trunkRevision === null)
    throw new FreshnessRefusal("trunk-absent", `${trackingRef} is not fetched`);
  let base;
  try {
    base = (
      await git(repository, "merge-base", trunkRevision, "HEAD")
    ).stdout.trim();
  } catch {
    throw new FreshnessRefusal(
      "no-common-base",
      `HEAD shares no history with ${trackingRef}`,
    );
  }
  const entry = await takenEntry(repository, trunkRevision, identity);
  if (!entry)
    throw new FreshnessRefusal(
      "not-taken",
      `${identity} is not Taken in the backlog on fetched ${trackingRef}`,
    );
  if (!(await takenEntry(repository, base, identity)))
    throw new FreshnessRefusal(
      "not-taken-at-base",
      `${identity} is not Taken in the backlog at the branch's base ${base}`,
    );
  let homePath;
  let planPath;
  try {
    const selection = selectedPreparation(repository, entry.href);
    homePath = selection.homePath;
    if (entry.plan) planPath = selection.linkedPlanPath(entry.plan.target);
  } catch (error) {
    throw new FreshnessRefusal("invalid-source-link", error.message);
  }
  const read = async (path) => ({
    base: await show(repository, base, path),
    trunk: await show(repository, trunkRevision, path),
    branch: await workingText(repository, path),
  });
  const absent = (record, path, texts) => {
    const where = Object.keys(texts).filter(
      (at) => texts[at] === undefined || texts[at] === null,
    );
    if (where.length)
      throw new FreshnessRefusal(
        "record-absent",
        `${record} ${path} is absent at ${where.join(", ")}`,
        { record, path, absentAt: where },
      );
  };
  // `compare` of one record from its base, named as `from`: the newest trunk
  // revision of `path` since the Take whose record text (`textAt`) the branch
  // copy contains, meaning its application from the Take leaves the branch
  // copy as it is; otherwise the Take.
  const fromContainedBase = async (compare, { textAt, ...fields }) => {
    const request = { repository, ...fields };
    const since = (
      await git(
        repository,
        "rev-list",
        `${base}..${trunkRevision}`,
        "--",
        fields.path,
      )
    ).stdout
      .split("\n")
      .filter(Boolean);
    let from = { revision: base, text: fields.base };
    for (const revision of since) {
      const text = await textAt(revision);
      if (text === undefined || text === null) continue;
      const { state } = await compare({ ...request, trunk: text });
      if (state === "unchanged") {
        from = { revision, text };
        break;
      }
    }
    const { record, path, ...compared } = await compare({
      ...request,
      base: from.text,
    });
    return { record, path, from: from.revision, ...compared };
  };
  const homes = await read(homePath);
  const writes = new Map();
  const records = [];
  const comparePlan = async (path, texts) => {
    const plan = await fromContainedBase(compareRecord, {
      record: "plan",
      path,
      ...texts,
      textAt: (revision) => show(repository, revision, path),
      doneRule: true,
    });
    if (plan.state === "amended") writes.set(path, plan.application);
    records.push(plan);
  };
  // Without a plan link, a home that reads as a plan is a plan-homed
  // correction: the whole document is its only record. Reading the document,
  // not the link's anchor, also detects a plan home without a story state.
  if (
    !planPath &&
    homes.base !== null &&
    readPlanSlices(homes.base).status === "interpreted"
  ) {
    absent("plan", homePath, homes);
    await comparePlan(homePath, homes);
  } else {
    const sections = Object.fromEntries(
      Object.entries(homes).map(([at, home]) => [
        at,
        sectionOf(home, entry.href),
      ]),
    );
    absent("story", homePath, sections);
    const plans = planPath && (await read(planPath));
    if (plans) absent("plan", planPath, plans);
    const story = await fromContainedBase(compareSection, {
      record: "story",
      path: homePath,
      base: sections.base.text,
      trunk: sections.trunk.text,
      branch: sections.branch.text,
      textAt: async (revision) =>
        sectionOf(await show(repository, revision, homePath), entry.href)?.text,
    });
    if (story.state === "amended")
      writes.set(homePath, withSection(sections.branch, story.application));
    records.push(story);
    if (plans) await comparePlan(planPath, plans);
  }
  const result = records.some((item) => item.state === "conflict")
    ? "conflict"
    : records.some((item) => item.state === "amended")
      ? "amended"
      : "unchanged";
  return {
    report: { result, identity, trunkRevision, base, records },
    writes,
  };
}

// Writes the clean application of every amended record into the working
// tree; a conflict writes nothing.
export async function absorbSourceFreshness(request) {
  const { report, writes } = await inspectSourceFreshness(request);
  if (report.result === "conflict") return { ...report, written: [] };
  const repository = resolve(request.workspace);
  for (const [path, text] of writes)
    await writeFile(join(repository, path), text);
  return { ...report, written: [...writes.keys()] };
}

function requestOf(argv) {
  const [command, ...flags] = argv;
  const usage =
    "usage: source-freshness.mjs check|absorb --workspace <path> --identity <id> --remote <remote> --target-ref refs/heads/<trunk>";
  if (!["check", "absorb"].includes(command))
    throw new FreshnessRefusal("invalid-input", usage);
  let values;
  try {
    values = flagValues(flags);
  } catch (error) {
    throw new FreshnessRefusal("invalid-input", `${error.message}; ${usage}`);
  }
  const { workspace, identity, remote, targetRef, ...unknown } = values;
  if (
    !workspace ||
    !identity ||
    !remote ||
    !targetRef ||
    Object.keys(unknown).length
  )
    throw new FreshnessRefusal("invalid-input", usage);
  if (!targetRef.startsWith("refs/heads/"))
    throw new FreshnessRefusal(
      "invalid-input",
      `authorized target must be a branch ref: ${targetRef}`,
    );
  return { command, request: { workspace, identity, remote, targetRef } };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  let output;
  try {
    const { command, request } = requestOf(process.argv.slice(2));
    output =
      command === "absorb"
        ? await absorbSourceFreshness(request)
        : (await inspectSourceFreshness(request)).report;
    process.exitCode =
      command === "absorb" && output.result === "amended"
        ? 0
        : exitCodes[output.result];
  } catch (error) {
    const refusal =
      error instanceof FreshnessRefusal
        ? error
        : new FreshnessRefusal("failed", error.message.trim());
    output = {
      result: "refused",
      reason: refusal.reason,
      message: refusal.message,
      ...refusal.fields,
    };
    process.exitCode = exitCodes.refused;
  }
  process.stdout.write(`${JSON.stringify(output)}\n`);
}
