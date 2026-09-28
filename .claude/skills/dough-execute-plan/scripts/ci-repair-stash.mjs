#!/usr/bin/env node
// Installed CLI for the CI repair pause: saves this execution checkout's
// unfinished work as one stash entry and later restores and drops only that
// entry, identified by OID and confirmed by the OID Git reports dropping, so
// every other writer's stash entries survive.
import { mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import {
  appliedState,
  conflictPaths,
  unrestored,
} from "./ci-repair-stash-applied.mjs";
import {
  dropExact,
  gitOutputOrNull,
  inventory,
  isDirty,
  stashEntries,
  stashEntryByOid,
} from "./ci-repair-stash-git.mjs";
import { git } from "./publication-git.mjs";

const usage =
  "usage: ci-repair-stash.mjs save --checkout PATH --label TEXT | restore --record FILE | drop --record FILE";

function failureText(error) {
  return `${error.stdout ?? ""}\n${error.stderr ?? ""}\n${error.message ?? ""}`.trim();
}

async function storeRecord(file, record) {
  await writeFile(file, `${JSON.stringify(record, null, 2)}\n`, {
    mode: 0o600,
  });
}

async function writeRecord(record) {
  const directory = await mkdtemp(join(tmpdir(), "dough-ci-repair-stash-"));
  const file = join(directory, "record.json");
  await storeRecord(file, record);
  return file;
}

export async function saveRepairStash({ checkout, label }) {
  if (!checkout || !label) throw new Error(usage);
  const root = (
    await git(resolve(checkout), "rev-parse", "--show-toplevel")
  ).stdout.trim();
  const before = await stashEntries(root);
  const record = {
    checkout: root,
    label,
    branch: await gitOutputOrNull(
      root,
      "symbolic-ref",
      "--short",
      "-q",
      "HEAD",
    ),
    head: (await git(root, "rev-parse", "HEAD")).stdout.trim(),
    ...(await inventory(root)),
    previousTop: before[0]?.oid ?? null,
    oid: null,
  };
  if (!isDirty(record)) {
    record.status = "clean";
  } else {
    let pushed = true;
    try {
      await git(root, "stash", "push", "--include-untracked", "-m", label);
    } catch (error) {
      pushed = false;
      record.status = "failed";
      record.error = failureText(error);
    }
    if (pushed) {
      const known = new Set(before.map((entry) => entry.oid));
      const created = (await stashEntries(root)).filter(
        (entry) =>
          !known.has(entry.oid) && entry.subject.endsWith(`: ${label}`),
      );
      const remaining = await inventory(root);
      if (isDirty(remaining)) record.remaining = remaining;
      if (created.length === 1) {
        record.oid = created[0].oid;
        record.status = isDirty(remaining) ? "unclean" : "stashed";
      } else {
        // The push succeeded but did not yield exactly one entry of ours.
        record.status = "ambiguous";
        record.candidates = created.map((entry) => entry.oid);
      }
    }
  }
  const file = await writeRecord(record);
  return {
    ok: record.status === "stashed" || record.status === "clean",
    record: file,
    ...record,
  };
}

async function readRecord(file) {
  if (!file) throw new Error(usage);
  return JSON.parse(await readFile(file, "utf8"));
}

// Remembers the last restore outcome in its record, so a later drop can
// refuse to discard an entry whose restore put nothing back.
export async function restoreRepairStash({ record: file }) {
  const record = await readRecord(file);
  const result = await restoreRecorded(file, record);
  record.restore = { status: result.status, applied: result.applied };
  await storeRecord(file, record);
  return result;
}

async function restoreRecorded(file, record) {
  const receipt = {
    record: file,
    oid: record.oid,
    applied: false,
    dropped: null,
  };
  if (record.status === "ambiguous")
    return { ok: false, status: "ambiguous", ...receipt, paths: [] };
  if (!record.oid) return { ok: true, status: "resumed", ...receipt };
  const checkout = record.checkout;
  if (!(await stashEntryByOid(checkout, record.oid)))
    return { ok: false, status: "missing", ...receipt, paths: [] };
  try {
    await git(checkout, "stash", "apply", "--index", record.oid);
  } catch (error) {
    const output = failureText(error);
    return {
      ok: false,
      status: "conflict",
      ...receipt,
      ...(await appliedState(record)),
      paths: await conflictPaths(checkout, output),
      error: output,
    };
  }
  const missingPaths = unrestored(record, await inventory(checkout));
  if (missingPaths.length > 0)
    return {
      ok: false,
      status: "conflict",
      ...receipt,
      ...(await appliedState(record)),
      paths: missingPaths,
      error: "applied work does not match the saved inventory; entry kept",
    };
  // A vanished entry reports no drop; a mismatch keeps the entry and names
  // the other writer's entry Git dropped instead of claiming this one went.
  receipt.applied = true;
  const drop = await dropExact(checkout, record.oid);
  if (drop.status === "mismatch") return { ok: false, ...receipt, ...drop };
  return { ok: true, status: "resumed", ...receipt, dropped: drop.selector };
}

// Finishes a resolved conflict: drops only the recorded OID's entry, never
// one whose last restore applied nothing (that work exists only there).
export async function dropRepairStash({ record: file }) {
  const { checkout, oid, restore } = await readRecord(file);
  if (restore?.applied === "none")
    return {
      ok: false,
      record: file,
      oid,
      status: "unapplied",
      selector: null,
    };
  const drop = await dropExact(checkout, oid);
  return { ok: drop.status === "dropped", record: file, oid, ...drop };
}

function argumentsOf(argv) {
  const result = { command: argv[0] };
  for (let index = 1; index < argv.length; index += 2) {
    if (!argv[index].startsWith("--") || index + 1 >= argv.length)
      throw new Error(`invalid argument ${argv[index]}\n${usage}`);
    result[argv[index].slice(2)] = argv[index + 1];
  }
  return result;
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const { command, ...options } = argumentsOf(process.argv.slice(2));
    const operation = {
      save: saveRepairStash,
      restore: restoreRepairStash,
      drop: dropRepairStash,
    }[command];
    if (!operation) throw new Error(usage);
    const result = await operation(options);
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
