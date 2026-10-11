// Removes exactly one explicitly named work item from whichever active list
// holds it, leaving every sibling, the order of both lists, the direction, and
// the text around them as they were.
//
// The caller has already decided that the work is done; this applies that one
// decision and nothing else. It never judges completion: a missing plan file,
// a missing seed, and a plan's status text establish nothing here, and none of
// them can make an entry removable. It never deletes a story or plan file
// either. Closing the work's canonical homes, with its seed, plan, and proof
// cleanup, stays with the wrap-up workflow that calls this. The other files it
// changes sit beside the backlog: it removes the execution agent profile that
// names the completed identity, so the name that held the work is released in
// the same change; for finished rather than dropped work it writes the work's
// done record; it removes done records older than their window; and it
// rebuilds the done catalog from the record files those changes leave. A
// preparation assignment is never ended by completing work.

import {
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createHash } from "node:crypto";
import { join } from "node:path";
import {
  agentIdentity,
  agentProfileDirectory,
  parseAgentProfile,
  profileAgentName,
} from "./product-backlog-agent-profile.mjs";
import { parseBacklog, renderBacklog } from "./product-backlog-document.mjs";
import {
  catalogDoneRecords,
  doneCatalogPath,
  renderDoneCatalog,
} from "./product-backlog-done-catalog.mjs";
import {
  doneRecordDirectory,
  doneRecordPath,
  isDoneRecordFileName,
  isWithinDoneWindow,
  parseDoneRecordFile,
  renderDoneRecord,
} from "./product-backlog-done-record.mjs";
import { configuredUserName } from "./product-backlog-git-repository.mjs";
import { findEntry, removeEntryLine } from "./product-backlog-placement.mjs";
import { replaceFile } from "./product-backlog-store.mjs";

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

// Changes the files beside the backlog in `backlogDirectory` that the removal
// of `entry` at `now` (a Date) closes: releases its execution agent profile,
// writes its done record unless the work was `dropped`, prunes expired done
// records, and rebuilds the done catalog from the records that remain.
// Returns the released profiles, the written record's path (undefined for
// dropped work), the expired records' paths, and the catalog outcome. The
// caller holds the backlog's lock, so cooperating completions rebuild the
// catalog one after another from each other's final records.
export function closeBesideBacklog(backlogDirectory, { entry, dropped, now }) {
  const released = releaseAgentProfiles(backlogDirectory, entry.identity);
  const record = dropped
    ? undefined
    : writeDoneRecord(backlogDirectory, {
        entry,
        profile: released[0]?.profile,
        developer: configuredUserName(backlogDirectory),
        now,
      });
  const expired = pruneDoneRecords(backlogDirectory, now);
  const catalog = rebuildDoneCatalog(backlogDirectory);
  return { released, record, expired, catalog };
}

// Removes every readable execution agent profile beside the backlog whose
// identity is the completed one, and returns each removed path relative to
// that directory with the profile it held. An unreadable profile names no
// identity, so it is left for a person to read; a preparation profile ends
// only through its own release.
function releaseAgentProfiles(backlogDirectory, identity) {
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
      released.push({ path, profile: read.profile });
    }
  }
  return released;
}

// Writes the done record of the removed `entry`, completed at `now` (a Date)
// by `developer` (undefined when the workspace names none), with the agent,
// host, and model of the execution `profile` it held, if any. Returns the
// record's path relative to the backlog directory. A record already written
// for the same identity is replaced.
function writeDoneRecord(backlogDirectory, { entry, profile, developer, now }) {
  const path = doneRecordPath(entry.identity);
  mkdirSync(join(backlogDirectory, doneRecordDirectory), { recursive: true });
  writeFileSync(
    join(backlogDirectory, path),
    renderDoneRecord({
      identity: entry.identity,
      title: entry.title,
      completedAt: now.toISOString(),
      developer,
      ...(profile === undefined
        ? {}
        : {
            agent: agentIdentity(profile.name).agent,
            host: profile.host,
            model: profile.model,
          }),
    }),
    "utf8",
  );
  return path;
}

// Removes every readable done record completed more than the window before
// `now`, and returns the removed paths relative to the backlog directory. An
// unreadable record names no completion time, so it is left for a person to
// read.
function pruneDoneRecords(backlogDirectory, now) {
  const directory = join(backlogDirectory, doneRecordDirectory);
  if (!existsSync(directory)) return [];
  const removed = [];
  for (const fileName of readdirSync(directory).sort()) {
    if (!isDoneRecordFileName(fileName)) continue;
    const path = `${doneRecordDirectory}/${fileName}`;
    const read = parseDoneRecordFile(
      fileName,
      readFileSync(join(backlogDirectory, path), "utf8"),
    );
    if (read.ok && !isWithinDoneWindow(read.record.completedAt, now)) {
      rmSync(join(backlogDirectory, path));
      removed.push(path);
    }
  }
  return removed;
}

// The Git blob hash of `bytes`: the name Git gives that exact content, so a
// published listing of the record files can be compared with the catalog
// without reading each record.
function gitBlobHash(bytes) {
  return createHash("sha1")
    .update(`blob ${bytes.length}\0`)
    .update(bytes)
    .digest("hex");
}

// Rebuilds the done catalog beside the backlog in `backlogDirectory` from the
// record files there, changing no record. Records the caller should already
// have made final; the catalog is only ever derived from them. Returns the
// catalog's path relative to that directory, how many records it lists, the
// unreadable record files it names, and whether it was "written", already
// "unchanged", "removed" because no record file remains, or stays "absent".
export function rebuildDoneCatalog(backlogDirectory) {
  const directory = join(backlogDirectory, doneRecordDirectory);
  const catalogFile = join(backlogDirectory, doneCatalogPath);
  const files = existsSync(directory)
    ? readdirSync(directory)
        .filter(isDoneRecordFileName)
        .sort()
        .map((fileName) => {
          const bytes = readFileSync(join(directory, fileName));
          return {
            fileName,
            text: bytes.toString("utf8"),
            blob: gitBlobHash(bytes),
          };
        })
    : [];
  const path = doneCatalogPath;
  if (files.length === 0) {
    if (!existsSync(catalogFile))
      return { path, status: "absent", records: 0, unreadable: [] };
    rmSync(catalogFile);
    return { path, status: "removed", records: 0, unreadable: [] };
  }
  const catalog = catalogDoneRecords(files);
  const summary = {
    path,
    records: catalog.records.length,
    unreadable: catalog.unreadable.map(
      ({ fileName }) => `${doneRecordDirectory}/${fileName}`,
    ),
  };
  const text = renderDoneCatalog(catalog);
  if (existsSync(catalogFile) && readFileSync(catalogFile, "utf8") === text)
    return { ...summary, status: "unchanged" };
  replaceFile(catalogFile, text);
  return { ...summary, status: "written" };
}
