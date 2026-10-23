// The identity-evidence sources read from files beside the backlog: the
// active seed directory, the backlog's two lists, and the done records and
// their catalog. Each source names the identities it shows, with the file or
// record behind each, and every file it could not read as a gap; a source it
// could not read at all is "unread". The history source
// (`product-backlog-identity-history.mjs`) reports in the same shape.

import { existsSync, readFileSync, readdirSync } from "node:fs";
import { join, relative, sep } from "node:path";
import { parseBacklog } from "./product-backlog-document.mjs";
import {
  doneCatalogMismatch,
  doneCatalogPath,
  parseDoneCatalog,
} from "./product-backlog-done-catalog.mjs";
import {
  doneRecordDirectory,
  parseDoneRecordFile,
} from "./product-backlog-done-record.mjs";
import { documentIdIn } from "./product-backlog-home-reader.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

// A seed ID's number is the digits it ends with ("SEED-084" is 84, written
// three digits wide). An identity is read by its seed ID, before any anchor;
// a plan-path identity ends in no digits and names no seed number.
const seedIdPattern = /^(?<prefix>.*\D)(?<digits>\d+)$/;

export function seedNumberOf(identity) {
  const seed = identity.split("#")[0];
  const match = seedIdPattern.exec(seed);
  if (!match) return undefined;
  const { prefix, digits } = match.groups;
  return { seed, prefix, width: digits.length, number: Number(digits) };
}

export const posixPath = (path) => path.split(sep).join("/");

// A source that could not be read at all names nothing and one gap.
export const unread = (source, path, reason) => ({
  source,
  path,
  read: false,
  named: [],
  gaps: [{ path, reason }],
});

export function seedDirectorySource(backlogDirectory, directory) {
  const path = posixPath(relative(backlogDirectory, directory)) || ".";
  if (!existsSync(directory)) return unread("seed-directory", path, "absent");
  const named = [];
  const gaps = [];
  for (const fileName of readdirSync(directory).sort()) {
    if (!fileName.endsWith(".md")) continue;
    const evidence = `${path}/${fileName}`;
    const id = documentIdIn(readFileSync(join(directory, fileName), "utf8"));
    if (id === "") {
      gaps.push({ path: evidence, reason: "front matter names no seed ID" });
    } else {
      named.push({ identity: id, evidence });
    }
  }
  return { source: "seed-directory", path, read: true, named, gaps };
}

export function backlogSource(backlogDirectory, file) {
  const path = posixPath(relative(backlogDirectory, file));
  if (!existsSync(file)) return unread("backlog", path, "absent");
  let document;
  try {
    document = parseBacklog(readFileSync(file, "utf8"));
  } catch (error) {
    if (!(error instanceof BacklogError)) throw error;
    return unread("backlog", path, error.message);
  }
  return {
    source: "backlog",
    path,
    read: true,
    named: document.entries.map(({ identity, list }) => ({
      identity,
      evidence: `${path} "## ${list}"`,
      list,
    })),
    gaps: [],
  };
}

export function doneRecordsSource(files) {
  const named = [];
  const gaps = [];
  for (const { fileName, text } of files) {
    const path = `${doneRecordDirectory}/${fileName}`;
    const read = parseDoneRecordFile(fileName, text);
    if (read.ok) {
      named.push({ ...read.record, evidence: path });
    } else {
      gaps.push({ path, reason: read.error });
    }
  }
  return {
    source: "done-records",
    path: doneRecordDirectory,
    read: true,
    named,
    gaps,
  };
}

// The catalog is an index of the record files, so one that is absent while
// records exist, unreadable, or describing other files than those present is
// a gap rather than evidence that nothing else was completed.
export function doneCatalogSource(backlogDirectory, files) {
  const path = doneCatalogPath;
  const catalogFile = join(backlogDirectory, doneCatalogPath);
  if (!existsSync(catalogFile)) {
    if (files.length === 0)
      return { source: "done-catalog", path, read: true, named: [], gaps: [] };
    return unread("done-catalog", path, "absent");
  }
  const read = parseDoneCatalog(readFileSync(catalogFile, "utf8"));
  if (!read.ok) return unread("done-catalog", path, read.error);
  const mismatch = doneCatalogMismatch(read.catalog, files);
  return {
    source: "done-catalog",
    path,
    read: true,
    named: read.catalog.records.map(({ identity, fileName }) => ({
      identity,
      evidence: `${doneRecordDirectory}/${fileName}`,
    })),
    gaps: mismatch === undefined ? [] : [{ path, reason: mismatch }],
  };
}
