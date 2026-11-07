// What a done catalog is: the small published index of the done records
// beside one backlog, so a reader can order them and choose which to read
// without reading every record. It is derived, never authored: it is rebuilt
// from the record files themselves after they are final, and a catalog that
// no longer matches them is a gap to repair, not a history to merge.
//
// Each readable record contributes its file name, identity, completion time,
// and the Git blob hash of its text, newest completion first. Each record file
// that cannot be read keeps its file name and blob hash under "unreadable",
// with no completion time made up for it. A record's title and its developer,
// agent, host, and model stay in the record alone.
//
// The catalog's spelling, how it is read back, and how it is checked against
// a published listing of the record files have one owner here. No
// filesystem, Git, or Node-only imports, so every reader shares it.

import {
  doneRecordDirectory,
  doneRecordFileName,
  isCompletionTime,
  isDoneRecordFileName,
  parseDoneRecordFile,
} from "./product-backlog-done-record.mjs";

/**
 * A catalogued record: its file name, identity, completion time, and blob.
 * @typedef {{ fileName: string, identity: string, completedAt: string, blob: string }} CataloguedDoneRecord
 * A record file the catalog could not read: its file name and blob alone.
 * @typedef {{ fileName: string, blob: string }} UnreadableDoneRecordEntry
 * @typedef {{ records: CataloguedDoneRecord[], unreadable: UnreadableDoneRecordEntry[] }} DoneCatalog
 */

// The catalog sits among the records it indexes. Its leading "." keeps it
// outside the names a done record can take.
export const doneCatalogFileName = ".catalog.json";

// The catalog's path relative to the backlog's directory.
export const doneCatalogPath = `${doneRecordDirectory}/${doneCatalogFileName}`;

const blobHash = /^(?:[0-9a-f]{40}|[0-9a-f]{64})$/;

// Newest completion first; records completed at the same time follow their
// file names, so equal record sets always produce the same catalog.
function catalogOrder(a, b) {
  if (a.completedAt !== b.completedAt)
    return a.completedAt < b.completedAt ? 1 : -1;
  return byFileName(a, b);
}

function byFileName(a, b) {
  if (a.fileName === b.fileName) return 0;
  return a.fileName < b.fileName ? -1 : 1;
}

/**
 * `catalog`'s two lists, sorted in place into the order a catalog is
 * published in.
 * @param {DoneCatalog} catalog
 * @returns {DoneCatalog}
 */
export function inPublishedOrder({ records, unreadable }) {
  return {
    records: records.sort(catalogOrder),
    unreadable: unreadable.sort(byFileName),
  };
}

/**
 * The catalog of the record files `files` lists, each with its text and the
 * Git blob hash of that text.
 * @param {{ fileName: string, text: string, blob: string }[]} files
 */
export function catalogDoneRecords(files) {
  const records = [];
  const unreadable = [];
  for (const { fileName, text, blob } of files) {
    const read = parseDoneRecordFile(fileName, text);
    if (read.ok) {
      const { identity, completedAt } = read.record;
      records.push({ fileName, identity, completedAt, blob });
    } else {
      unreadable.push({ fileName, blob });
    }
  }
  return inPublishedOrder({ records, unreadable });
}

/** @param {{ records: object[], unreadable: object[] }} catalog */
export function renderDoneCatalog({ records, unreadable }) {
  const catalog = {
    schemaVersion: 1,
    records: records.map(({ fileName, identity, completedAt, blob }) => ({
      fileName,
      identity,
      completedAt,
      blob,
    })),
    unreadable: unreadable.map(({ fileName, blob }) => ({ fileName, blob })),
  };
  return `${JSON.stringify(catalog, null, 2)}\n`;
}

const isObject = (value) =>
  value !== null && typeof value === "object" && !Array.isArray(value);

const hasExactly = (value, keys) =>
  isObject(value) &&
  Object.keys(value).length === keys.length &&
  keys.every((key) => Object.hasOwn(value, key));

function recordError(entry) {
  if (!hasExactly(entry, ["fileName", "identity", "completedAt", "blob"]))
    return "a catalogued record holds exactly fileName, identity, completedAt, and blob";
  const { fileName, identity, completedAt, blob } = entry;
  if (typeof identity !== "string" || identity === "")
    return "a catalogued record requires an identity";
  if (doneRecordFileName(identity) !== fileName)
    return `catalogued record ${fileName} names another identity`;
  if (!isCompletionTime(completedAt))
    return `catalogued record ${fileName} requires a UTC ISO completion time`;
  if (typeof blob !== "string" || !blobHash.test(blob))
    return `catalogued record ${fileName} requires a Git blob hash`;
  return undefined;
}

function unreadableError(entry) {
  if (!hasExactly(entry, ["fileName", "blob"]))
    return "an unreadable record entry holds exactly fileName and blob";
  const { fileName, blob } = entry;
  if (typeof fileName !== "string" || !isDoneRecordFileName(fileName))
    return "an unreadable record entry requires a done record file name";
  if (typeof blob !== "string" || !blobHash.test(blob))
    return `unreadable record ${fileName} requires a Git blob hash`;
  return undefined;
}

function sorted(entries, order) {
  return entries.every(
    (entry, index) => index === 0 || order(entries[index - 1], entry) < 0,
  );
}

// Reads published catalog text back: { ok: true, catalog } or
// { ok: false, error }. Anything other than what renderDoneCatalog writes —
// another schema version, an extra fact, a repeated file name, or another
// order — is refused rather than interpreted.
/**
 * @param {string} text
 * @returns {{ ok: true, catalog: DoneCatalog } | { ok: false, error: string }}
 */
export function parseDoneCatalog(text) {
  let data;
  try {
    data = JSON.parse(text);
  } catch {
    return { ok: false, error: "done catalog is not JSON" };
  }
  if (!isObject(data) || data.schemaVersion !== 1)
    return { ok: false, error: "done catalog schemaVersion must be 1" };
  if (
    !hasExactly(data, ["schemaVersion", "records", "unreadable"]) ||
    !Array.isArray(data.records) ||
    !Array.isArray(data.unreadable)
  )
    return {
      ok: false,
      error:
        "done catalog holds exactly schemaVersion, records, and unreadable",
    };
  const error =
    data.records.map(recordError).find(Boolean) ??
    data.unreadable.map(unreadableError).find(Boolean);
  if (error) return { ok: false, error };
  if (
    !sorted(data.records, catalogOrder) ||
    !sorted(data.unreadable, byFileName)
  )
    return { ok: false, error: "done catalog is not in its published order" };
  const names = [...data.records, ...data.unreadable].map(
    ({ fileName }) => fileName,
  );
  if (new Set(names).size !== names.length)
    return { ok: false, error: "done catalog names one record file twice" };
  return {
    ok: true,
    catalog: { records: data.records, unreadable: data.unreadable },
  };
}

/**
 * Why `catalog` does not describe the record files a published listing holds
 * beside the same backlog, or undefined when it describes exactly them: every
 * listed record file once, at the same blob hash, and nothing else. Only the
 * names a done record can take count as listed record files.
 * @param {DoneCatalog} catalog
 * @param {{ fileName: string, blob: string }[]} listing
 */
export function doneCatalogMismatch(catalog, listing) {
  const catalogued = new Map(
    [...catalog.records, ...catalog.unreadable].map(({ fileName, blob }) => [
      fileName,
      blob,
    ]),
  );
  const listed = listing.filter(({ fileName }) =>
    isDoneRecordFileName(fileName),
  );
  for (const { fileName, blob } of listed) {
    if (!catalogued.has(fileName))
      return `done catalog does not list ${doneRecordDirectory}/${fileName}`;
    if (catalogued.get(fileName) !== blob)
      return `done catalog describes an earlier ${doneRecordDirectory}/${fileName}`;
  }
  const published = new Set(listed.map(({ fileName }) => fileName));
  const extra = [...catalogued.keys()].find((name) => !published.has(name));
  if (extra !== undefined)
    return `done catalog lists ${doneRecordDirectory}/${extra}, which is not published`;
  return undefined;
}
