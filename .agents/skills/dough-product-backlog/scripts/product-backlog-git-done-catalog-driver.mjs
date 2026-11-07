#!/usr/bin/env node
// A Git merge driver for the done catalog beside the backlog. The catalog is
// derived from the record files, and Git may merge it before those files
// (its name sorts first), so this driver cannot rebuild it. It only keeps a
// change to the catalog alone from ever stopping the operation: it writes a
// provisional combination of both sides' rows keyed by record file name into
// the "ours" temp file, or leaves that file's bytes as they are when either
// side does not read back as a catalog, and always reports success. The
// adapter that runs the operation rebuilds the catalog from the final record
// files, which settles its content.
import { readFileSync, writeFileSync } from "node:fs";
import {
  inPublishedOrder,
  parseDoneCatalog,
  renderDoneCatalog,
} from "./product-backlog-done-catalog.mjs";

// Each catalogued file name's row, with the list that holds it.
const rowsByFileName = (catalog) =>
  new Map(
    ["records", "unreadable"].flatMap((list) =>
      catalog[list].map((entry) => [entry.fileName, { list, entry }]),
    ),
  );

const sameRow = (a, b) => JSON.stringify(a) === JSON.stringify(b);

// Each row one side changed since the ancestor (an unreadable ancestor counts
// as an empty catalog) takes that side's row; a row both sides changed keeps
// ours. Undefined when either side does not read back as a catalog.
function combineDoneCatalogs(ancestorText, oursText, theirsText) {
  const ours = parseDoneCatalog(oursText);
  const theirs = parseDoneCatalog(theirsText);
  if (!ours.ok || !theirs.ok) return undefined;
  const ancestor = parseDoneCatalog(ancestorText);
  const before = rowsByFileName(
    ancestor.ok ? ancestor.catalog : { records: [], unreadable: [] },
  );
  const mine = rowsByFileName(ours.catalog);
  const other = rowsByFileName(theirs.catalog);
  const combined = { records: [], unreadable: [] };
  for (const name of new Set([
    ...before.keys(),
    ...mine.keys(),
    ...other.keys(),
  ])) {
    const row = sameRow(mine.get(name), before.get(name))
      ? other.get(name)
      : mine.get(name);
    if (row !== undefined) combined[row.list].push(row.entry);
  }
  return renderDoneCatalog(inPublishedOrder(combined));
}

const [, , ancestorPath, oursPath, theirsPath] = process.argv;
const combined = combineDoneCatalogs(
  readFileSync(ancestorPath, "utf8"),
  readFileSync(oursPath, "utf8"),
  readFileSync(theirsPath, "utf8"),
);
if (combined !== undefined) writeFileSync(oursPath, combined, "utf8");
