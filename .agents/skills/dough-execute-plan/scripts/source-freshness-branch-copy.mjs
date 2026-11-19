// The branch copy of a source-freshness record, for `source-freshness.mjs`:
// the workspace's working tree, meaning committed increments plus any
// uncommitted edit of the slice being delivered.
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { joinSource } from "../../dough-product-backlog/scripts/product-backlog-source.mjs";

// The working-tree text of `path`, or null when the file is absent.
export async function workingText(workspace, path) {
  try {
    return await readFile(join(workspace, path), "utf8");
  } catch (error) {
    if (error.code === "ENOENT") return null;
    throw error;
  }
}

// `text` in place of the branch copy's selected section, keeping every
// sibling section and the separator lines that follow it byte for byte.
export function withSection(branchSection, text) {
  const { document, region } = branchSection;
  const kept = branchSection.text.split("\n").length;
  const lines = [
    ...document.lines.slice(0, region.start),
    ...text.split("\n"),
    ...document.lines.slice(region.start + kept),
  ];
  return joinSource({ ...document, lines });
}
