// The planned-delivery status step: the selected slice becomes done in the
// working plan, and no other slice may advance beside it.
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { git } from "./publication-git.mjs";
import {
  locatePlanSliceStatuses,
  readPlanSlices,
} from "../../dough-product-backlog/scripts/product-backlog-plan-reader.mjs";
import {
  joinSource,
  splitSource,
} from "../../dough-product-backlog/scripts/product-backlog-source.mjs";

// Done slice indexes in the checkout's committed plan; an untracked plan has
// none, so every other done slice in the working plan is newly done.
async function committedDoneSlices(workspace, path) {
  const shown = await git(workspace, "show", `HEAD:./${path}`).catch(
    () => undefined,
  );
  const read = shown && readPlanSlices(shown.stdout);
  return new Set(
    (read?.slices ?? [])
      .filter((slice) => slice.status === "done")
      .map((slice) => slice.index),
  );
}

// Marks only the selected slice's real status line done; an already-done
// slice is left as it is, so a repeated request changes nothing. Refuses,
// before writing, a missing, duplicated or unreadable selected slice, and any
// other slice done in the working plan but not in the committed plan.
export async function markSelectedSliceDone(workspace, { path, slice }) {
  const file = resolve(workspace, path);
  const source = await readFile(file, "utf8");
  const read = locatePlanSliceStatuses(source);
  if (read.status !== "interpreted")
    throw new Error(`slice ${slice}: ${read.problem}`);
  const selected = read.slices.filter((item) => item.index === slice);
  if (!selected.length) throw new Error(`slice ${slice} is not in ${path}`);
  if (selected.length > 1)
    throw new Error(`slice ${slice} heading is duplicated in ${path}`);
  const committed = await committedDoneSlices(workspace, path);
  const advanced = read.slices
    .filter(
      (item) =>
        item.index !== slice &&
        item.status === "done" &&
        !committed.has(item.index),
    )
    .map((item) => item.index);
  if (advanced.length)
    throw new Error(
      `slice ${[...new Set(advanced)].join(", ")} is done in ${path} but not in its committed plan; only slice ${slice} may become done`,
    );
  if (selected[0].status === "done") return;
  const split = splitSource(source);
  const line = selected[0].statusLine;
  split.lines[line] = split.lines[line].replace(/\bplanned\b/, "done");
  await writeFile(file, joinSource(split));
}
