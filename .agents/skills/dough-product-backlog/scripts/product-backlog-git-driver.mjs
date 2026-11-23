#!/usr/bin/env node
// A Git merge driver for one attributed path: the product backlog. Git
// invokes a merge driver with the real ancestor/ours/theirs temp file paths
// (`%O %A %B`) whenever a path changed on both sides of a merge — clean or
// conflicted alike — which is exactly the immutable input this tool's shared
// resolver needs and exactly the moment a plain textual merge would otherwise
// combine two backlog changes without ever asking whether they agree.
//
// The driver reads the files Git supplies and passes their text to the
// shared resolver. Success overwrites the "ours" file with the reconciled
// backlog, which Git then takes as the merged content. Refusal never writes
// a partial result: it leaves the "ours" temp file as real Git-style conflict
// markers around each side's untouched content, with the dispute explained
// between them, and exits non-zero so Git records the path as unmerged with
// its own stage 1/2/3 populated from the original ancestor/ours/theirs blobs.
import { readFileSync, writeFileSync } from "node:fs";
import { mergeBacklogs } from "./product-backlog-merge.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { readFile } from "./product-backlog-store.mjs";

// Show the policy's explanation together with the actual path and stages.
function gitFacingRefusal(error, path) {
  const body = error.message.replace(
    /\nDecide what each of these should say, repair the versions by hand, and merge them again\.$/,
    "",
  );
  return [
    `Git could not combine both sides' changes to ${path} automatically.`,
    body,
    `Inspect each side through Git itself:`,
    `  git show :1:${path}   # the ancestor`,
    `  git show :2:${path}   # your side`,
    `  git show :3:${path}   # their side`,
    `Resolve ${path} by hand, then \`git add ${path}\`, then continue the operation.`,
  ].join("\n");
}

// The recoverable shape of a stopped merge for this path: ordinary Git
// conflict markers so the usual tools still recognise it, carrying the two
// full sides across untouched and the dispute as a Markdown comment between
// them, so resolving it is deleting the markers and the comment around the
// side that should stand — or writing a new decision — the same as any other
// Git conflict.
function conflictMarkers(oursPath, theirsPath, explanation) {
  const ours = readFileSync(oursPath, "utf8");
  const theirs = readFileSync(theirsPath, "utf8");
  const noted = explanation
    .split("\n")
    .map((line) => `     ${line}`)
    .join("\n");
  const oursBody = ours.endsWith("\n") ? ours : `${ours}\n`;
  const theirsBody = theirs.endsWith("\n") ? theirs : `${theirs}\n`;
  return (
    `<<<<<<< ours\n` +
    `<!--\n${noted}\n-->\n` +
    `${oursBody}` +
    `=======\n` +
    `${theirsBody}` +
    `>>>>>>> theirs\n`
  );
}

function runDriver(ancestorPath, oursPath, theirsPath, path) {
  const named = path ?? oursPath;
  try {
    const outcome = mergeBacklogs(
      readFile(ancestorPath, `Git's ancestor version for ${named} is missing.`),
      readFile(oursPath, `Git's first branch version for ${named} is missing.`),
      readFile(
        theirsPath,
        `Git's second branch version for ${named} is missing.`,
      ),
    );
    writeFileSync(oursPath, outcome.source, "utf8");
    return {
      code: 0,
      message:
        outcome.changes.length > 0
          ? outcome.changes.join("\n")
          : `Merged; neither side changed ${named}.`,
    };
  } catch (error) {
    if (!(error instanceof BacklogError)) {
      throw error;
    }
    const message = gitFacingRefusal(error, named);
    writeFileSync(
      oursPath,
      conflictMarkers(oursPath, theirsPath, message),
      "utf8",
    );
    return { code: 1, message };
  }
}

const [, , ancestorPath, oursPath, theirsPath, path] = process.argv;
const result = runDriver(ancestorPath, oursPath, theirsPath, path);
if (result.code === 0) {
  console.log(result.message);
} else {
  console.error(result.message);
}
process.exit(result.code);
