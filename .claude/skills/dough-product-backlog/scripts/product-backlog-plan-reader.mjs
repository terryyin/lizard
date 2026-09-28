// Interprets the established slices section of a plan (`## Ordered slices`,
// or the compatible `## Slices` heading) from already loaded text. CLI and
// browser share this meaning: a done status is recorded completion, not
// independent verification; Accepted evidence is separate from a prospective
// Proof recipe; unsupported layout is uninterpretable, not an empty slice list.
// A plan's `## Execution complete` record, with its required `Product advice:`
// entry, is read independently of the slices. Lines inside fenced code blocks
// are quoted examples, never the plan's own structure. No filesystem or
// Node-only imports.

import { splitSource } from "./product-backlog-source.mjs";

const slicesSection = /^## +(?:Ordered slices|Slices) *$/i;
const sliceHeading = /^### +(?<index>\d+)\. +(?<name>\S.*?)\s*$/;
const typeLine = /^Type: +(?<type>\S.*?)\s*$/;
const statusLine = /^Status: +(?<status>planned|done)\b/;
const proofLine = /^Proof: *(?<proof>.*?)\s*$/;
const acceptedLine = /^Accepted: *(?<accepted>.*?)\s*$/;
const fieldStart =
  /^(?:Type:|Status:|Proof:|Accepted:|Behavior:|Structure:|### |## )/;

const completionSection = /^## +Execution complete *$/i;
const adviceLine = /^Product advice: *(?<advice>.*?)\s*$/;
const fenceOpener = /^ {0,3}(?<marker>`{3,}(?=[^`]*$)|~{3,})/;
const fenceCloser = /^ {0,3}(?<marker>`{3,}|~{3,})\s*$/;

// Whether each line is outside fenced code blocks, so a quoted example of a
// heading or entry is never read as the plan's own structure. As in
// CommonMark, a backtick fence opens only when no backtick follows its run
// (so a line starting with an inline code span stays prose), and a fence
// closes only at a bare run of the opener's character at least as long as the
// opener.
function unfencedLines(lines) {
  const unfenced = [];
  let fence;
  for (const line of lines) {
    if (fence !== undefined) {
      const closed = fenceCloser.exec(line)?.groups.marker;
      if (closed?.[0] === fence[0] && closed.length >= fence.length) {
        fence = undefined;
      }
      unfenced.push(false);
      continue;
    }
    fence = fenceOpener.exec(line)?.groups.marker;
    unfenced.push(fence === undefined);
  }
  return unfenced;
}

// The `## Execution complete` record: absent when the plan has no such
// section, the verbatim advice (continuing until the next `## ` heading) when
// it has a non-empty `Product advice:` entry, and a problem otherwise.
function readCompletion(lines, unfenced) {
  const start = lines.findIndex(
    (line, index) => unfenced[index] && completionSection.test(line),
  );
  if (start === -1) {
    return undefined;
  }
  const next = lines.findIndex(
    (line, index) => index > start && unfenced[index] && /^## /.test(line),
  );
  const end = next === -1 ? lines.length : next;
  const entryIndex = lines.findIndex(
    (line, index) =>
      index > start && index < end && unfenced[index] && adviceLine.test(line),
  );
  if (entryIndex !== -1) {
    const advice = [
      adviceLine.exec(lines[entryIndex]).groups.advice,
      ...lines.slice(entryIndex + 1, end).map((line) => line.trimEnd()),
    ]
      .join("\n")
      .trim();
    if (advice !== "") {
      return { advice };
    }
  }
  return {
    problem:
      "This plan’s “## Execution complete” record has no readable “Product advice:” entry.",
  };
}

function sectionBounds(lines, unfenced) {
  const start = lines.findIndex(
    (line, index) => unfenced[index] && slicesSection.test(line),
  );
  if (start === -1) {
    return undefined;
  }
  let end = lines.length;
  for (let index = start + 1; index < lines.length; index += 1) {
    if (
      unfenced[index] &&
      /^## /.test(lines[index]) &&
      !slicesSection.test(lines[index])
    ) {
      end = index;
      break;
    }
  }
  return { start, end };
}

function continuation(lines, unfenced, from, until) {
  const parts = [];
  for (let index = from; index < until; index += 1) {
    const line = lines[index];
    if (line.trim() === "") {
      if (parts.length === 0) {
        continue;
      }
      break;
    }
    if (unfenced[index] && fieldStart.test(line)) {
      break;
    }
    parts.push(line.trimEnd());
  }
  return parts.join("\n").trim();
}

function readSlice(lines, unfenced, headingIndex, until) {
  const heading = sliceHeading.exec(lines[headingIndex]);
  if (!heading) {
    return { ok: false };
  }
  let type;
  let status;
  let proof;
  let accepted;
  let sawUnsupportedField = false;

  for (let index = headingIndex + 1; index < until; index += 1) {
    if (!unfenced[index]) {
      continue;
    }
    const line = lines[index];
    const typed = typeLine.exec(line);
    if (typed) {
      type = typed.groups.type.trim();
      continue;
    }
    const stated = statusLine.exec(line);
    if (stated) {
      status = stated.groups.status;
      continue;
    }
    const prospective = proofLine.exec(line);
    if (prospective) {
      const first = prospective.groups.proof.trim();
      const rest = continuation(lines, unfenced, index + 1, until);
      proof = [first, rest].filter((part) => part !== "").join("\n");
      continue;
    }
    const evidence = acceptedLine.exec(line);
    if (evidence) {
      const first = evidence.groups.accepted.trim();
      const rest = continuation(lines, unfenced, index + 1, until);
      accepted = [first, rest].filter((part) => part !== "").join("\n");
      continue;
    }
    if (/^Status:/.test(line)) {
      sawUnsupportedField = true;
    }
  }

  if (type === undefined || status === undefined || sawUnsupportedField) {
    return { ok: false };
  }

  return {
    ok: true,
    slice: {
      index: Number(heading.groups.index),
      name: heading.groups.name.trim(),
      type,
      status,
      ...(proof !== undefined && proof !== "" && { proof }),
      ...(accepted !== undefined && accepted !== "" && { accepted }),
    },
  };
}

// Reads ordered slices from plan Markdown under `## Ordered slices` or
// `## Slices`. Returns interpreted slices, an empty interpretable list, or
// uninterpretable when the established section layout is missing or malformed.
// Either answer carries `completion` when the plan has an `## Execution
// complete` record: `{ advice }`, or `{ problem }` when its advice is missing.
export function readPlanSlices(source) {
  if (typeof source !== "string") {
    return {
      status: "uninterpretable",
      problem: "Plan source must be Markdown text.",
    };
  }
  const { lines } = splitSource(source);
  const unfenced = unfencedLines(lines);
  const completion = readCompletion(lines, unfenced);
  const withCompletion = (answer) =>
    completion === undefined ? answer : { ...answer, completion };
  const bounds = sectionBounds(lines, unfenced);
  if (bounds === undefined) {
    return withCompletion({
      status: "uninterpretable",
      problem:
        "This plan has no established “## Ordered slices” or “## Slices” section, so its slice progress cannot be interpreted.",
    });
  }

  const headingIndexes = [];
  for (let index = bounds.start + 1; index < bounds.end; index += 1) {
    if (!unfenced[index]) {
      continue;
    }
    if (sliceHeading.test(lines[index])) {
      headingIndexes.push(index);
    } else if (/^### /.test(lines[index])) {
      return withCompletion({
        status: "uninterpretable",
        problem:
          "An ordered-slices heading is not in the established “### N. name” form.",
      });
    }
  }

  const slices = [];
  for (let i = 0; i < headingIndexes.length; i += 1) {
    const headingIndex = headingIndexes[i];
    const until =
      i + 1 < headingIndexes.length ? headingIndexes[i + 1] : bounds.end;
    const read = readSlice(lines, unfenced, headingIndex, until);
    if (!read.ok) {
      return withCompletion({
        status: "uninterpretable",
        problem:
          "An ordered slice is missing a supported Type or Status: planned|done, so the plan layout cannot be interpreted.",
      });
    }
    slices.push(read.slice);
  }

  return withCompletion({ status: "interpreted", slices });
}
