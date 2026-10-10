import { readFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { parseArgs } from "node:util";
import { readHome } from "../../dough-product-backlog/scripts/product-backlog-home-reader.mjs";
import { splitHref } from "../../dough-product-backlog/scripts/product-backlog-identity.mjs";
import { readPlanSlices } from "../../dough-product-backlog/scripts/product-backlog-plan-reader.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import {
  normalizeStoryQuote,
  readStoryObligations,
} from "./story-obligation-reader.mjs";

export { readStoryObligations } from "./story-obligation-reader.mjs";

// Checks recorded ownership, not whether the coordinator's story judgment is right.
async function inspectStoryObligations(
  planPath,
  { slice, completion, committingSlice } = {},
) {
  const source = await readFile(planPath, "utf8");
  const record = readStoryObligations(source);
  const problems = [...record.problems];
  const result = () => ({
    result: {
      ok: problems.length === 0,
      entryCount: record.entries.length,
      ...(problems.length && { problems }),
    },
    entries: record.entries,
  });
  if (!record.entries.length) return result();
  const read = readPlanSlices(source);
  if (read.status !== "interpreted")
    problems.push({ reason: "unreadable-slices", message: read.problem });
  const slices = new Map((read.slices ?? []).map((item) => [item.index, item]));
  const isDoneOrCommitting = (index) =>
    slices.get(index)?.status === "done" || committingSlice === index;
  for (const item of read.slices ?? []) {
    if (slices.get(item.index) !== item)
      problems.push({ reason: "ambiguous-slice", slice: item.index });
  }
  if (slice !== undefined && !slices.has(slice))
    problems.push({ reason: "unknown-slice", slice });
  let story;
  if (!record.sourceHref) problems.push({ reason: "missing-story-source" });
  else {
    try {
      const storySource = await readFile(
        resolve(dirname(planPath), splitHref(record.sourceHref).path),
        "utf8",
      );
      const home = readHome(storySource, record.sourceHref);
      story = normalizeStoryQuote(
        home.document.lines
          .slice(home.region.start, home.region.end)
          .join("\n"),
      );
    } catch (error) {
      problems.push({
        reason: "unreadable-story-source",
        message: error.message,
      });
    }
  }
  for (const entry of record.entries) {
    const problem = (reason, fields = {}) =>
      problems.push({ entry: entry.id, reason, ...fields });
    if (story !== undefined) {
      for (const [field, quote] of [
        ["Story clause", entry.storyClause],
        ["Disposition", entry.disposition?.quote],
      ]) {
        if (
          quote !== undefined &&
          (!normalizeStoryQuote(quote) ||
            !story.includes(normalizeStoryQuote(quote)))
        )
          problem("quote-not-in-story", { field, quote });
      }
    }
    const disposition = entry.disposition;
    if (completion && disposition?.open)
      problem("open-obligation", { boundary: "completion" });
    if (read.status !== "interpreted") continue;
    if (entry.reportedSlice !== undefined && !slices.has(entry.reportedSlice))
      problem("dangling-reported-slice", { slice: entry.reportedSlice });
    if (!disposition) continue;
    if (
      !completion &&
      disposition.type === "return" &&
      isDoneOrCommitting(entry.reportedSlice)
    )
      problem("open-obligation", { slice: entry.reportedSlice });
    if (disposition.type === "receiving") {
      const receiver = slices.get(disposition.slice);
      if (!receiver)
        problem("dangling-receiving-slice", { slice: disposition.slice });
      else if (isDoneOrCommitting(disposition.slice)) {
        if (!completion)
          problem("open-obligation", { slice: disposition.slice });
      } else if (disposition.slice <= entry.reportedSlice)
        problem("invalid-receiving-slice", { slice: disposition.slice });
    }
    if (disposition.type === "interim") {
      for (const dependency of disposition.slices) {
        if (!slices.has(dependency))
          problem("dangling-interim-slice", { slice: dependency });
      }
      const last = read.slices.findLast((item) =>
        disposition.slices.includes(item.index),
      );
      if (!completion && last && isDoneOrCommitting(last.index))
        problem("open-obligation", { slice: last.index });
    }
    if (disposition.type === "proved" && !slices.has(disposition.slice))
      problem("dangling-proof-slice", { slice: disposition.slice });
  }
  return result();
}

export async function checkStoryObligations(
  planPath,
  { slice, completion } = {},
) {
  const { result } = await inspectStoryObligations(planPath, {
    slice,
    completion,
    committingSlice: slice,
  });
  return result;
}

export async function listStoryObligations(planPath, slice) {
  const { result, entries } = await inspectStoryObligations(planPath, {
    slice,
  });
  const receives = (entry) => {
    const disposition = entry.disposition;
    if (!disposition?.open) return false;
    if (disposition.type === "return") return entry.reportedSlice === slice;
    if (disposition.type === "receiving") return disposition.slice === slice;
    return disposition.slices.includes(slice);
  };
  return {
    ok: result.ok,
    slice,
    entries: result.ok ? entries.filter(receives) : [],
    ...(result.problems && { problems: result.problems }),
  };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  let result;
  try {
    const { values, positionals } = parseArgs({
      options: {
        plan: { type: "string" },
        slice: { type: "string" },
        completion: { type: "boolean" },
      },
      allowPositionals: true,
    });
    if (
      positionals.length !== 1 ||
      !["check", "list"].includes(positionals[0]) ||
      !values.plan ||
      (values.slice !== undefined && !/^[1-9]\d*$/.test(values.slice)) ||
      (positionals[0] === "list" && (!values.slice || values.completion))
    ) {
      throw new Error(
        "usage: story-obligations.mjs check --plan <PLAN.md> [--slice <N>] [--completion]; or list --plan <PLAN.md> --slice <N>",
      );
    }
    const slice = values.slice === undefined ? undefined : Number(values.slice);
    result =
      positionals[0] === "list"
        ? await listStoryObligations(resolve(values.plan), slice)
        : await checkStoryObligations(resolve(values.plan), {
            slice,
            completion: values.completion,
          });
  } catch (error) {
    result = {
      ok: false,
      problems: [{ reason: "invalid-input", message: error.message }],
    };
  }
  process.stdout.write(`${JSON.stringify(result)}\n`);
  if (!result.ok) process.exitCode = 1;
}
