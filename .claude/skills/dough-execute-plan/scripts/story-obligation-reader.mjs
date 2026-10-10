import { unfencedPlanLines } from "../../dough-product-backlog/scripts/product-backlog-plan-reader.mjs";

// Reads the plan's own obligation entries, leaving story judgment to its owner.
// Fenced examples cannot supply plan structure or fields.

const quoted = /^"([\s\S]+)"$/;
const number = "([1-9]\\d*)";

// Every disposition has one representation shared by validation and listing.
function readDisposition(text) {
  if (text === "return") return { type: "return", open: true };
  let match = new RegExp(`^receiving slice ${number}$`).exec(text);
  if (match) return { type: "receiving", slice: Number(match[1]), open: true };
  match = /^interim until slices ([1-9]\d*(?:,\s*[1-9]\d*)*)$/.exec(text);
  if (match)
    return {
      type: "interim",
      slices: match[1].split(/,\s*/).map(Number),
      open: true,
    };
  match = /^(excluded|owner changed) "(.+)"$/.exec(text);
  if (match) return { type: match[1], quote: match[2], open: false };
  match = /^no user cost "(.+)": (\S.*)$/.exec(text);
  if (match)
    return {
      type: "no user cost",
      quote: match[1],
      reason: match[2],
      open: false,
    };
  match = new RegExp(`^proved by slice ${number}: (\\S.*)$`).exec(text);
  if (match)
    return {
      type: "proved",
      slice: Number(match[1]),
      proof: match[2],
      open: false,
    };
  return undefined;
}

function readEntry(lines, problems) {
  const heading = /^### +(\S+)\. +(\S.*?)\s*$/.exec(lines[0]);
  const entry = { id: heading?.[1] ?? lines[0], title: heading?.[2] ?? "" };
  const problem = (reason, field) =>
    problems.push({ entry: entry.id, reason, field });
  if (!heading) problem("malformed-entry", "heading");
  const fields = new Map();
  let current;
  for (const line of lines.slice(1)) {
    if (!line.trim()) continue;
    const field = /^(Reported|Story clause|Disposition):\s*(.*)$/.exec(line);
    if (field) {
      current = field[1];
      if (fields.has(current)) problem("duplicate-field", current);
      else fields.set(current, [field[2]]);
    } else if (current) fields.get(current).push(line.trim());
    else problem("malformed-entry", "text");
  }
  const value = (field) => fields.get(field)?.join(" ").trim();
  const reported = value("Reported");
  const match = /^slice ([1-9]\d*) — "(.+)"$/.exec(reported ?? "");
  if (!match || !match[2].trim())
    problem(
      reported === undefined ? "missing-reported" : "malformed-reported",
      "Reported",
    );
  else {
    entry.reportedSlice = Number(match[1]);
    entry.reported = match[2];
  }
  const clause = value("Story clause");
  const quote = quoted.exec(clause ?? "");
  if (!quote)
    problem(
      clause === undefined ? "missing-story-clause" : "malformed-story-clause",
      "Story clause",
    );
  else entry.storyClause = quote[1];
  entry.disposition = readDisposition(value("Disposition") ?? "");
  if (!entry.disposition) problem("no-disposition", "Disposition");
  return entry;
}

export function readStoryObligations(source) {
  const rawLines = source.split(/\r?\n/);
  const unfenced = unfencedPlanLines(rawLines);
  const lines = rawLines.map((line, index) => (unfenced[index] ? line : ""));
  const starts = lines.flatMap((line, index) =>
    /^## +Story obligations\s*$/.test(line) ? [index] : [],
  );
  const problems = [];
  const entries = [];
  if (starts.length > 1)
    problems.push({ reason: "duplicate-obligations-section" });
  if (!starts.length) return { present: false, entries, problems };
  const start = starts[0];
  const next = lines.findIndex(
    (line, index) => index > start && /^## /.test(line),
  );
  const end = next === -1 ? lines.length : next;
  let pending;
  for (const line of lines.slice(start + 1, end)) {
    if (/^### /.test(line)) {
      if (pending) entries.push(readEntry(pending, problems));
      pending = [line];
    } else if (pending) pending.push(line);
    else if (line.trim())
      problems.push({ reason: "malformed-obligations-section" });
  }
  if (pending) entries.push(readEntry(pending, problems));
  const ids = new Set();
  for (const entry of entries) {
    if (ids.has(entry.id))
      problems.push({ entry: entry.id, reason: "duplicate-entry" });
    ids.add(entry.id);
  }
  const sources = lines.flatMap((line) => {
    const href = /^(?:\*\*Source:\*\*|Source:) +\[[^\]]+\]\(([^)]+)\)/.exec(
      line,
    )?.[1];
    return href ? [href] : [];
  });
  return {
    present: true,
    entries,
    problems,
    sourceHref: sources.length === 1 ? sources[0] : undefined,
  };
}

// Whitespace wrapping and Markdown emphasis do not change a verbatim quote.
// Underscores inside identifiers (such as benchmark_weight) stay meaningful.
export function normalizeStoryQuote(text) {
  let normalized = text;
  let previous;
  do {
    previous = normalized;
    normalized = normalized.replace(
      /(?<!\w)(\*{1,3}|_{1,3})(?=\S)([\s\S]*?\S)\1(?!\w)/g,
      "$2",
    );
  } while (normalized !== previous);
  return normalized.replace(/\s+/g, " ").trim();
}
