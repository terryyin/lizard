#!/usr/bin/env node
// Lists every file in a checkout that names a changed file or a supplied
// effect term, so the proof a change owes its existing consumers is chosen
// from the whole checkout rather than from the directories already in view.
import { execFileSync } from "node:child_process";
import { existsSync, lstatSync, realpathSync } from "node:fs";
import {
  basename,
  dirname,
  isAbsolute,
  relative,
  resolve,
  sep,
} from "node:path";
import { parseArgs } from "node:util";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { consumerKind, renderConsumerTrace } from "./consumer-trace-record.mjs";
import {
  changedAgainst,
  derivedTerms,
  readText,
} from "./consumer-trace-terms.mjs";

const USAGE =
  "usage: consumer-trace.mjs --checkout <path> [--base <revision>] [--path <changed file>]... [--term <text>]... (at least one --base, --path or --term; --base HEAD traces the uncommitted and untracked change)";

class ConsumerTraceInputError extends Error {}

function git(root, args) {
  return execFileSync("git", ["-C", root, ...args], {
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
    stdio: ["ignore", "pipe", "pipe"],
  });
}

function checkoutRoot(checkout) {
  if (!checkout || !existsSync(checkout))
    throw new ConsumerTraceInputError(`checkout not found: ${checkout}`);
  try {
    return realpathSync(git(checkout, ["rev-parse", "--show-toplevel"]).trim());
  } catch {
    throw new ConsumerTraceInputError(`not a Git checkout: ${checkout}`);
  }
}

function canonical(path) {
  let existing = path;
  const rest = [];
  while (!existsSync(existing) && dirname(existing) !== existing) {
    rest.unshift(basename(existing));
    existing = dirname(existing);
  }
  return resolve(realpathSync(existing), ...rest);
}

function repositoryPath(root, path) {
  const absolute = canonical(isAbsolute(path) ? path : resolve(root, path));
  const inside = relative(root, absolute);
  if (!inside || inside.startsWith("..") || isAbsolute(inside))
    throw new ConsumerTraceInputError(`path outside the checkout: ${path}`);
  return inside.split(sep).join("/");
}

function revisionOf(root) {
  try {
    return git(root, ["rev-parse", "--short=12", "HEAD"]).trim();
  } catch {
    return "no commit";
  }
}

// Terms carry an origin so derived terms can sit beside the supplied ones;
// the first origin of a term is the one recorded.
function effectTerms(paths, terms, derived) {
  const result = [];
  const add = ({ term, origin, derived = false }) => {
    if (term && !result.some((item) => item.term === term))
      result.push({ term, origin, derived });
  };
  for (const path of paths) {
    add({ term: path, origin: "changed path" });
    add({ term: basename(path), origin: `basename of \`${path}\`` });
  }
  for (const term of terms) add({ term, origin: "supplied" });
  for (const term of derived) add(term);
  return result;
}

function changeAgainstBase(root, base) {
  try {
    git(root, ["rev-parse", "--verify", "--quiet", `${base}^{commit}`]);
  } catch {
    throw new ConsumerTraceInputError(`base revision not found: ${base}`);
  }
  return changedAgainst(git, root, base);
}

// A derived term found in more than 1% of the files (at least 20) names the
// change too loosely to point at its consumers (a `pid` key, a `types`
// module, a removed `node:url` import); the record keeps it with its count
// for the agent to supply as a `--term`, and it lists no file on its own.
function broadLimit(fileCount) {
  return Math.max(20, Math.ceil(fileCount / 100));
}

function searchedFiles(root, changed) {
  const listed = git(root, [
    "ls-files",
    "-z",
    "--cached",
    "--others",
    "--exclude-standard",
  ])
    .split("\0")
    .filter(Boolean);
  return [...new Set(listed)].filter((path) => {
    if (changed.has(path)) return false;
    try {
      return lstatSync(resolve(root, path)).isFile();
    } catch {
      return false;
    }
  });
}

function firstMatch(content, needles) {
  const lower = content.toLowerCase();
  const matched = needles.filter(({ needle }) => lower.includes(needle));
  if (!matched.length) return undefined;
  const lines = content.split(/\r?\n/);
  const holds = (line) =>
    matched.some(({ needle }) => line.toLowerCase().includes(needle));
  // A term spanning a line break still names the file, from its first line.
  const index = Math.max(0, lines.findIndex(holds));
  const terms = matched.map(({ term }) => term);
  return { terms, line: index + 1, text: lines[index].trim() };
}

function traceConsumers({ checkout, base, paths = [], terms = [] }) {
  if (!base && !paths.length && !terms.length)
    throw new ConsumerTraceInputError(
      "supply at least one --base, --path or --term",
    );
  const root = checkoutRoot(checkout);
  const changes = base ? changeAgainstBase(root, base) : [];
  const changed = [
    ...new Set([
      ...changes.map(({ path }) => path),
      ...paths.map((path) => repositoryPath(root, path)),
    ]),
  ].sort();
  const searched = effectTerms(changed, terms, derivedTerms(changes));
  const needles = searched.map((item) => ({
    item,
    term: item.term,
    needle: item.term.toLowerCase(),
  }));
  const files = searchedFiles(root, new Set(changed));
  const located = [];
  for (const path of files) {
    const text = readText(root, path);
    const match = text === undefined ? undefined : firstMatch(text, needles);
    if (match) located.push({ path, text, match });
  }
  const limit = broadLimit(files.length);
  for (const item of searched) {
    const count = located.filter(({ match }) =>
      match.terms.includes(item.term),
    ).length;
    if (item.derived && count > limit) item.broad = count;
  }
  const listed = needles.filter(({ item }) => !item.broad);
  const consumers = [];
  for (const { path, text, match } of located) {
    const broad = match.terms.some((term) =>
      searched.some((item) => item.broad && item.term === term),
    );
    const narrowed = broad ? firstMatch(text, listed) : match;
    if (narrowed)
      consumers.push({ path, kind: consumerKind(path), ...narrowed });
  }
  consumers.sort((left, right) => left.path.localeCompare(right.path));
  return {
    revision: revisionOf(root),
    changed,
    terms: searched,
    fileCount: files.length,
    consumers,
  };
}

const OPTIONS = {
  checkout: { type: "string" },
  base: { type: "string" },
  path: { type: "string", multiple: true },
  term: { type: "string", multiple: true },
};

function readConsumerTraceArgs(argv) {
  let parsed;
  try {
    parsed = parseArgs({ args: argv, options: OPTIONS });
  } catch (error) {
    throw new ConsumerTraceInputError(error.message);
  }
  const { checkout, base, path = [], term = [] } = parsed.values;
  if (!checkout) throw new ConsumerTraceInputError("--checkout is required");
  if ([base ?? "x", ...path, ...term].some((value) => !value.trim()))
    throw new ConsumerTraceInputError("--base, --path and --term need a value");
  return { checkout, base, paths: path, terms: term };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const record = traceConsumers(readConsumerTraceArgs(process.argv.slice(2)));
    process.stdout.write(renderConsumerTrace(record));
  } catch (error) {
    if (!(error instanceof ConsumerTraceInputError)) throw error;
    process.stderr.write(`${error.message}\n${USAGE}\n`);
    process.exitCode = 2;
  }
}
