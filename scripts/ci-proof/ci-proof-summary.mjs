// CI proof summary: the collector's command line, importing only Node and its
// sibling ci-proof-*.mjs modules, that CI jobs run in two steps.
//
//   fragment  converts one job's test report into one fragment per proof
//             entry it ran (ci-proof-fragments.mjs).
//   summarize collects the current attempt's fragments against the
//             declared entries and parts (`--expect <entry>=<parts>`) into
//             the attempt's summary (ci-proof-collect.mjs).
//
// Both name the attempt: a GitHub Actions run attempt by `--attempt` (and
// `summarize` by `--run-id`), or a CI adapter's attempt by `--attempt-id`.
//
// The versioned schemas are in ci-proof-contract.mjs; this module re-exports
// the collector's whole API.
import {
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  realpathSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { parseArgs } from "node:util";
import { describeSummary, summarize } from "./ci-proof-collect.mjs";
import {
  attemptProblems,
  entrySlug,
  fragmentProblems,
  isPositive,
} from "./ci-proof-contract.mjs";
import {
  fragmentFromPlaywrightRecord,
  fragmentsFromOutcomes,
} from "./ci-proof-fragments.mjs";

export * from "./ci-proof-collect.mjs";
export * from "./ci-proof-contract.mjs";
export * from "./ci-proof-fragments.mjs";

function jsonFiles(directory) {
  return readdirSync(directory, { recursive: true, withFileTypes: true })
    .filter((item) => item.isFile() && item.name.endsWith(".json"))
    .map((item) => join(item.parentPath, item.name))
    .sort();
}

// Every JSON file under DIRECTORY, which may be absent when no job uploaded.
function readFragments(directory) {
  if (!existsSync(directory)) return [];
  return jsonFiles(directory).map((source) => {
    try {
      return { source, value: JSON.parse(readFileSync(source, "utf8")) };
    } catch {
      return { source, value: undefined };
    }
  });
}

function positive(name, text) {
  const value = Number(text);
  if (!/^[0-9]+$/.test(text ?? "") || !isPositive(value)) {
    throw new Error(`--${name} ${text} is not a positive integer`);
  }
  return value;
}

// A command's string options: every REQUIRED one must be given, and each of
// MULTIPLE may be given more than once.
function options(argv, { required, optional = [], multiple = [] }) {
  const names = [...required, ...optional];
  const { values } = parseArgs({
    args: argv,
    options: Object.fromEntries(
      names.map((name) => [
        name,
        { type: "string", multiple: multiple.includes(name) },
      ]),
    ),
  });
  for (const name of required) {
    if (values[name] === undefined || values[name] === "") {
      throw new Error(`--${name} is required`);
    }
  }
  return values;
}

// The attempt that VALUES name for a COMMAND: `--attempt-id`, or the
// integer options GITHUB maps to their identity fields.
function attemptIdentity(command, values, github) {
  const identity = {};
  for (const [name, field] of Object.entries(github)) {
    if (values[name] !== undefined) {
      identity[field] = positive(name, values[name]);
    }
  }
  if (values["attempt-id"] !== undefined) {
    identity.attemptId = values["attempt-id"];
  }
  const problems = attemptProblems(identity, Object.values(github));
  if (problems.length > 0) {
    throw new Error(`${command} ${problems.join("; ")}`);
  }
  return identity;
}

function parseExpect(declaration) {
  const equals = declaration.lastIndexOf("=");
  const entry = declaration.slice(0, equals);
  if (equals <= 0) {
    throw new Error(`--expect ${declaration} is not <entry>=<parts>`);
  }
  return { entry, parts: positive("expect", declaration.slice(equals + 1)) };
}

function runFragment(argv) {
  const values = options(argv, {
    required: ["entry", "part", "of", "revision", "out"],
    optional: ["attempt", "attempt-id", "playwright-record", "outcomes"],
    multiple: ["entry"],
  });
  const identity = {
    entries: values.entry,
    part: positive("part", values.part),
    of: positive("of", values.of),
    ...attemptIdentity("fragment", values, { attempt: "attempt" }),
    revision: values.revision,
  };
  let fragments;
  if (values["playwright-record"] !== undefined) {
    const record = JSON.parse(
      readFileSync(values["playwright-record"], "utf8"),
    );
    fragments = fragmentFromPlaywrightRecord(record, identity);
  } else if (values.outcomes !== undefined) {
    const text = readFileSync(values.outcomes, "utf8");
    fragments = fragmentsFromOutcomes(text, identity);
  } else {
    throw new Error("--playwright-record or --outcomes is required");
  }
  mkdirSync(values.out, { recursive: true });
  for (const value of fragments) {
    const problems = fragmentProblems(value);
    if (problems.length > 0) throw new Error(problems.join("; "));
    const file = join(
      values.out,
      `${entrySlug(value.entry)}-${value.part}.json`,
    );
    writeFileSync(file, `${JSON.stringify(value)}\n`);
  }
}

function runSummarize(argv) {
  const values = options(argv, {
    required: ["fragments", "expect", "repository", "revision", "out"],
    optional: ["run-id", "attempt", "attempt-id"],
    multiple: ["expect"],
  });
  const { summary, notes } = summarize({
    fragments: readFragments(values.fragments),
    expect: values.expect.map(parseExpect),
    run: {
      repository: values.repository,
      ...attemptIdentity("summarize", values, {
        "run-id": "runId",
        attempt: "attempt",
      }),
      revision: values.revision,
    },
  });
  writeFileSync(values.out, `${JSON.stringify(summary)}\n`);
  for (const note of notes) process.stderr.write(`${note}\n`);
  for (const line of describeSummary(summary))
    process.stdout.write(`${line}\n`);
}

// As dough-execute-plan's isDirectCliEntry, repeated so the copied collector
// needs no installed skill: a symlinked spelling of argv still runs the CLI.
function isDirectEntry() {
  if (!process.argv[1]) return false;
  if (import.meta.url === pathToFileURL(process.argv[1]).href) return true;
  try {
    return (
      realpathSync(fileURLToPath(import.meta.url)) ===
      realpathSync(process.argv[1])
    );
  } catch {
    return false;
  }
}

if (isDirectEntry()) {
  const [command, ...argv] = process.argv.slice(2);
  try {
    if (command === "fragment") runFragment(argv);
    else if (command === "summarize") runSummarize(argv);
    else
      throw new Error(
        "usage: ci-proof-summary.mjs fragment|summarize [options]",
      );
  } catch (error) {
    process.stderr.write(`ci-proof-summary: ${error.message}\n`);
    process.exitCode = 1;
  }
}
