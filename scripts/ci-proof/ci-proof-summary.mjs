// CI proof summary: the collector's command line, importing only Node and its
// sibling ci-proof-*.mjs modules, that CI jobs run in two steps, each naming
// a GitHub Actions run attempt or a CI adapter's attempt.
//
//   fragment  converts one job's test report into one fragment per proof
//             entry it ran, in the report's unit (ci-proof-fragments.mjs),
//             and with `--by-file` breaks each down by test file
//             (ci-proof-figures.mjs, "File breakdown").
//   summarize collects the current attempt's fragments against the
//             declared entries, parts and units into the attempt's summary
//             (ci-proof-collect.mjs).
//
// USAGE below states both steps' options and the unit of each report and
// declaration. Their options are read by ci-proof-options.mjs. The versioned
// schemas are in ci-proof-contract.mjs; this module re-exports the
// collector's whole API.
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
import { describeSummary, summarize } from "./ci-proof-collect.mjs";
import {
  SUMMARY_READ_LIMIT_BYTES,
  entrySlug,
  fragmentProblems,
} from "./ci-proof-contract.mjs";
import {
  fragmentFromPlaywrightRecord,
  fragmentsFromOutcomes,
} from "./ci-proof-fragments.mjs";
import {
  attemptIdentity,
  declaredUnit,
  options,
  parseExpect,
  positive,
} from "./ci-proof-options.mjs";

export * from "./ci-proof-collect.mjs";
export * from "./ci-proof-contract.mjs";
export * from "./ci-proof-fragments.mjs";

// What the command line says when given no command it knows.
const USAGE = `usage: ci-proof-summary.mjs fragment
         (--outcomes <report> --unit case|file [--times <times>] | --playwright-record <record>)
         [--by-file] --entry <entry>... --part <part> --of <parts>
         (--attempt <run attempt> | --attempt-id <attempt id>) --revision <revision> --out <dir>
       ci-proof-summary.mjs summarize --fragments <dir> --expect <entry>=<parts>:case|file...
         --repository <owner/repo> (--run-id <run id> --attempt <run attempt> | --attempt-id <attempt id>)
         --revision <revision> --out <summary file>
A Playwright record's tests are test cases. --unit file counts each outcomes line as one
test file's aggregate outcome, however many cases the file holds; --unit case as one test
case. Declare each entry once per unit it reports, with that unit's part count. Outcomes
without --unit are legacy test results; a declaration without :<unit> takes whichever
one unit its entry's fragments report.`;

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

function runFragment(argv) {
  const values = options(argv, {
    required: ["entry", "part", "of", "revision", "out"],
    optional: [
      "attempt",
      "attempt-id",
      "playwright-record",
      "outcomes",
      "times",
      "unit",
    ],
    multiple: ["entry"],
    flags: ["by-file"],
  });
  const byFile = { byFile: values["by-file"] === true };
  const identity = {
    entries: values.entry,
    part: positive("part", values.part),
    of: positive("of", values.of),
    ...attemptIdentity("fragment", values, { attempt: "attempt" }),
    revision: values.revision,
  };
  let fragments;
  if (values["playwright-record"] !== undefined) {
    for (const name of ["times", "unit"]) {
      if (values[name] !== undefined) {
        throw new Error(`--${name} applies only with --outcomes`);
      }
    }
    const record = JSON.parse(
      readFileSync(values["playwright-record"], "utf8"),
    );
    fragments = fragmentFromPlaywrightRecord(record, identity, byFile);
  } else if (values.outcomes !== undefined) {
    const text = readFileSync(values.outcomes, "utf8");
    const times =
      values.times === undefined
        ? undefined
        : readFileSync(values.times, "utf8");
    fragments = fragmentsFromOutcomes(text, identity, times, {
      ...byFile,
      ...(values.unit === undefined
        ? {}
        : { unit: declaredUnit("unit", values.unit) }),
    });
  } else {
    throw new Error("--playwright-record or --outcomes is required");
  }
  mkdirSync(values.out, { recursive: true });
  for (const value of fragments) {
    const problems = fragmentProblems(value);
    if (problems.length > 0) throw new Error(problems.join("; "));
    const file = join(
      values.out,
      `${entrySlug(value.entry)}-${value.unit}-${value.part}.json`,
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
  const text = `${JSON.stringify(summary)}\n`;
  writeFileSync(values.out, text);
  for (const note of notes) process.stderr.write(`${note}\n`);
  const bytes = Buffer.byteLength(text);
  if (bytes > SUMMARY_READ_LIMIT_BYTES) {
    process.stderr.write(
      `the summary is ${bytes} bytes, more than the ${SUMMARY_READ_LIMIT_BYTES} Intent may read as a CI proof summary; Intent will not show it\n`,
    );
  }
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
    else throw new Error(USAGE);
  } catch (error) {
    process.stderr.write(`ci-proof-summary: ${error.message}\n`);
    process.exitCode = 1;
  }
}
