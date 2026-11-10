// Fixture: collector fragment option surface from before `--unit` existed.
// parseArgs refuses unknown flags with `Unknown option '--unit'`, matching
// the pre-unit publisher so shallow CI checkouts need no git history depth.
import { parseArgs } from "node:util";

const FRAGMENT_STRINGS = [
  "entry",
  "part",
  "of",
  "revision",
  "out",
  "attempt",
  "attempt-id",
  "playwright-record",
  "outcomes",
  "times",
];

function options(argv) {
  parseArgs({
    args: argv,
    options: Object.fromEntries([
      ...FRAGMENT_STRINGS.map((name) => [
        name,
        { type: "string", multiple: name === "entry" },
      ]),
      ["by-file", { type: "boolean" }],
    ]),
  });
}

const [command, ...argv] = process.argv.slice(2);
try {
  if (command !== "fragment") {
    throw new Error("usage: ci-proof-summary.mjs fragment [options]");
  }
  options(argv);
  throw new Error("expected unknown --unit");
} catch (error) {
  process.stderr.write(`ci-proof-summary: ${error.message}\n`);
  process.exitCode = 1;
}
