// A job's test report as CI proof fragments, one per proof entry it ran (an
// entry is a configured Intent location path). Node built-ins only, like the
// rest of the collector (ci-proof-summary.mjs).
import { fragment, zeroCounts } from "./ci-proof-contract.mjs";

// One Playwright test passes when every execution was expected, fails when
// any was unexpected or flaky, and is otherwise other (skipped or unfinished).
function playwrightTestOutcome(executions) {
  const outcomes = executions.map((execution) => execution.outcome);
  if (
    outcomes.some((outcome) => outcome === "unexpected" || outcome === "flaky")
  ) {
    return "failed";
  }
  if (
    outcomes.length > 0 &&
    outcomes.every((outcome) => outcome === "expected")
  ) {
    return "passed";
  }
  return "other";
}

// A dashboard proof record (OPEN_DOUGH_DASHBOARD_PROOF_RECORD) as the fragment
// of its one entry; counted unit: tests. A record that did not complete a run
// gives no fragment, so its part stays missing.
export function fragmentFromPlaywrightRecord(record, identity) {
  if (record?.schemaVersion !== 1) {
    throw new Error("the Playwright proof record is not schemaVersion 1");
  }
  if (record.phase !== "completed" || !Array.isArray(record.tests)) {
    throw new Error(
      `the Playwright proof record did not complete a run (phase ${record.phase})`,
    );
  }
  if (identity.entries.length !== 1) {
    throw new Error("a Playwright proof record proves exactly one --entry");
  }
  const counts = zeroCounts();
  for (const test of record.tests) {
    counts[playwrightTestOutcome(test.executions ?? [])] += 1;
  }
  return [fragment(identity, identity.entries[0], "tests", counts)];
}

function owningEntry(entries, check) {
  const owners = entries.filter(
    (entry) => check === entry || check.startsWith(`${entry}/`),
  );
  return owners.sort((left, right) => right.length - left.length)[0];
}

// An outcomes file, one `<status><TAB><check path>` line per check, as one
// fragment per entry; counted unit: checks. Each check counts toward the entry
// whose path contains it: status `passed` and `failed` as such, any other as
// other. A check in no entry proves no entry, so it counts toward none.
export function fragmentsFromOutcomes(text, identity) {
  const counts = new Map(
    identity.entries.map((entry) => [entry, zeroCounts()]),
  );
  for (const [index, line] of text.split("\n").entries()) {
    if (line === "") continue;
    const tab = line.indexOf("\t");
    if (tab <= 0 || tab === line.length - 1) {
      throw new Error(`outcomes line ${index + 1} is not <status><TAB><check>`);
    }
    const status = line.slice(0, tab);
    const check = line.slice(tab + 1);
    const entry = owningEntry(identity.entries, check);
    if (entry === undefined) continue;
    const field = status === "passed" || status === "failed" ? status : "other";
    counts.get(entry)[field] += 1;
  }
  return identity.entries.map((entry) =>
    fragment(identity, entry, "checks", counts.get(entry)),
  );
}
