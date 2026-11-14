// A job's test report as CI proof fragments, one per proof entry it ran (an
// entry is a configured Intent location path), counted in the unit the
// report establishes or its publisher declares (ci-proof-evidence.mjs,
// "Units"). Node built-ins only, like the rest of the collector
// (ci-proof-summary.mjs).
import {
  DECLARED_UNITS,
  LEGACY_UNIT,
  fragment,
  zeroCounts,
} from "./ci-proof-contract.mjs";
import {
  breakdownExtras,
  emptyFigures,
  isPathIn,
  recordResult,
  unassigningUnusable,
} from "./ci-proof-figures.mjs";

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

// Sum of every finished attempt's durationMs across the record's tests
// (retries included). Absent when no attempt recorded a duration.
function playwrightDurationMs(tests) {
  let durationMs;
  for (const test of tests) {
    for (const attempt of test.attempts ?? []) {
      if (typeof attempt.durationMs !== "number") continue;
      durationMs = (durationMs ?? 0) + attempt.durationMs;
    }
  }
  return durationMs;
}

// The repository path of a Playwright test's file: its absolute
// `location.file` relative to the record's own run directory (`run.cwd`,
// where the run was started: the repository root for `npm run`), when it is
// a file inside one of ENTRIES. Undefined otherwise, so it is never a
// machine path.
function repositoryPathOf(record, test, entries) {
  const file = test.location?.file;
  const cwd = record.run?.cwd;
  if (typeof file !== "string" || typeof cwd !== "string" || cwd === "") {
    return undefined;
  }
  const root = cwd.endsWith("/") ? cwd : `${cwd}/`;
  if (!file.startsWith(root)) return undefined;
  const path = file.slice(root.length);
  return entries.some((entry) => isPathIn(path, entry)) ? path : undefined;
}

// A Playwright proof record (`--playwright-record`) as one fragment
// per entry; each test it lists is one test case outcome, so the unit is
// `case`. Multiple entries partition tests by their
// most specific owning repository path, like an outcomes report. A single
// entry keeps whole-record totals, including results without usable paths.
// A record that did not complete a run gives no fragment, so its part stays
// missing. Optional durationMs sums every finished attempt of that entry's
// tests. With BY_FILE, a test without a usable path is unassigned, and a test
// none of whose attempts recorded a duration is untimed.
export function fragmentFromPlaywrightRecord(
  record,
  identity,
  { byFile = false } = {},
) {
  if (record?.schemaVersion !== 1) {
    throw new Error("the Playwright proof record is not schemaVersion 1");
  }
  if (record.phase !== "completed" || !Array.isArray(record.tests)) {
    throw new Error(
      `the Playwright proof record did not complete a run (phase ${record.phase})`,
    );
  }
  if (identity.entries.length === 0) {
    throw new Error("a Playwright proof record needs at least one --entry");
  }
  const testsByEntry = new Map(identity.entries.map((entry) => [entry, []]));
  for (const test of record.tests) {
    const path = repositoryPathOf(record, test, identity.entries);
    const entry =
      identity.entries.length === 1
        ? identity.entries[0]
        : path === undefined
          ? undefined
          : owningEntry(identity.entries, path);
    if (entry !== undefined) testsByEntry.get(entry).push(test);
  }
  return identity.entries.map((entry) => {
    const tests = testsByEntry.get(entry);
    const counts = zeroCounts();
    const breakdown = new Map();
    for (const test of tests) {
      const outcome = playwrightTestOutcome(test.executions ?? []);
      counts[outcome] += 1;
      if (byFile) {
        recordResult(
          breakdown,
          repositoryPathOf(record, test, [entry]),
          outcome,
          playwrightDurationMs([test]),
        );
      }
    }
    const durationMs = playwrightDurationMs(tests);
    return fragment(identity, entry, { unit: "case" }, counts, {
      ...(durationMs === undefined ? {} : { durationMs }),
      ...(byFile ? breakdownExtras(breakdown) : {}),
    });
  });
}

function owningEntry(entries, check) {
  const owners = entries.filter(
    (entry) => check === entry || check.startsWith(`${entry}/`),
  );
  return owners.sort((left, right) => right.length - left.length)[0];
}

// Walks TEXT's non-empty `<left><TAB><check>` lines. KIND and LEFT name the
// format in parse errors (for example "outcomes" / "<status>").
function forEachTabLine(text, kind, left, visit) {
  for (const [index, line] of text.split("\n").entries()) {
    if (line === "") continue;
    const tab = line.indexOf("\t");
    if (tab <= 0 || tab === line.length - 1) {
      throw new Error(`${kind} line ${index + 1} is not ${left}<TAB><check>`);
    }
    visit(line.slice(0, tab), line.slice(tab + 1), index + 1);
  }
}

// Sum milliseconds and count timings by check path, one
// `<seconds><TAB><check path>` line per timed check. Paths in no entry
// contribute to none.
function checkTimes(text, entries) {
  const times = new Map();
  forEachTabLine(text, "times", "<seconds>", (secondsText, check, line) => {
    const seconds = Number(secondsText);
    if (!Number.isFinite(seconds) || seconds < 0) {
      throw new Error(
        `times line ${line} is not a non-negative number of seconds`,
      );
    }
    if (owningEntry(entries, check) === undefined) return;
    const time = times.get(check) ?? { durationMs: 0, timedChecks: 0 };
    times.set(check, {
      durationMs: time.durationMs + seconds * 1000,
      timedChecks: time.timedChecks + 1,
    });
  });
  return times;
}

// Per-entry summed milliseconds of TIMES; only entries that own at least one
// times line appear.
function durationsOf(times, entries) {
  const durations = new Map();
  for (const [check, { durationMs }] of times) {
    const entry = owningEntry(entries, check);
    durations.set(entry, (durations.get(entry) ?? 0) + durationMs);
  }
  return durations;
}

// An outcomes file, one `<status><TAB><check path>` line per check, as one
// fragment per entry. The format does not say what a line's outcome is, so
// UNIT declares it: `case` when each line is one test case's outcome, `file`
// when each is one file's aggregate outcome. Without UNIT the results are
// legacy, labelled `checks` as before units were declared, and never read as
// either. Each check counts toward the entry
// whose path contains it: status `passed` and `failed` as such, any other as
// other. A check in no entry proves no entry, so it counts toward none.
// Optional TIMES text, same labels as outcomes, sets each matching entry's
// durationMs to the sum of its checks' seconds as milliseconds. With
// BY_FILE, each fragment is also broken down by check path (a check's label
// is its repository path; one that is not a relative path inside its entry
// is unassigned); a check with no times line is untimed, and a times line
// without an outcome adds its time to its path alone.
export function fragmentsFromOutcomes(
  text,
  identity,
  timesText,
  { byFile = false, unit } = {},
) {
  if (unit !== undefined && !DECLARED_UNITS.includes(unit)) {
    throw new Error(
      `the outcomes unit ${unit} is not ${DECLARED_UNITS.join(" or ")}`,
    );
  }
  const counted =
    unit === undefined ? { unit: LEGACY_UNIT, label: "checks" } : { unit };
  const counts = new Map(
    identity.entries.map((entry) => [entry, zeroCounts()]),
  );
  const times =
    timesText === undefined
      ? new Map()
      : checkTimes(timesText, identity.entries);
  const breakdowns = new Map(
    identity.entries.map((entry) => [entry, new Map()]),
  );
  forEachTabLine(text, "outcomes", "<status>", (status, check) => {
    const entry = owningEntry(identity.entries, check);
    if (entry === undefined) return;
    const field = status === "passed" || status === "failed" ? status : "other";
    counts.get(entry)[field] += 1;
    if (byFile) recordResult(breakdowns.get(entry), check, field, undefined);
  });
  if (byFile) {
    for (const [check, { durationMs, timedChecks }] of times) {
      const breakdown = breakdowns.get(owningEntry(identity.entries, check));
      const figures = breakdown.get(check) ?? emptyFigures();
      breakdown.set(check, {
        ...figures,
        untimed: Math.max(0, figures.untimed - timedChecks),
        durationMs,
      });
    }
    for (const [entry, breakdown] of breakdowns)
      breakdowns.set(entry, unassigningUnusable(breakdown, entry));
  }
  const durations =
    timesText === undefined ? null : durationsOf(times, identity.entries);
  return identity.entries.map((entry) => {
    const extras = {
      ...(durations !== null && durations.has(entry)
        ? { durationMs: durations.get(entry) }
        : {}),
      ...(byFile ? breakdownExtras(breakdowns.get(entry)) : {}),
    };
    return fragment(identity, entry, counted, counts.get(entry), extras);
  });
}
