import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { test } from "node:test";
import { cucumberApi, fixture, simple, succeeded } from "./cucumber-runner.js";

test("ordinary focused runs retain source identities, declaring boundaries and tested bytes", (t) => {
  const f = fixture(t, String.raw`Feature: proof
  Rule: examples
    Scenario: ordinary
      Given passed
    Scenario Outline: source <value>
      Given passed
      Examples:
        | value    | slash       | newline       |
        | one\|two | back\\slash | first\nsecond |
`, `BeforeAll(function () {
  writeFileSync('features/recorded/test.feature', 'changed after Cucumber loaded it');
  execFileSync('git', ['commit', '--allow-empty', '-qm', 'commit changed during run']);
});`);
  f.put("website/frontend/features/outside.feature", simple);
  const commit = f.git("rev-parse", "HEAD");
  const blob = f.git("hash-object", "website/frontend/features/recorded/test.feature");
  succeeded(f.run(["--name", "ordinary|source|completed"]));
  assert.notEqual(f.git("rev-parse", "HEAD"), commit);
  assert.notEqual(f.git("hash-object", "website/frontend/features/recorded/test.feature"), blob);
  const [observations] = f.records();
  assert.equal(observations.length, 2);
  assert.deepEqual(observations.map(({ scenario }) => scenario), ["ordinary", "source <value>"]);
  assert.deepEqual(observations[1].example, ["one|two", "back\\slash", "first\nsecond"]);
  assert.equal(Object.hasOwn(observations[0], "example"), false);
  for (const observation of observations) {
    assert.deepEqual(Object.keys(observation).sort(), ["attempts", "commit", "durationMs", "feature", "featureBlob", "finishedAt", "outcome", "scenario", ...(observation.example ? ["example"] : [])].sort());
    assert.equal(observation.commit, commit);
    assert.equal(observation.featureBlob, blob);
    assert.equal(observation.feature, "website/frontend/features/recorded/test.feature");
    assert.equal(observation.attempts, 1);
    assert.equal(observation.outcome, "passed");
    assert.ok(observation.durationMs >= 0);
    assert.equal(new Date(observation.finishedAt).toISOString(), observation.finishedAt);
  }
  assert.match(readFileSync(join(f.cwd, "report.html"), "utf8"), /source/);
});

test("results include hook failures and executed skipped, pending, undefined and ambiguous scenarios", (t) => {
  const names = ["passed", "failed", "pending", "skipped", "undefined", "ambiguous", "before failure", "after failure"];
  const f = fixture(t, `Feature: outcomes\n${names.map((name) => `  Scenario: ${name}\n    Given ${name.includes("failure") ? "passed" : name}\n`).join("")}`,
    `Before({ name: 'failure before', tags: '@unused' }, function () {});
Before(function ({ pickle }) { if (pickle.name === 'before failure') throw new Error('before broke'); });
After(function ({ pickle }) { if (pickle.name === 'after failure') throw new Error('after broke'); });`);
  const result = f.run();
  assert.equal(result.status, 1);
  assert.match(result.stdout, /intentional failure/);
  assert.match(result.stdout, /before broke/);
  assert.match(result.stdout, /after broke/);
  const [observations] = f.records();
  assert.deepEqual(observations.map(({ scenario, outcome }) => [scenario, outcome]), names.map((name) => [name, name.includes("failure") ? "failed" : name]));
});

test("Git attributes and CRLF conversion apply to the source bytes Cucumber loaded", (t) => {
  const f = fixture(t, simple.replaceAll("\n", "\r\n"));
  f.git("config", "core.autocrlf", "true");
  f.put(".gitattributes", "*.feature text eol=lf\n");
  const expected = f.git("hash-object", "website/frontend/features/recorded/test.feature");
  succeeded(f.run());
  assert.equal(f.records()[0][0].featureBlob, expected);
});

test("an explicitly skipped step ran even without a passing hook", (t) => {
  const f = fixture(t, "Feature: skipped\n  Scenario: executed skip\n    Given skipped\n");
  assert.equal(f.run().status, 0);
  assert.equal(f.records()[0][0].outcome, "skipped");
  assert.ok(f.records()[0][0].durationMs > 0);
});

test("retries record only the final attempt and accumulate its measured durations", (t) => {
  const f = fixture(t, "Feature: retry\n  Scenario: retry\n    Given flaky\n", `
let calls = 0;
Given('flaky', async function () {
  if (existsSync('proof')) throw new Error('intermediate retry was recorded');
  await new Promise((resolve) => setTimeout(resolve, 10));
  if (++calls === 1) throw new Error('retry me');
});`);
  succeeded(f.run(["--retry", "1"]));
  const [[observation]] = f.records();
  const events = f.messages();
  assert.equal(events.filter(({ testCaseFinished }) => testCaseFinished?.willBeRetried).length, 1);
  const durationMs = events.flatMap(({ testStepFinished }) => testStepFinished ? [testStepFinished.testStepResult.duration] : [])
    .reduce((total, { seconds, nanos }) => total + Number(seconds) * 1000 + nanos / 1e6, 0);
  assert.equal(observation.durationMs, durationMs);
  assert.equal(observation.attempts, 2);
  assert.equal(observation.outcome, "passed");
});

test("dry-run and fail-fast do not observe scenarios whose steps and hooks did not run", (t) => {
  const f = fixture(t, "Feature: stop\n  Scenario: first\n    Given failed\n  Scenario: unexecuted\n    Given passed\n", "Before(function () {}); After(function () {});");
  succeeded(f.run(["--dry-run"]));
  assert.equal(readdirSync(f.cwd).includes("proof"), false);
  assert.equal(f.run(["--fail-fast"]).status, 1);
  assert.deepEqual(f.records()[0].map(({ scenario }) => scenario), ["first"]);
});

test("CLI, configured and programmatic dry-runs with undefined steps record no observations", (t) => {
  const f = fixture(t, "Feature: undefined\n  Scenario: unknown\n    Given undefined\n");
  assert.equal(f.run(["--dry-run"]).status, 0);
  assert.equal(f.run(["-d"]).status, 0);
  f.configure(true, { dryRun: true });
  assert.equal(f.run().status, 0);
  const programmatic = spawnSync(process.execPath, ["--input-type=module", "-e", `
    import { loadConfiguration, runCucumber } from ${JSON.stringify(cucumberApi)};
    const { runConfiguration } = await loadConfiguration();
    runConfiguration.runtime.dryRun = true;
    const result = await runCucumber(runConfiguration);
    process.exitCode = result.success ? 0 : 1;
  `], { cwd: f.cwd, encoding: "utf8", timeout: 30000 });
  succeeded(programmatic);
  assert.equal(readdirSync(f.cwd).includes("proof"), false);
});

test("a later execution of the same scenario identity replaces its earlier retry totals", (t) => {
  const f = fixture(t, "Feature: duplicate\n  Scenario: same identity\n    Given flaky\n  Scenario: same identity\n    Given passed\n", `
let calls = 0;
Given('flaky', function () { if (++calls === 1) throw new Error('retry me'); });`);
  succeeded(f.run(["--retry", "1"]));
  const [observations] = f.records();
  assert.equal(observations.length, 1);
  assert.equal(observations[0].attempts, 1);
  assert.equal(observations[0].outcome, "passed");
});

test("proof failures and absence of Git remain silent and preserve reports, diagnostics and status", (t) => {
  const f = fixture(t, "Feature: fail\n  Scenario: fail\n    Given failed\n");
  f.configure(false);
  const baseline = f.run();
  f.configure();
  writeFileSync(f.proof, "not a directory");
  const blocked = f.run();
  const withoutTimes = (output) => output.replace(/\d+m [\d.]+s/g, "TIME");
  assert.equal(blocked.status, baseline.status);
  assert.equal(blocked.stderr, baseline.stderr);
  assert.equal(withoutTimes(blocked.stdout), withoutTimes(baseline.stdout));
  assert.match(readFileSync(join(f.cwd, "report.html"), "utf8"), /intentional failure/);
  rmSync(join(f.root, ".git"), { recursive: true });
  const outsideGit = f.run();
  assert.equal(outsideGit.status, baseline.status);
  assert.equal(outsideGit.stderr, baseline.stderr);
  assert.equal(withoutTimes(outsideGit.stdout), withoutTimes(baseline.stdout));
});

test("interruption keeps only completed scenarios in an already readable run file", async (t) => {
  const f = fixture(t, "Feature: interrupt\n  Scenario: completed\n    Given passed\n  Scenario: in flight\n    Given wait for termination\n", `
Given('wait for termination', async function () {
  writeFileSync('ready', 'ready');
  await new Promise(() => {});
});`);
  const { child, done } = f.start();
  await waitFor(() => {
    try { return readFileSync(join(f.cwd, "ready"), "utf8") === "ready"; } catch { return false; }
  });
  assert.deepEqual(f.records()[0].map(({ scenario }) => scenario), ["completed"]);
  child.kill("SIGKILL");
  assert.equal((await done).signal, "SIGKILL");
  assert.deepEqual(f.records()[0].map(({ scenario }) => scenario), ["completed"]);
  assert.deepEqual(readdirSync(f.proof), f.files());
});

test("parallel workers and concurrent runs use separate atomic files", async (t) => {
  const f = fixture(t, `Feature: parallel\n${Array.from({ length: 8 }, (_, i) => `  Scenario: case ${i}\n    Given passed\n`).join("")}`);
  const first = f.start(["--parallel", "2", "--format", "message:first.ndjson"]);
  const second = f.start(["--parallel", "2", "--format", "message:second.ndjson"]);
  let complete = false;
  const finishing = Promise.all([first.done, second.done]).then((results) => { complete = true; return results; });
  while (!complete) {
    try { f.records(); } catch (error) { if (error.code !== "ENOENT") throw error; }
    await new Promise((resolve) => setTimeout(resolve, 2));
  }
  for (const result of await finishing) succeeded(result);
  const runs = f.records();
  assert.equal(runs.length, 2);
  for (const observations of runs) {
    assert.equal(observations.length, 8);
    assert.equal(new Set(observations.map(({ scenario }) => scenario)).size, 8);
  }
  assert.deepEqual(readdirSync(f.proof).sort(), f.files().sort());
});

async function waitFor(observed) {
  const deadline = Date.now() + 15000;
  while (!observed()) {
    assert.ok(Date.now() < deadline, "runner did not reach the synchronization point");
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
}
