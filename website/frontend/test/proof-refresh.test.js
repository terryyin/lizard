import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, utimesSync } from "node:fs";
import { join } from "node:path";
import { pathToFileURL } from "node:url";
import { test } from "node:test";
import { fixture, succeeded, waitFor } from "./cucumber-runner.js";

test("ordinary pass/fail never read proof or create proof resources, even with a forced plugin", (t) => {
  for (const force of [false, true]) for (const existing of [false, true]) for (const step of ["passed", "failed"]) {
    const f = fixture(t, `Feature: normal\n  Scenario: ordinary\n    Given ${step}\n`);
    f.configure(force ? "forced" : true);
    if (existing) {
      f.seed("historic.json", [f.entry("retained")]);
      f.put("website/frontend/proof/.proof.lock/owner", "leave existing locks alone");
    }
    f.put("website/frontend/no-proof-io.mjs", `
import fs from 'node:fs';
import { resolve, sep } from 'node:path';
import { syncBuiltinESMExports } from 'node:module';
const directory = ${JSON.stringify(f.proof)}, marker = 'unexpected-proof-io';
const write = fs.writeFileSync;
for (const name of ['readFileSync','readdirSync','mkdirSync','openSync','renameSync','rmSync','rmdirSync','statSync','utimesSync']) {
  const original = fs[name];
  fs[name] = function (path, ...args) {
    const target = typeof path === 'string' ? resolve(path) : '';
    if (target === directory || target.startsWith(directory + sep)) write(marker, name);
    return original.call(this, path, ...args);
  };
}
syncBuiltinESMExports();
`);
    const before = f.state(), head = f.git("rev-parse", "HEAD");
    const result = f.run([], { CI: "true", NODE_OPTIONS: `--import=${pathToFileURL(join(f.cwd, "no-proof-io.mjs")).href}` });
    assert.equal(result.status, step === "passed" ? 0 : 1, result.stdout);
    assert.equal(result.stderr, "");
    assert.deepEqual(f.state(), before);
    assert.equal(existsSync(join(f.cwd, "unexpected-proof-io")), false);
    assert.equal(f.git("rev-parse", "HEAD"), head);
  }
});

test("only the exact explicit option opts in, including named profiles", (t) => {
  const f = fixture(t);
  for (const value of ["", "0", "true"]) {
    succeeded(f.run(["--profile", "default"], { RECORD_CUCUMBER_PROOF: value }));
    assert.equal(f.state(), null);
  }
  succeeded(f.record(["--profile", "default"]));
  assert.deepEqual(f.files(), ["latest.json"]);
});

test("focused adoption reduces legacy observations before retaining passes, regardless of failure file order", (t) => {
  for (const reverse of [false, true]) {
    const f = fixture(t, "Feature: focus\n  Scenario: refreshed\n    Given passed\n  Scenario: unexecuted\n    Given passed\n");
    const unexecuted = f.entry("unexecuted");
    const passes = [f.entry("refreshed"), unexecuted, f.entry("later failure"), f.entry("tied failure")];
    const failures = [f.entry("later failure", { outcome: "failed", finishedAt: "2001-01-01T00:00:00.000Z" }),
      f.entry("tied failure", { outcome: "failed" })];
    f.seed("first.json", reverse ? failures : passes);
    f.seed("nested/last.json", reverse ? passes : failures);
    const head = f.git("rev-parse", "HEAD");
    succeeded(f.record(["--name", "refreshed"]));
    assert.deepEqual(f.files(), ["latest.json"]);
    const [retained] = f.records();
    assert.deepEqual(retained.map(({ scenario }) => scenario), ["refreshed", "unexecuted"]);
    assert.deepEqual(retained[1], unexecuted);
    assert.notEqual(retained[0].finishedAt, "2000-01-01T00:00:00.000Z");
    assert.equal(f.git("rev-parse", "HEAD"), head, "recording must not commit");
    succeeded(f.record(["--name", "refreshed"]));
    assert.deepEqual(f.files(), ["latest.json"], "runs must not accumulate files");
    assert.deepEqual(f.records()[0][1], unexecuted);
  }
});

test("an incoming pass cannot resurrect success invalidated by a newer legacy failure", (t) => {
  const f = fixture(t);
  f.seed("old.json", [f.entry("completed")]);
  f.seed("nested/new.json", [f.entry("completed", { outcome: "failed", finishedAt: "2050-01-01T00:00:00.000Z" })]);
  succeeded(f.record());
  assert.deepEqual(f.files(), []);
});

test("adoption preserves the reader's first pass for an unexecuted legacy pass/pass tie", (t) => {
  const f = fixture(t);
  const first = f.entry("unexecuted", { attempts: 2 });
  f.seed("a.json", [first]);
  f.seed("z.json", [f.entry("unexecuted", { attempts: 9 })]);
  succeeded(f.record());
  assert.deepEqual(f.records()[0].find(({ scenario }) => scenario === "unexecuted"), first);
});

test("the real final timestamp replaces an equal-time pass but never an equal-time legacy failure", (t) => {
  for (const outcome of ["passed", "failed"]) {
    const f = fixture(t);
    f.seed("old.json", [f.entry("completed")]);
    f.put("website/frontend/seed-at-finish.mjs", `
import { writeFileSync } from 'node:fs';
export default { type: 'plugin', coordinator({ on }) {
  on('message', ({ testCaseFinished: finished }) => {
    if (!finished || finished.willBeRetried) return;
    const { seconds, nanos } = finished.timestamp;
    const finishedAt = new Date(Number(seconds) * 1000 + nanos / 1e6).toISOString();
    const prior = ${JSON.stringify(f.entry("completed", { outcome, attempts: 7 }))};
    writeFileSync('proof/at-finish.json', JSON.stringify({version: 1, observations: [{...prior, finishedAt}]}));
  });
} };
`);
    f.configure(true, { plugin: [pathToFileURL(join(f.cwd, "seed-at-finish.mjs")).href,
      new URL("../proof-plugin.js", import.meta.url).href] });
    succeeded(f.record());
    if (outcome === "failed") assert.deepEqual(f.files(), []);
    else assert.equal(f.records()[0][0].attempts, 1, "incoming final pass must replace the prior equal-time pass");
  }
});

test("final failures clear every older observation while preserving newer and unexecuted proof", (t) => {
  const f = fixture(t, "Feature: fail\n  Scenario: completed\n    Given failed\n");
  const future = f.entry("completed", { finishedAt: "2050-01-01T00:00:00.000Z" });
  const other = f.entry("unexecuted");
  f.seed("old.json", [f.entry("completed"), other]);
  f.seed("nested/old-only.json", [f.entry("completed")]);
  f.seed("nested/newer.json", [future]);
  assert.equal(f.record().status, 1);
  assert.deepEqual(f.files(), ["latest.json"]);
  assert.deepEqual(f.records(), [[future, other]]);
  const empty = fixture(t, "Feature: fail\n  Scenario: completed\n    Given failed\n");
  empty.seed("old.json", [empty.entry("completed")]);
  empty.seed("nested/also-old.json", [empty.entry("completed")]);
  assert.equal(empty.record().status, 1);
  assert.deepEqual(empty.files(), [], "empty proof is deleted, not replaced with failure markers");
  assert.ok(empty.state().every(([name]) => name.endsWith("/")), "only empty legacy directories may remain");
});

test("intermediate retry failures leave retained success intact until the final pass", (t) => {
  const f = fixture(t, "Feature: retry\n  Scenario: completed\n    Given retry without cleanup\n", `
let calls = 0;
Given('retry without cleanup', function () {
  const retained = JSON.parse(readFileSync('proof/old.json', 'utf8')).observations;
  if (retained[0].finishedAt !== '2000-01-01T00:00:00.000Z') throw new Error('intermediate proof changed');
  if (++calls === 1) throw new Error('retry intentionally');
});`);
  f.seed("old.json", [f.entry("completed")]);
  succeeded(f.record(["--retry", "1"]));
  assert.equal(f.records()[0][0].attempts, 2);
  assert.notEqual(f.records()[0][0].finishedAt, "2000-01-01T00:00:00.000Z");
});

test("explicit pass/fail preserve normal output, diagnostics and exit meaning", (t) => {
  for (const step of ["passed", "failed"]) {
    const f = fixture(t, `Feature: parity\n  Scenario: parity\n    Given ${step}\n`);
    const normal = f.run(["--profile", "default"]), recorded = f.record(["--profile", "default"]);
    const withoutTime = (value) => value.replace(/\d+m [\d.]+s/g, "TIME");
    assert.equal(recorded.status, normal.status);
    assert.equal(recorded.stderr, normal.stderr);
    assert.equal(withoutTime(recorded.stdout), withoutTime(normal.stdout));
    assert.match(readFileSync(join(f.cwd, "report.html"), "utf8"), /parity/);
  }
});

test("unreadable proof aborts the refresh silently without consuming retained files", (t) => {
  for (const invalid of ["{", JSON.stringify({ version: 1, observations: [{ scenario: "invalid" }] })]) {
    const f = fixture(t);
    f.seed("good.json", [f.entry("unexecuted")]);
    f.put("website/frontend/proof/bad.json", invalid);
    const before = f.state();
    succeeded(f.record());
    assert.deepEqual(f.state(), before);
  }
});

test("a stale orphaned mutation lock is recovered by the next explicit run", (t) => {
  const f = fixture(t);
  f.seed("old.json", [f.entry("unexecuted")]);
  const lock = join(f.proof, ".proof.lock");
  mkdirSync(lock);
  utimesSync(lock, new Date(0), new Date(0)); // the filesystem state left by a long-dead owner
  succeeded(f.record());
  assert.equal(existsSync(lock), false);
  assert.deepEqual(f.files(), ["latest.json"]);
  assert.deepEqual(f.records()[0].map(({ scenario }) => scenario), ["completed", "unexecuted"]);
});

test("an explicit refresh waits for a live checkout lock and reloads proof after acquiring it", async (t) => {
  const f = fixture(t);
  f.seed("old.json", [f.entry("unexecuted")]);
  f.put("website/frontend/hold-lock.mjs", `
import lockfile from ${JSON.stringify(new URL("../node_modules/proper-lockfile/index.js", import.meta.url).href)};
import { writeFileSync } from 'node:fs';
const release = lockfile.lockSync(${JSON.stringify(f.proof)}, {lockfilePath: ${JSON.stringify(join(f.proof, ".proof.lock"))}, realpath: false, stale: 10000});
process.stdout.write('locked');
process.stdin.once('data', () => {
  writeFileSync(${JSON.stringify(join(f.proof, "old.json"))}, ${JSON.stringify(JSON.stringify({ version: 1,
    observations: [f.entry("unexecuted"), f.entry("written while waiting")] }))});
  release(); process.exit(0);
});
`);
  const holder = spawn(process.execPath, ["hold-lock.mjs"], { cwd: f.cwd, stdio: ["pipe", "pipe", "pipe"] });
  t.after(() => { if (holder.exitCode === null) holder.kill("SIGKILL"); });
  const held = new Promise((resolve, reject) => { holder.stdout.once("data", resolve); holder.once("error", reject); });
  const ended = new Promise((resolve) => holder.once("exit", resolve));
  await held;
  f.put("website/frontend/observe-lock.mjs", `
import fs from 'node:fs'; import { syncBuiltinESMExports } from 'node:module';
const mkdir = fs.mkdirSync, write = fs.writeFileSync;
fs.mkdirSync = function(path, ...args) {
  if (path === ${JSON.stringify(join(f.proof, ".proof.lock"))}) write('lock-requested', 'requested');
  return mkdir.call(this, path, ...args);
};
syncBuiltinESMExports();
`);
  const run = f.recordStart([], { NODE_OPTIONS: `--import=${pathToFileURL(join(f.cwd, "observe-lock.mjs")).href}` });
  await waitFor(() => existsSync(join(f.cwd, "lock-requested")));
  assert.deepEqual(f.files(), ["old.json"]);
  holder.stdin.write("release\n");
  assert.equal(await ended, 0);
  succeeded(await run.done);
  assert.deepEqual(f.records()[0].map(({ scenario }) => scenario), ["completed", "unexecuted", "written while waiting"]);
});

test("concurrent pass and cleanup transactions retain both changes instead of stale run snapshots", async (t) => {
  const f = fixture(t, "Feature: concurrency\n  Scenario: removed\n    Given failed\n  Scenario: retained\n    Given passed\n");
  f.seed("old.json", [f.entry("removed"), f.entry("unexecuted")]);
  const pass = f.recordStart(["--name", "retained", "--parallel", "2", "--format", "message:pass.ndjson"]);
  const failure = f.recordStart(["--name", "removed", "--parallel", "2", "--format", "message:failure.ndjson"]);
  const [passed, failed] = await Promise.all([pass.done, failure.done]);
  succeeded(passed);
  assert.equal(failed.status, 1, failed.stdout);
  assert.equal(failed.stderr, "");
  assert.deepEqual(f.files(), ["latest.json"]);
  assert.deepEqual(f.records()[0].map(({ scenario }) => scenario), ["retained", "unexecuted"]);
  assert.equal(existsSync(join(f.proof, ".proof.lock")), false);
});
