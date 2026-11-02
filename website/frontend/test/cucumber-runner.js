import assert from "node:assert/strict";
import { execFileSync, spawn, spawnSync } from "node:child_process";
import { mkdtempSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const frontend = resolve(dirname(fileURLToPath(import.meta.url)), "..");
export const cucumberApi = pathToFileURL(join(frontend, "node_modules/@cucumber/cucumber/lib/api/index.js")).href;
const runner = join(frontend, "node_modules/@cucumber/cucumber/bin/cucumber.js");
const cucumber = pathToFileURL(join(frontend, "node_modules/@cucumber/cucumber/lib/index.js")).href;
const plugin = join(frontend, "proof-plugin.js");
export const simple = "Feature: proof\n  Scenario: completed\n    Given passed\n";

export function fixture(t, feature = simple, extraSteps = "") {
  const root = mkdtempSync(join(tmpdir(), "lizard-cucumber-proof-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const cwd = join(root, "website/frontend");
  const proof = join(cwd, "proof");
  const put = (path, data) => {
    mkdirSync(dirname(join(root, path)), { recursive: true });
    writeFileSync(join(root, path), data);
  };
  const git = (...args) => execFileSync("git", args, { cwd: root, encoding: "utf8", stdio: ["pipe", "pipe", "pipe"] }).trim();
  git("init", "-q");
  git("config", "user.name", "Proof test");
  git("config", "user.email", "proof@example.invalid");
  put(".planning/open-dough.json", JSON.stringify({ intent: { locations: [
    { path: "website/frontend/features/recorded", kind: "gherkin", proof: "website/frontend/proof" },
  ] } }));
  put("website/frontend/features/recorded/test.feature", feature);
  put("website/frontend/steps.mjs", `
import { Given, Before, After, BeforeAll } from ${JSON.stringify(cucumber)};
import { existsSync, readdirSync, readFileSync, writeFileSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
Given('passed', function () {});
Given('failed', function () { throw new Error('intentional failure'); });
Given('pending', function () { return 'pending'; });
Given('skipped', function () { return 'skipped'; });
Given('ambiguous', function () {});
Given('ambiguous', function () {});
${extraSteps}
`);
  const configure = (record = true, options = {}) => put("website/frontend/cucumber.mjs", `export default ${JSON.stringify({
    paths: ["features/**/*.feature"], import: ["steps.mjs"],
    format: ["progress", "html:report.html"], plugin: record ? [plugin] : [], ...options,
  })};\n`);
  configure();
  git("add", ".");
  git("commit", "-qm", "test sources");
  const command = (args = []) => [runner, "--format", "message:messages.ndjson", ...args];
  const run = (args = [], env = {}) => spawnSync(process.execPath, command(args), {
    cwd, env: { ...process.env, ...env }, encoding: "utf8", timeout: 30000,
  });
  const start = (args = []) => {
    const child = spawn(process.execPath, command(args), { cwd, env: process.env, stdio: ["ignore", "pipe", "pipe"] });
    let stdout = "", stderr = "";
    child.stdout.on("data", (chunk) => { stdout += chunk; });
    child.stderr.on("data", (chunk) => { stderr += chunk; });
    const done = new Promise((resolve, reject) => {
      child.once("error", reject);
      child.once("exit", (status, signal) => resolve({ status, signal, stdout, stderr }));
    });
    t.after(() => { if (child.exitCode === null) child.kill("SIGKILL"); });
    return { child, done };
  };
  const files = () => readdirSync(proof).filter((name) => name.endsWith(".json"));
  const records = () => files().map((file) => {
    const document = JSON.parse(readFileSync(join(proof, file), "utf8"));
    assert.deepEqual(Object.keys(document).sort(), ["observations", "version"]);
    assert.equal(document.version, 1);
    return document.observations;
  });
  const messages = () => readFileSync(join(cwd, "messages.ndjson"), "utf8").trim().split("\n").map(JSON.parse);
  return { root, cwd, proof, put, git, configure, run, start, files, records, messages };
}

export function succeeded(result) {
  assert.equal(result.status, 0, `${result.stdout}\n${result.stderr}\n${result.error ?? ""}`);
  assert.equal(result.stderr, "");
}
