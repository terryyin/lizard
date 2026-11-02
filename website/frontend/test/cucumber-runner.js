import assert from "node:assert/strict";
import { execFileSync, spawn, spawnSync } from "node:child_process";
import { existsSync, mkdtempSync, mkdirSync, readFileSync, readdirSync, realpathSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const frontend = resolve(dirname(fileURLToPath(import.meta.url)), "..");
export const cucumberApi = pathToFileURL(join(frontend, "node_modules/@cucumber/cucumber/lib/api/index.js")).href;
const runner = join(frontend, "node_modules/@cucumber/cucumber/bin/cucumber.js");
const cucumber = pathToFileURL(join(frontend, "node_modules/@cucumber/cucumber/lib/index.js")).href;
const plugin = join(frontend, "proof-plugin.js");
const projectConfiguration = pathToFileURL(join(frontend, "cucumber.mjs")).href;
export const simple = "Feature: proof\n  Scenario: completed\n    Given passed\n";

export function fixture(t, feature = simple, extraSteps = "") {
  const root = realpathSync(mkdtempSync(join(tmpdir(), "lizard-cucumber-proof-")));
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
  const configure = (record = true, options = {}) => put("website/frontend/cucumber.mjs", `
import project from ${JSON.stringify(projectConfiguration)};
export default { ...project, ...${JSON.stringify({
    paths: ["features/**/*.feature"], import: ["steps.mjs"],
    format: ["progress", "html:report.html"],
  })}, plugin: ${!record ? "[]" : record === "forced" ? JSON.stringify([plugin])
    : `project.plugin.map(path => path.startsWith('.') ? ${JSON.stringify(frontend)} + '/' + path : path)`}, ...${JSON.stringify(options)} };\n`);
  configure();
  git("add", ".");
  git("commit", "-qm", "test sources");
  const command = (args = []) => [runner, "--format", "message:messages.ndjson", ...args];
  const environment = (env) => ({ ...process.env, RECORD_CUCUMBER_PROOF: "", ...env });
  const run = (args = [], env = {}) => spawnSync(process.execPath, command(args), {
    cwd, env: environment(env), encoding: "utf8", timeout: 30000,
  });
  const record = (args = [], env = {}) => run(args, { ...env, RECORD_CUCUMBER_PROOF: "1" });
  const start = (args = [], env = {}) => {
    const child = spawn(process.execPath, command(args), { cwd, env: environment(env), stdio: ["ignore", "pipe", "pipe"] });
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
  const recordStart = (args = [], env = {}) => start(args, { ...env, RECORD_CUCUMBER_PROOF: "1" });
  const files = (directory = proof, prefix = "") => !existsSync(directory) ? [] : readdirSync(directory, { withFileTypes: true })
    .flatMap((entry) => entry.isDirectory() ? files(join(directory, entry.name), `${prefix}${entry.name}/`)
      : entry.name.endsWith(".json") ? [`${prefix}${entry.name}`] : []).sort();
  const records = () => files().map((file) => {
    const document = JSON.parse(readFileSync(join(proof, file), "utf8"));
    assert.deepEqual(Object.keys(document).sort(), ["observations", "version"]);
    assert.equal(document.version, 1);
    return document.observations;
  });
  const entry = (scenario, values = {}) => ({
    feature: "website/frontend/features/recorded/test.feature", scenario, outcome: "passed", attempts: 1,
    durationMs: 1, finishedAt: "2000-01-01T00:00:00.000Z", commit: git("rev-parse", "HEAD"),
    featureBlob: git("hash-object", "website/frontend/features/recorded/test.feature"), ...values,
  });
  const seed = (name, observations) => put(`website/frontend/proof/${name}`, JSON.stringify({ version: 1, observations }));
  const state = (directory = proof, prefix = "") => !existsSync(directory) ? null
    : readdirSync(directory, { withFileTypes: true }).sort((a, b) => a.name < b.name ? -1 : 1).flatMap((item) =>
      item.isDirectory() ? [[`${prefix}${item.name}/`, null], ...state(join(directory, item.name), `${prefix}${item.name}/`)]
        : [[`${prefix}${item.name}`, readFileSync(join(directory, item.name), "utf8")]]);
  const messages = () => readFileSync(join(cwd, "messages.ndjson"), "utf8").trim().split("\n").map(JSON.parse);
  return { root, cwd, proof, put, git, configure, run, record, start, recordStart, files, records, entry, seed, state, messages };
}

export function succeeded(result) {
  assert.equal(result.status, 0, `${result.stdout}\n${result.stderr}\n${result.error ?? ""}`);
  assert.equal(result.stderr, "");
}

export async function waitFor(observed) {
  const deadline = Date.now() + 15000;
  while (!observed()) {
    assert.ok(Date.now() < deadline, "runner did not reach the synchronization point");
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
}
