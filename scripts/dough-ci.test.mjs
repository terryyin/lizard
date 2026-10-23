import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";
import { readCiAdapter } from "../.agents/skills/dough-execute-plan/scripts/ci-command-adapter.mjs";

const root = fileURLToPath(new URL("..", import.meta.url));
const check = { repo: "owner/project", branch: "codex/adapter" };
const sha = "0123456789abcdef0123456789abcdef01234567";

function fixture(t, data) {
  const directory = mkdtempSync("/tmp/lizard-ci-");
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  writeFileSync(join(directory, "fixture.json"), JSON.stringify(data));
  writeFileSync(
    join(directory, "gh"),
    `#!${process.execPath}
const {appendFileSync, readFileSync} = require("node:fs");
const args = process.argv.slice(2);
if (args[0] === "auth") { console.log("fixture-token"); process.exit(); }
appendFileSync(process.env.CI_CALLS, JSON.stringify(args) + "\\n");
const route = args.find(value => value.startsWith("repos/"));
const data = JSON.parse(readFileSync(process.env.CI_FIXTURE, "utf8"));
let body;
if (route === "repos/owner/project/actions/workflows/python-app.yml") body = {id: 73, name: "Python application"};
else if (route?.startsWith("repos/owner/project/actions/workflows/73/runs?")) body = {workflow_runs: data.runs};
else if (route === "repos/owner/project/actions/runs/run:opaque/attempts/try:opaque/jobs?per_page=100&page=1") body = {jobs: data.jobs};
else if (route === "repos/owner/project/actions/jobs/job:opaque/logs") body = data.log;
else { console.error("unexpected route", route); process.exit(1); }
process.stdout.write("HTTP/2.0 200 OK\\r\\n\\r\\n" + (typeof body === "string" ? body : JSON.stringify(body)));
`,
    { mode: 0o700 },
  );
  const env = {
    ...process.env,
    PATH: `${directory}:${process.env.PATH}`,
    CI_FIXTURE: join(directory, "fixture.json"),
    CI_CALLS: join(directory, "calls.jsonl"),
    DOUGH_GITHUB_REQUEST_OWNER_ROOT: join(directory, "owner"),
    DOUGH_GITHUB_REQUEST_OWNER_IDLE_MS: "10",
  };
  delete env.DOUGH_CI_WORKFLOW;
  delete env.IN_NIX_SHELL;
  return {
    invoke(request, extra = {}) {
      const command = readCiAdapter(root);
      assert.deepEqual(command, ["node", "scripts/dough-ci.mjs", "python-app.yml"]);
      return spawnSync(process.execPath, command.slice(1), {
        cwd: root,
        env: { ...env, ...extra },
        input: JSON.stringify(request) + "\n",
        encoding: "utf8",
        timeout: 10_000,
      });
    },
    calls() {
      return readFileSync(env.CI_CALLS, "utf8").trim().split("\n").map(JSON.parse);
    },
  };
}

test("project command discovers exact identities and all outcomes without Nix", (t) => {
  const cases = [
    ["queued", null, "pending"],
    ["in_progress", "failure", "pending"],
    ["completed", "success", "success"],
    ...["failure", "timed_out", "startup_failure", "action_required", "stale"].map(
      (conclusion) => ["completed", conclusion, "failure"],
    ),
    ...["cancelled", "skipped", "neutral", null].map(
      (conclusion) => ["completed", conclusion, "incomplete"],
    ),
  ];
  const runs = cases.map(([status, conclusion], index) => ({
    id: `run:${index}`, run_attempt: `attempt:${index}`, head_sha: sha,
    head_branch: check.branch, event: "push", status, conclusion,
    html_url: `https://example.test/${index}`, created_at: "2026-10-23T10:00:00Z",
  }));
  const fake = fixture(t, { runs });
  const result = fake.invoke({ operation: "discover", check });
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(JSON.parse(result.stdout), {
    attempts: runs.map((run, index) => ({
      runId: run.id, attemptId: run.run_attempt, sha,
      outcome: cases[index][2], branch: check.branch, check: "Python application",
      context: "push", url: run.html_url, time: run.created_at,
    })),
  });
  const conflicting = fake.invoke(
    { operation: "discover", check },
    { DOUGH_CI_WORKFLOW: "wrong.yml" },
  );
  assert.equal(conflicting.status, 0, conflicting.stderr);
  assert.deepEqual(JSON.parse(conflicting.stdout), JSON.parse(result.stdout));
  const calls = fake.calls();
  assert.equal(calls.length, 4);
  const route = calls[1].find((argument) => argument.startsWith("repos/"));
  const query = new URL(`https://example.test/${route}`).searchParams;
  assert.equal(query.get("branch"), check.branch);
  assert.equal(query.get("event"), "push");
});

test("diagnosis preserves attempt IDs and bounds useful Unicode log evidence", (t) => {
  const fake = fixture(t, {
    jobs: [{ id: "job:opaque", name: "tests", conclusion: "failure", steps: [] }],
    log: Array.from({ length: 40 }, () => "錯".repeat(300)).join("\n"),
  });
  const result = fake.invoke({
    operation: "diagnose", check,
    attempt: { runId: "run:opaque", attemptId: "try:opaque", sha },
  });
  assert.equal(result.status, 0, result.stderr);
  const answer = JSON.parse(result.stdout);
  assert.match(answer.excerpt, /^tests: failure\n錯/);
  assert.ok(Buffer.byteLength(answer.excerpt) <= 16 * 1024);
  assert.ok(Buffer.byteLength(result.stdout) <= 64 * 1024);
  assert.equal(answer.excerpt.includes("�"), false);
  assert.equal(answer.truncated, true);
  assert.equal(fake.calls().length, 2);
});

test("diagnosis honestly reports absent evidence", (t) => {
  const fake = fixture(t, { jobs: [] });
  const result = fake.invoke({
    operation: "diagnose", check,
    attempt: { runId: "run:opaque", attemptId: "try:opaque", sha },
  });
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(JSON.parse(result.stdout), {
    unavailable: "GitHub returned no failed jobs for this attempt",
  });
});

test("unsupported operations fail without a protocol response", (t) => {
  const fake = fixture(t, {});
  const result = fake.invoke({ operation: "retry", check });
  assert.equal(result.status, 2);
  assert.equal(result.stdout, "");
  assert.equal(result.stderr, "unsupported CI adapter operation\n");
});
