import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { isAbsolute, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { refreshProof } from "./proof-snapshot.js";

const statuses = ["UNKNOWN", "PASSED", "SKIPPED", "PENDING", "UNDEFINED", "AMBIGUOUS", "FAILED"];
const milliseconds = ({ seconds = 0, nanos = 0 } = {}) => Number(seconds) * 1000 + nanos / 1e6;
const inside = (directory, file) => {
  const path = relative(directory, file);
  return path !== ".." && !path.startsWith(`..${sep}`) && !isAbsolute(path);
};

/** A main-process observer only: it never controls execution or writes reports. */
export default {
  type: "plugin",
  coordinator({ operation, environment: { cwd, env }, on }) {
    if (operation !== "runCucumber" || env.RECORD_CUCUMBER_PROOF !== "1") return;
    try {
      const git = (...args) => execFileSync("git", args, { cwd, encoding: "utf8", stdio: ["pipe", "pipe", "pipe"] }).trim();
      const root = git("rev-parse", "--show-toplevel");
      const commit = git("rev-parse", "HEAD");
      const config = JSON.parse(readFileSync(join(root, ".planning/open-dough.json"), "utf8"));
      const locations = (config.intent?.locations ?? [])
        .filter(({ kind, path, proof }) => kind === "gherkin" && typeof path === "string" && typeof proof === "string")
        .map(({ path, proof }) => ({ path: resolve(root, path), proof: resolve(root, proof) }))
        .filter(({ path, proof }) => inside(root, path) && inside(root, proof));
      if (!locations.length) return;

      const sources = new Map();
      const nodes = new Map();
      const pickles = new Map();
      const cases = new Map();
      const attempts = new Map();
      const totals = new Map();

      const rememberChildren = (children) => {
        for (const { scenario, rule } of children ?? []) {
          if (rule) rememberChildren(rule.children);
          if (!scenario) continue;
          nodes.set(scenario.id, { scenario: scenario.name });
          for (const examples of scenario.examples ?? []) {
            for (const row of examples.tableBody ?? []) nodes.set(row.id, { example: row.cells.map(({ value }) => value) });
          }
        }
      };
      const observe = (message) => {
        if (message.source) {
          const { uri, data } = message.source;
          const file = uri.startsWith("file:") ? fileURLToPath(uri) : resolve(cwd, uri);
          const location = locations.find(({ path }) => inside(path, file));
          if (location) {
            // Hash Cucumber's loaded source, not a later edit to the file on disk.
            const featureBlob = execFileSync("git", ["hash-object", "--stdin", "--path", relative(root, file)], {
              cwd: root, input: data, encoding: "utf8", stdio: ["pipe", "pipe", "pipe"],
            }).trim();
            sources.set(uri, { feature: relative(root, file).split(sep).join("/"), featureBlob, proof: location.proof });
          }
        }
        if (message.gherkinDocument) rememberChildren(message.gherkinDocument.feature?.children);
        if (message.pickle) pickles.set(message.pickle.id, message.pickle);
        if (message.testCase) cases.set(message.testCase.id, message.testCase.pickleId);
        if (message.testCaseStarted) {
          const started = message.testCaseStarted;
          // attempt zero is a new execution, even when its identity ran earlier.
          if (started.attempt === 0) totals.delete(started.testCaseId);
          attempts.set(started.id, { started, statuses: [], durationMs: 0 });
        }
        if (message.testStepFinished) {
          const { testCaseStartedId, testStepResult } = message.testStepFinished;
          const attempt = attempts.get(testCaseStartedId);
          if (attempt) {
            attempt.statuses.push(testStepResult.status);
            attempt.durationMs += milliseconds(testStepResult.duration);
          }
        }
        if (!message.testCaseFinished) return;
        const finished = message.testCaseFinished;
        const attempt = attempts.get(finished.testCaseStartedId);
        attempts.delete(finished.testCaseStartedId);
        if (!attempt) return;
        const { started, durationMs } = attempt;
        // Executed steps/hooks carry stopwatch time, even when returning
        // 'skipped'. Dry-run/fail-fast also emit undefined/ambiguous results
        // without invoking any code; status alone is not execution evidence.
        if (!(durationMs > 0)) return;
        const total = totals.get(started.testCaseId) ?? { attempts: 0, durationMs: 0 };
        total.attempts += 1;
        total.durationMs += durationMs;
        totals.set(started.testCaseId, total);
        if (finished.willBeRetried) return;
        totals.delete(started.testCaseId);
        const pickle = pickles.get(cases.get(started.testCaseId));
        const source = sources.get(pickle?.uri);
        if (!source) return;
        const identity = Object.assign({}, ...pickle.astNodeIds.map((id) => nodes.get(id)));
        if (typeof identity.scenario !== "string") return;
        const outcome = statuses[Math.max(0, ...attempt.statuses.map((status) => statuses.indexOf(status)))].toLowerCase();
        const observation = {
          feature: source.feature, ...identity, outcome, ...total,
          finishedAt: new Date(milliseconds(finished.timestamp)).toISOString(), commit, featureBlob: source.featureBlob,
        };
        refreshProof(source.proof, observation);
      };
      on("message", (message) => {
        try { observe(message); } catch { /* Proof is best effort and never changes the test result. */ }
      });
    } catch { /* No Git checkout, missing configuration or unwritable proof: stay silent. */ }
  },
};
