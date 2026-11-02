import { randomUUID } from "node:crypto";
import { mkdirSync, readFileSync, readdirSync, renameSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import lockfile from "proper-lockfile";

const outcomes = new Set(["passed", "failed", "skipped", "pending", "undefined", "ambiguous", "unknown"]);
const gitObjectId = /^(?:[a-f0-9]{40}|[a-f0-9]{64})$/;
const identity = ({ feature, scenario, example }) => JSON.stringify([feature, scenario, example ?? null]);

function observation(value) {
  const { feature, scenario, example, outcome, attempts, durationMs, finishedAt, commit, featureBlob } = value ?? {};
  if (typeof feature !== "string" || !feature || typeof scenario !== "string"
      || (example !== undefined && (!Array.isArray(example) || example.some((cell) => typeof cell !== "string")))
      || !outcomes.has(outcome) || !Number.isInteger(attempts) || attempts < 1
      || typeof durationMs !== "number" || !Number.isFinite(durationMs) || durationMs < 0
      || typeof finishedAt !== "string" || !Number.isFinite(Date.parse(finishedAt))
      || typeof commit !== "string" || !gitObjectId.test(commit)
      || typeof featureBlob !== "string" || !gitObjectId.test(featureBlob)) throw new Error("Invalid proof observation");
  return { feature, scenario, ...(example === undefined ? {} : { example }), outcome, attempts,
    durationMs, finishedAt, commit, featureBlob };
}

function proofFiles(directory) {
  return readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name);
    return entry.isDirectory() ? proofFiles(path) : entry.name.endsWith(".json") ? [path] : [];
  }).sort();
}

function retainLatest(latest, value, incoming = false) {
  const key = identity(value), previous = latest.get(key);
  const time = Date.parse(value.finishedAt), previousTime = Date.parse(previous?.finishedAt);
  if (!previous || time > previousTime
      || (time === previousTime && (value.outcome !== "passed" && previous.outcome === "passed"
        || incoming && value.outcome === "passed" && previous.outcome === "passed"))) latest.set(key, value);
}

function atomicWrite(path, entries) {
  const temporary = `${path}.${randomUUID()}.tmp`;
  try {
    writeFileSync(temporary, `${JSON.stringify({ version: 1, observations: entries }, null, 2)}\n`, { flag: "wx" });
    renameSync(temporary, path);
  } finally {
    rmSync(temporary, { force: true });
  }
}

function acquire(directory) {
  const deadline = Date.now() + 15000;
  const sleeper = new Int32Array(new SharedArrayBuffer(4));
  for (;;) {
    try {
      return lockfile.lockSync(directory, {
        lockfilePath: join(directory, ".proof.lock"), realpath: false, stale: 10000,
        // Recording must never turn an asynchronous lease error into a test failure.
        onCompromised: () => {},
      });
    } catch (error) {
      if (error.code !== "ELOCKED" || Date.now() >= deadline) throw error;
      Atomics.wait(sleeper, 0, 0, 20);
    }
  }
}

/** One synchronous transaction per final executed scenario, never an in-memory run snapshot. */
export function refreshProof(directory, result) {
  mkdirSync(directory, { recursive: true });
  const release = acquire(directory);
  try {
    const files = proofFiles(directory), latest = new Map();
    for (const file of files) {
      const document = JSON.parse(readFileSync(file, "utf8"));
      if (document.version !== 1 || !Array.isArray(document.observations)) throw new Error("Invalid proof file");
      for (const entry of document.observations) retainLatest(latest, observation(entry));
    }
    retainLatest(latest, observation(result), true);
    const successes = [...latest].filter(([, { outcome }]) => outcome === "passed")
      .sort(([left], [right]) => left < right ? -1 : left > right ? 1 : 0).map(([, value]) => value);
    const snapshot = join(directory, "latest.json");
    if (successes.length) atomicWrite(snapshot, successes);
    // A failed final result is absent everywhere; never persist a failure marker.
    for (const file of files) if (file !== snapshot || !successes.length) rmSync(file);
  } finally {
    release();
  }
}
