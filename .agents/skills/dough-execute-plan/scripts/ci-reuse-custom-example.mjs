#!/usr/bin/env node
// Runnable local provider example. Serialize scheduling; replace the journal
// with the project's authenticated, complete current-attempt API in real CI.
import { spawnSync } from "node:child_process";
import { mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { isFullGitRevision } from "./ci-revisions.mjs";

function journal(path) {
  try {
    const value = JSON.parse(readFileSync(path, "utf8"));
    if (!Array.isArray(value.entries) || !Number.isFinite(value.sequence))
      throw Error("invalid provider journal");
    return { ...value, readable: true };
  } catch (error) {
    return { entries: [], sequence: 0, readable: error.code === "ENOENT" };
  }
}

function writeJson(path, value) {
  mkdirSync(dirname(path), { recursive: true });
  const pending = `${path}.${process.pid}.tmp`;
  writeFileSync(pending, `${JSON.stringify(value, null, 2)}\n`);
  renameSync(pending, path);
}

function run(command) {
  if (
    !Array.isArray(command) ||
    !command.length ||
    command.some((argument) => typeof argument !== "string" || !argument)
  )
    throw Error("check command must be a nonempty argument array");
  const result = spawnSync(command[0], command.slice(1), {
    encoding: "utf8",
    timeout: 120000,
    maxBuffer: 16 * 1024,
  });
  return {
    success: !result.error && result.status === 0,
    log: `${result.stdout ?? ""}${result.stderr ?? ""}`,
  };
}

export function scheduleCustomAttempt(mode, statePath, job) {
  if (
    !["source", "target"].includes(mode) ||
    !["repo", "workflow", "ref", "event", "runId", "attemptId"].every(
      (field) => typeof job[field] === "string" && job[field],
    ) ||
    !isFullGitRevision(job.sha)
  )
    throw Error(
      "supply source/target mode and the provider's exact job identity",
    );
  const state = journal(statePath);
  if (
    state.entries.some(
      (entry) =>
        entry?.attempt?.runId === job.runId &&
        entry.attempt.attemptId === job.attemptId,
    )
  )
    throw Error("each scheduled attempt needs its own provider identity");
  const order = state.sequence + 1; // creation sequence, never an ID comparison
  const report = resolve(`${statePath}.reports`, `${order}.json`);
  let decision = { reuse: false, reason: "direct-source", sha: job.sha };
  if (mode === "target") {
    const gate = spawnSync(
      process.execPath,
      [join(dirname(fileURLToPath(import.meta.url)), "ci-reuse-gate.mjs")],
      {
        encoding: "utf8",
        input: JSON.stringify({
          sha: job.sha,
          targetRef: job.ref,
          policy: job.policy,
          query: {
            complete:
              state.readable &&
              state.entries.every(
                (entry) =>
                  entry?.repo && entry.workflow && entry.attempt && entry.fact,
              ),
            freshOverride: job.freshOverride ?? false,
            attempts: state.entries
              .filter(
                (entry) =>
                  entry?.repo === job.repo &&
                  entry.workflow === job.workflow &&
                  entry.fact?.current !== false &&
                  entry.attempt?.sha === job.sha,
              )
              .map((entry) => entry.fact),
          },
        }),
      },
    );
    try {
      if (gate.error || gate.status !== 0) throw Error("gate unavailable");
      decision = JSON.parse(gate.stdout);
    } catch {
      decision = { reuse: false, reason: "unreadable-evidence", sha: job.sha };
    }
  }
  const startedAt = Date.now();
  const attempt = {
    runId: job.runId,
    attemptId: job.attemptId,
    sha: job.sha,
    outcome: "pending",
    url: pathToFileURL(report).href,
    time: new Date(startedAt).toISOString(),
  };
  const fact = {
    repo: job.repo,
    workflow: job.workflow,
    ref: job.ref,
    event: job.event,
    ...attempt,
    direct: false,
    current: true,
    order,
    unit: job.policy?.unit,
    configuration: job.policy?.configuration,
    context: job.policy?.context,
  };
  for (const entry of state.entries)
    if (
      entry?.repo === job.repo &&
      entry.attempt?.runId === job.runId &&
      entry.fact
    )
      entry.fact.current = false;
  const entries = [
    ...state.entries,
    {
      kind: mode,
      repo: job.repo,
      workflow: job.workflow,
      branch: job.ref,
      attempt,
      fact,
    },
  ];
  // Readers see the new unresolved attempt before covered execution starts.
  writeJson(statePath, { sequence: order, entries });
  const covered = decision.reuse
    ? { success: true, log: "covered execution reused" }
    : run(job.covered);
  const targetOnly = mode === "target" ? run(job.targetOnly) : undefined;
  const now = Date.now();
  Object.assign(attempt, {
    outcome:
      covered.success && (!targetOnly || targetOnly.success)
        ? "success"
        : "failure",
    time: new Date(now).toISOString(),
  });
  Object.assign(fact, {
    outcome: covered.success ? "success" : "failure",
    direct: !decision.reuse && covered.success,
    validatedAt: now,
    validUntil: now + (job.policy?.maxAgeMs ?? 0),
  });
  const result = { target: attempt, decision, covered, targetOnly, fact };
  writeJson(report, result);
  writeJson(statePath, { sequence: order, entries });
  return { attempt, decision };
}

// Target `entries` as discovered attempts, newest first, each naming its own
// target branch, check and context.
export function discoveredAttempts(entries) {
  return [...entries]
    .sort((left, right) => right.fact.order - left.fact.order)
    .map(({ attempt, branch, workflow, fact }) => ({
      ...attempt,
      branch,
      check: workflow,
      context: fact.event,
    }));
}

export function customAdapterResponse(statePath, request) {
  const state = journal(statePath);
  if (!state.readable) throw Error("provider journal unreadable");
  const entries = state.entries.filter(
    (entry) =>
      entry?.kind === "target" &&
      entry.repo === request.check.repo &&
      entry.branch === request.check.branch,
  );
  if (request.operation === "discover") {
    // This provider owns the current aggregate for each SHA. Do not expose an
    // earlier green beside its pending/failed replacement to success-preferring
    // consumers. Reports retain every historical target identity separately.
    const current = new Map();
    for (const entry of entries) {
      const previous = current.get(entry.attempt.sha);
      if (
        !Number.isFinite(entry.fact?.order) ||
        previous?.fact.order === entry.fact.order
      )
        throw Error("provider target ordering unavailable or ambiguous");
      if (!previous || entry.fact.order > previous.fact.order)
        current.set(entry.attempt.sha, entry);
    }
    return { attempts: discoveredAttempts(current.values()) };
  }
  if (request.operation !== "diagnose") throw Error("unsupported operation");
  const entry = entries.find(
    ({ attempt }) =>
      attempt.runId === request.attempt.runId &&
      attempt.attemptId === request.attempt.attemptId &&
      attempt.sha === request.attempt.sha,
  );
  if (!entry) return { unavailable: "provider has no matching target attempt" };
  const report = JSON.parse(readFileSync(new URL(entry.attempt.url), "utf8"));
  // Reports are inspectable locally; diagnostics expose only known outcomes,
  // never raw command logs or extra adapter fields.
  return {
    excerpt: `Covered check: ${report.covered.success ? "success" : "failure"}; target-only: ${report.targetOnly?.success ?? "not requested"}; reuse: ${report.decision.reason}`,
    truncated: false,
  };
}

if (
  process.argv[1] &&
  import.meta.url === pathToFileURL(process.argv[1]).href
) {
  const [mode, statePath, jobPath] = process.argv.slice(2);
  if (mode === "adapter") {
    let input = "";
    for await (const chunk of process.stdin) input += chunk;
    process.stdout.write(
      JSON.stringify(customAdapterResponse(statePath, JSON.parse(input))),
    );
  } else {
    const result = scheduleCustomAttempt(
      mode,
      statePath,
      JSON.parse(readFileSync(jobPath, "utf8")),
    );
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (result.attempt.outcome !== "success") process.exitCode = 1;
  }
}
