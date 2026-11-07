// Historical lookup for the gate only. Ordinary target polling is unchanged.
import { readGitHubActions, askGitHub } from "./ci-github-actions.mjs";

function directProof(log) {
  const proofs = log.split("\n").flatMap((line) => {
    // Accept emitted output, never the shell's echoed command/script source.
    const emitted = line.match(
      /^(?:\d{4}-\d\d-\d\dT[\d:.]+Z )?DOUGH_CI_DIRECT (\{.*\})\r?$/,
    );
    if (!emitted) return [];
    try {
      return [JSON.parse(emitted[1])];
    } catch {
      return [null];
    }
  });
  return proofs.length === 1 ? proofs[0] : null;
}

// `freshOverride` must come from the project's current scheduling request.
// A bounded exact-SHA listing includes failures/pending attempts too. No
// branch/event/status filter can hide a currently relevant contradiction.
export async function readGitHubReuseFacts(
  {
    sha,
    policy,
    currentRunId,
    freshOverride,
    maxPages = 3,
    gh = readGitHubActions,
    log = askGitHub,
  },
  requestedSignal,
) {
  const signal = requestedSignal
    ? AbortSignal.any([requestedSignal, AbortSignal.timeout(20000)])
    : AbortSignal.timeout(20000);
  const query = { complete: false, freshOverride, attempts: [] };
  if (freshOverride !== false) return query;
  const api = (route) => ["api", `repos/${policy.repo}/actions/${route}`];
  try {
    if (!Number.isInteger(maxPages) || maxPages < 1 || maxPages > 10)
      return query;
    const workflow = await gh(
      api(`workflows/${encodeURIComponent(policy.workflow)}`),
      signal,
    );
    const runs = [];
    let total;
    for (let page = 1; page <= maxPages; page += 1) {
      const params = new URLSearchParams({
        head_sha: sha,
        per_page: "100",
        page: String(page),
      });
      const answer = await gh(
        api(`workflows/${workflow.id}/runs?${params}`),
        signal,
      );
      if (
        !Array.isArray(answer.workflow_runs) ||
        answer.workflow_runs.length > 100 ||
        !Number.isInteger(answer.total_count) ||
        answer.total_count < 0
      )
        return query;
      if (total !== undefined && total !== answer.total_count) return query;
      total = answer.total_count;
      runs.push(...answer.workflow_runs);
      if (runs.length === total) break;
      if (runs.length > total || answer.workflow_runs.length < 100)
        return query;
    }
    if (
      runs.length !== total ||
      total >= 1000 ||
      new Set(runs.map((run) => String(run.id))).size !== runs.length
    )
      return query;
    for (const listed of runs) {
      if (String(listed.id) === String(currentRunId)) continue;
      const run = await gh(api(`runs/${listed.id}`), signal);
      if (
        run.head_sha !== sha ||
        run.workflow_id !== workflow.id ||
        run.run_attempt !== listed.run_attempt ||
        run.head_repository?.full_name !== policy.repo
      )
        return query;
      if (
        !policy.refs.includes(run.head_branch) ||
        !policy.events.includes(run.event)
      )
        continue;
      const attempt = await gh(
        api(`runs/${run.id}/attempts/${run.run_attempt}`),
        signal,
      );
      if (attempt.run_attempt !== run.run_attempt || attempt.head_sha !== sha)
        return query;
      const facts = {
        sha,
        repo: policy.repo,
        workflow: policy.workflow,
        ref: run.head_branch,
        event: run.event,
        runId: String(run.id),
        attemptId: String(run.run_attempt),
        url: run.html_url,
        current: true,
        order: Date.parse(attempt.run_started_at),
        outcome:
          attempt.status === "completed" ? attempt.conclusion : "pending",
        direct: false,
      };
      const answer = await gh(
        api(
          `runs/${run.id}/attempts/${run.run_attempt}/jobs?per_page=100&page=1`,
        ),
        signal,
      );
      if (
        !Array.isArray(answer.jobs) ||
        answer.total_count !== answer.jobs.length ||
        answer.jobs.length > 100
      )
        return query;
      const jobs = answer.jobs.filter((job) => job.name === policy.unit);
      if (
        jobs.length === 1 &&
        jobs[0].status === "completed" &&
        jobs[0].conclusion === "success"
      ) {
        const proof = directProof(
          await log(api(`jobs/${jobs[0].id}/logs`), signal),
        );
        if (
          proof?.sha === sha &&
          proof.unit === policy.unit &&
          proof.configuration === policy.configuration &&
          proof.context === policy.context
        ) {
          Object.assign(facts, {
            unit: proof.unit,
            configuration: proof.configuration,
            context: proof.context,
            direct: true,
            validatedAt: Date.parse(jobs[0].completed_at),
            validUntil: Date.parse(jobs[0].completed_at) + policy.maxAgeMs,
          });
        }
      }
      // A rerun starting during acquisition invalidates this snapshot.
      const current = await gh(api(`runs/${run.id}`), signal);
      if (
        current.run_attempt !== run.run_attempt ||
        current.updated_at !== run.updated_at
      )
        return query;
      query.attempts.push(facts);
    }
    signal.throwIfAborted();
    query.complete = true;
  } catch {
    /* Unreadable/rate-limited/invalid answers choose fresh execution. */
  }
  return query;
}
