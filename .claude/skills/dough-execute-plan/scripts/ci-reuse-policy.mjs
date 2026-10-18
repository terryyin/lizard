// Provider-owned evidence at the scheduling boundary. IDs are opaque; ordering
// is an explicit provider fact, never inferred from IDs or preferredAttempt.
import { isFullGitRevision } from "./ci-revisions.mjs";
const nonempty = (value) => typeof value === "string" && value.length > 0;
const same = (left, right) => JSON.stringify(left) === JSON.stringify(right);
const keyFields = ["repo", "workflow", "unit", "configuration", "context"];
const identity = (fact) => JSON.stringify([fact.runId, fact.attemptId]);

export function reusableValidation({
  sha,
  targetRef,
  policy,
  query,
  now = Date.now(),
} = {}) {
  const fresh = (reason) => ({ reuse: false, reason, sha });
  if (
    !isFullGitRevision(sha) ||
    !policy ||
    !nonempty(policy.targetRef) ||
    targetRef !== policy.targetRef ||
    !keyFields.every((field) => nonempty(policy[field])) ||
    ![policy.refs, policy.events].every(
      (values) =>
        Array.isArray(values) && values.length && values.every(nonempty),
    ) ||
    !Number.isFinite(policy.maxAgeMs) ||
    policy.maxAgeMs <= 0
  )
    return fresh("invalid-policy");
  if (query?.freshOverride !== false) return fresh("fresh-override-or-unknown");
  if (query.complete !== true || !Array.isArray(query.attempts))
    return fresh("incomplete-query");
  if (
    !Number.isFinite(now) ||
    query.attempts.some(
      (fact) =>
        !fact ||
        !["sha", "repo", "workflow", "ref", "event"].every((field) =>
          nonempty(fact[field]),
        ),
    )
  )
    return fresh("unknown-source-facts");
  // Reader must include every relevant current attempt, successful or not.
  // Policy narrows trust before ordering. Unknown source facts fail closed.
  const relevant = query.attempts.filter(
    (fact) =>
      fact.sha === sha &&
      fact.repo === policy.repo &&
      fact.workflow === policy.workflow &&
      policy.refs.includes(fact.ref) &&
      policy.events.includes(fact.event),
  );
  if (!relevant.length) return fresh("no-source");
  if (
    relevant.some(
      (fact) =>
        !nonempty(fact.runId) ||
        !nonempty(fact.attemptId) ||
        !Number.isFinite(fact.order) ||
        fact.current !== true,
    )
  )
    return fresh("unknown-current-attempt");
  if (new Set(relevant.map(identity)).size !== relevant.length)
    return fresh("contradictory-attempts");
  const ordered = relevant.toSorted((a, b) => b.order - a.order);
  if (ordered.length > 1 && ordered[0].order === ordered[1].order)
    return fresh("ambiguous-order");
  const source = ordered[0];
  if (source.outcome !== "success" || source.direct !== true)
    return fresh("latest-not-direct-success");
  if (!keyFields.every((field) => same(source[field], policy[field])))
    return fresh("context-mismatch");
  if (
    !Number.isFinite(source.validatedAt) ||
    source.validatedAt > now ||
    now - source.validatedAt > policy.maxAgeMs ||
    !Number.isFinite(source.validUntil) ||
    source.validUntil <= now
  )
    return fresh("expired-or-unknown-validity");
  if (!nonempty(source.url)) return fresh("uninspectable-source");
  return { reuse: true, reason: "current-direct-success", sha, source };
}
