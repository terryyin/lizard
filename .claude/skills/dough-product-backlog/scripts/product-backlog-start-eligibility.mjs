// Start eligibility: whether new execution of a story may start, derived from
// the two facts its canonical home already records — the preparing agent's
// readiness assessment (`readStoryState`) and the dependency agreement
// (`readStoryDependencies`). Nothing here is stored. "Readiness" keeps
// meaning the recorded assessment; eligibility is the derived answer.
//
// Pure over already-read results, with no filesystem access, so the CLI, the
// start command, and the dashboard share one meaning. A reader that refused
// is passed as `{ status: "unavailable", problem }`.

function gap(fact, problem) {
  return { kind: "gap", fact, problem };
}

function dependencyBlockers(dependencies) {
  if (dependencies?.status === "unavailable")
    return [gap("dependencies", dependencies.problem)];
  if (
    (dependencies?.status !== "recorded" &&
      dependencies?.status !== "not-recorded") ||
    !Array.isArray(dependencies.blocking)
  )
    return [gap("dependencies", "Dependency facts are not available.")];
  return dependencies.blocking.map((entry) => ({
    kind: "dependency",
    supplier: entry.supplier.identity,
    href: entry.supplier.href,
    state: entry.state,
    rationale: entry.rationale,
    condition: entry.condition,
    ...(entry.decision !== undefined && { decision: entry.decision }),
  }));
}

function preparationBlockers(preparation) {
  if (preparation?.status === "unavailable")
    return [gap("preparation", preparation.problem)];
  if (preparation?.status === "not-recorded")
    return [gap("preparation", "Preparation is not recorded.")];
  if (preparation?.status === "unsupported-version")
    return [
      gap(
        "preparation",
        `Unsupported story-state schema version ${JSON.stringify(preparation.schemaVersion)}.`,
      ),
    ];
  if (preparation?.status !== "recorded")
    return [gap("preparation", "Preparation facts are not available.")];
  const { assessment } = preparation;
  if (assessment?.status === "ready") return [];
  if (assessment?.status === "not-ready" || assessment?.status === "absent")
    return [
      {
        kind: "assessment",
        status: assessment.status,
        reasons: assessment.reasons ?? [],
      },
    ];
  return [gap("preparation", "The readiness assessment is not available.")];
}

// Eligible only when the assessment is `ready` and no dependency is
// unsatisfied. Blockers list each unsatisfied dependency first, then the
// assessment or the preparation gap.
export function deriveStartEligibility({ preparation, dependencies }) {
  const blockers = [
    ...dependencyBlockers(dependencies),
    ...preparationBlockers(preparation),
  ];
  return blockers.length === 0
    ? { status: "eligible", blockers }
    : { status: "blocked", blockers };
}
