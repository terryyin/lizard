// The authorized integration target that accepted supplier integration is
// judged against: a full accepted revision, the target fetched from its
// remote, and Git containment between revisions.
import { git, gitLine, gitOutcome } from "./product-backlog-git-repository.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";

export function fullRevision(value, field) {
  requireField(value, field);
  if (!/^[0-9a-f]{40,64}$/.test(value))
    throw new BacklogError(`${field} must be a full Git revision.`);
  return value;
}

export const contains = (before, after, root) =>
  gitOutcome(["merge-base", "--is-ancestor", before, after], root).code === 0;

// Validates --accepted-revision, --remote and --target, then fetches that
// target and reports its tip.
export function fetchAcceptedTarget(values, root) {
  const accepted = fullRevision(
    values["accepted-revision"],
    "accepted-revision",
  );
  requireField(values.remote, "remote");
  requireField(values.target, "target");
  const ref = values.target.startsWith("refs/heads/")
    ? values.target
    : `refs/heads/${values.target}`;
  git(["check-ref-format", ref], root);
  git(["fetch", "--quiet", values.remote, ref], root);
  return {
    accepted,
    target: {
      remote: values.remote,
      ref,
      sha: gitLine(["rev-parse", "FETCH_HEAD"], root),
    },
  };
}
