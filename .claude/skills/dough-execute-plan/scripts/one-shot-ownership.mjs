// Whether queued work may still finish as one-shot work on a trunk revision:
// no Taken entry and no agent profile (execution or preparation) names it, and
// its recorded preparation carries no not-ready reason. The start reads it on
// fetched trunk; managed delivery rereads it on each fetched target tip, never
// on a rebased candidate that could hide a competing holder. Delivery loads it
// only for one-shot work, so it otherwise never needs the product-backlog skill.
// Escalating a grown queued attempt into admission rechecks the same holders.
import {
  parseBacklog,
  queueHeading,
  takenHeading,
} from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { occupiedAssignments } from "./agent-assignments.mjs";
import { selectedPreparation, show } from "./execution-source.mjs";
import { requireResolvedStoryDependencies } from "../../dough-product-backlog/scripts/product-backlog-story-dependencies.mjs";
import {
  backlogPath as trunkBacklogPath,
  isAncestor,
} from "./workspace-publication-ownership.mjs";

// The backlog entry naming `identity` at `rev`, and the profiles naming it.
async function holdingAt(cwd, rev, identity, backlogPath) {
  if (!rev) return { entry: undefined, profiles: [] };
  const backlog = await show(cwd, rev, backlogPath);
  const entry =
    backlog === null
      ? undefined
      : parseBacklog(backlog).entries.find(
          (item) => item.identity === identity,
        );
  const profiles = (await occupiedAssignments(cwd, rev, backlogPath)).filter(
    (profile) => profile.identity === identity,
  );
  return { entry, profiles };
}

// Why another owner holds the work, or undefined when none does.
function heldBy({ entry, profiles }) {
  if (entry?.list === takenHeading)
    return {
      holder: { list: takenHeading },
      error: "selected identity is already Taken on fetched trunk",
    };
  const [profile] = profiles;
  if (profile)
    return {
      holder: { agent: profile.agent, activity: profile.activity },
      error: `selected identity is held on fetched trunk by ${profile.agent} for ${profile.activity}`,
    };
  return undefined;
}

// The not-ready reasons the story's recorded preparation carries at `rev`,
// even when later edits call for reassessment; undefined when it records none.
async function notReadyReasons(cwd, rev, entry) {
  const selection = selectedPreparation(cwd, entry.href);
  const home = await show(cwd, rev, selection.homePath);
  if (home === null)
    throw new Error("selected canonical home is absent on fetched trunk");
  requireResolvedStoryDependencies(home, entry.href);
  const { assessment } = selection.read(home);
  const notReady =
    assessment.status === "not-ready" || assessment.recorded === "not-ready";
  return notReady ? assessment.reasons : undefined;
}

// Throws the refusal for work another owner holds on `rev`: a Taken entry or
// an agent profile (execution or preparation) naming it.
export async function requireUnheld(cwd, rev, identity, backlogPath) {
  const held = heldBy(await holdingAt(cwd, rev, identity, backlogPath));
  if (held) throw new Error(held.error);
}

// The start's check: throws the refusal for work that is Taken, held by a
// profile, or queued with a recorded not-ready reason. Unlisted work passes.
export async function requireOneShotStart(cwd, rev, identity, backlogPath) {
  const holding = await holdingAt(cwd, rev, identity, backlogPath);
  const held = heldBy(holding);
  if (held?.holder.list === takenHeading)
    throw new Error(`${held.error}; continue it under its existing claim`);
  if (held) throw new Error(`${held.error}; it is not one-shot work`);
  if (holding.entry?.list !== queueHeading) return;
  const reasons = await notReadyReasons(cwd, rev, holding.entry);
  if (reasons)
    throw new Error(
      `selected identity's recorded preparation is not-ready: ${reasons.join("; ")}`,
    );
}

// Managed delivery's `onFetchedTarget` for queued one-shot work: on each
// fetched target tip, before anything is rewritten or pushed, the stop for a
// tip where another owner holds the work, or where its entry has left the
// Backlog list other than through this candidate.
export function queuedOwnershipGuard({
  workspace,
  identity,
  backlogPath = trunkBacklogPath,
}) {
  const stop = ({ holder, error }) => ({
    status: "ownership-changed",
    fields: { ownership: { identity, ...holder }, error },
  });
  return async ({ candidate, remoteTip }) => {
    const holding = await holdingAt(
      workspace,
      remoteTip,
      identity,
      backlogPath,
    );
    const held = heldBy(holding);
    if (held) return stop(held);
    const queued =
      holding.entry?.list === queueHeading ||
      (remoteTip && (await isAncestor(workspace, candidate, remoteTip)));
    if (queued) return undefined;
    return stop({
      holder: {},
      error:
        "selected identity is no longer in the Backlog list on fetched trunk",
    });
  };
}
