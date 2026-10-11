// Admission source: an accepted mission that neither backlog list holds yet,
// or a queued story whose one-shot attempt grew and carries its edits into
// admission. Its canonical story, and a plan that story declares, may still be
// drafts in the originating checkout supplied as the integration checkout;
// admission carries only that owned content, reconciled with fetched trunk
// (execution-admission-reconcile.mjs).
// Preparation is read as recorded; admission never records readiness or
// approach of its own.
import {
  parseBacklog,
  queueHeading,
  requireUnlistedHome,
} from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { sameDocument } from "../../dough-product-backlog/scripts/product-backlog-plan.mjs";
import { BacklogError } from "../../dough-product-backlog/scripts/product-backlog-refusal.mjs";
import { readStoryPurpose } from "../../dough-product-backlog/scripts/product-backlog-story-purpose.mjs";
import { requireResolvedStoryDependencies } from "../../dough-product-backlog/scripts/product-backlog-story-dependencies.mjs";
import {
  draftsOf,
  reconcilePlan,
  reconcileStory,
  refused,
  versionsOf,
} from "./execution-admission-reconcile.mjs";
import { sectionOf, selectedPreparation, show } from "./execution-source.mjs";
import { requireUnheld } from "./one-shot-ownership.mjs";
import { backlogPath } from "./workspace-publication-ownership.mjs";

// The selected story section must name the identity being admitted.
function requireIdentity(state, identity) {
  if (state.identity !== identity)
    throw refused(
      `${state.key} names identity "${state.identity}", not "${identity}"`,
    );
}

// The minimal content an admitted story needs: its own identity, recorded
// preparation facts and a recorded Goal the dashboard can show. `state` is
// the preparation read from `source`.
function requireAdmissibleStory(state, source, href, identity) {
  requireIdentity(state, identity);
  if (state.status !== "recorded")
    throw refused(
      `${state.key} has no recorded preparation facts; record them first`,
    );
  if (readStoryPurpose(source, href).status !== "recorded")
    throw refused(`${state.key} has no recorded Goal`);
  return state;
}

// The drafted content admission carries, as the claim writes it: the files
// that differ from fetched trunk.
const changedFiles = (...pairs) =>
  pairs
    .filter(([file, content]) => file && content !== file.trunk)
    .map(([file, content]) => ({ path: file.path, content }));

// A queued story whose one-shot attempt grew: its existing entry moves to
// Taken without a readiness assessment, carrying any drafted edit of its
// story section and declared plan. Another holder, such as a preparation
// profile, still refuses it.
async function readQueuedAdmission(request, remoteRef, candidateSha, entry) {
  const { repository, identity, link } = request;
  if (link !== entry.href)
    throw refused(`selected identity is queued at ${entry.href}, not ${link}`);
  await requireUnheld(repository, remoteRef, identity, backlogPath);
  const selection = selectedPreparation(repository, entry.href);
  const drafts = await draftsOf(request, remoteRef, candidateSha);
  const home = await versionsOf(request, remoteRef, drafts, selection.homePath);
  if (home.trunk === null)
    throw refused(`selected canonical home ${selection.homePath} is absent`);
  requireResolvedStoryDependencies(home.trunk, link);
  const homeSource = reconcileStory(home, link);
  requireResolvedStoryDependencies(homeSource, link);
  const state = selection.read(homeSource);
  requireIdentity(state, identity);
  const declared = selection.declaredPlan(state.approach ?? {});
  const plan = declared.planTarget
    ? await versionsOf(request, remoteRef, drafts, declared.planPath)
    : undefined;
  if (plan?.draft === null)
    throw refused(`declared plan ${declared.planPath} is absent`);
  const planSource = plan && reconcilePlan(plan);
  return {
    homePath: selection.homePath,
    planPath: declared.planPath,
    planTarget: entry.plan?.target ?? declared.planTarget,
    selectedSource: sectionOf(homeSource, link).text,
    planSource,
    admission: {
      title: entry.title,
      href: link,
      queued: true,
      files: changedFiles([home, homeSource], [plan, planSource]),
    },
  };
}

// With `candidateSha`, the admission that preserved claim candidate carries.
// Once the work is listed, only the listing and its plan, whose owner the
// claim's provenance decides.
export async function readAdmissionSource(request, remoteRef, candidateSha) {
  const { repository, identity, link } = request;
  const backlog = await show(repository, remoteRef, backlogPath);
  if (backlog === null) throw refused("fetched trunk has no product backlog");
  const document = parseBacklog(backlog);
  const entry = document.entries.find((item) => item.identity === identity);
  if (entry?.list === queueHeading) {
    if (request.carry === true)
      return readQueuedAdmission(request, remoteRef, candidateSha, entry);
    throw refused(
      "selected identity is already queued on fetched trunk; start it as queued work",
    );
  }
  if (entry) return { existing: entry, planTarget: entry.plan?.target };
  // The work is unlisted; its home must be too, as the Take admitting it will
  // require. Refusing here leaves no workspace behind.
  try {
    requireUnlistedHome(document, link);
  } catch (error) {
    if (error instanceof BacklogError) throw refused(error.message);
    throw error;
  }
  const selection = selectedPreparation(repository, link);
  const { homePath } = selection;
  const drafts = await draftsOf(request, remoteRef, candidateSha);
  const home = await versionsOf(request, remoteRef, drafts, homePath);
  if (home.draft === null)
    throw refused(`selected canonical home ${homePath} is absent`);
  // Neither a carried draft nor an old retained candidate may hide a newly
  // published prerequisite on this unlisted canonical story.
  if (sectionOf(home.trunk, link))
    requireResolvedStoryDependencies(home.trunk, link);
  const drafted = requireAdmissibleStory(
    selection.read(home.draft),
    home.draft,
    link,
    identity,
  );
  const declaredPlan = selection.declaredPlan(drafted.approach);
  const { planPath, planHref, planTarget, planIsCanonical } = declaredPlan;
  let plan;
  if (drafted.approach.kind === "planned") {
    // A requested section of the declared plan requests that plan.
    if (request.plan && !sameDocument(request.plan, planHref))
      throw refused("requested plan disagrees with recorded preparation");
    if (!planIsCanonical) {
      plan = await versionsOf(request, remoteRef, drafts, planPath);
      if (plan.draft === null)
        throw refused(`declared plan ${planPath} is absent`);
    }
  } else if (request.plan) {
    throw refused(`a ${drafted.approach.kind} story links no plan`);
  }
  const homeSource = reconcileStory(home, link);
  requireResolvedStoryDependencies(homeSource, link);
  const planSource = plan && reconcilePlan(plan);
  const preparation = requireAdmissibleStory(
    selection.read(homeSource, declaredPlan, planSource),
    homeSource,
    link,
    identity,
  );
  const files = changedFiles([home, homeSource], [plan, planSource]);
  return {
    homePath,
    planPath,
    planTarget,
    preparation,
    selectedSource: sectionOf(homeSource, link).text,
    planSource,
    admission: { title: request.title, href: link, files },
  };
}
