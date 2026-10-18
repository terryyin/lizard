// Published queued or continued Taken source, and the selected story's section
// and declared plan shared with admission.
import { dirname, posix, relative, resolve, sep } from "node:path";
import {
  parseBacklog,
  queueHeading,
  takenHeading,
} from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { readHome } from "../../dough-product-backlog/scripts/product-backlog-home-reader.mjs";
import { splitHref } from "../../dough-product-backlog/scripts/product-backlog-identity.mjs";
import {
  planFileOf,
  sameDocument,
} from "../../dough-product-backlog/scripts/product-backlog-plan.mjs";
import { readStoryState } from "../../dough-product-backlog/scripts/product-backlog-story-state.mjs";
import { BacklogError } from "../../dough-product-backlog/scripts/product-backlog-refusal.mjs";
import { deriveStartEligibility } from "../../dough-product-backlog/scripts/product-backlog-start-eligibility.mjs";
import {
  blockedByDependencies,
  readStoryDependencies,
  withoutStoryDependencies,
} from "../../dough-product-backlog/scripts/product-backlog-story-dependencies.mjs";
import { git } from "./publication-git.mjs";
import {
  backlogPath,
  claimProvenance,
  classifyOwnership,
  isAncestor,
} from "./workspace-publication-ownership.mjs";

export async function show(cwd, rev, path) {
  try {
    return (await git(cwd, "show", `${rev}:${path}`)).stdout;
  } catch {
    return null;
  }
}

function within(root, path) {
  const project = resolve(root);
  const absolute = resolve(project, path);
  if (absolute !== project && !absolute.startsWith(project + sep))
    throw new Error(`source path escapes project: ${path}`);
  return relative(project, absolute).split(sep).join("/");
}

// The selected story's section of `source`, or undefined when `source` has
// none: its document, its region, and its text without the separator lines a
// following sibling section may add. Whitespace within the section, including
// trailing spaces on text, is kept.
export function sectionOf(source, href) {
  if (source === null) return undefined;
  let home;
  try {
    home = readHome(source, href);
  } catch (error) {
    if (error instanceof BacklogError) return undefined;
    throw error;
  }
  const { document, region } = home;
  const lines = document.lines.slice(region.start, region.end);
  while (lines.at(-1) === "") lines.pop();
  return { document, region, text: lines.join("\n") };
}

// Where the selected story's preparation lives and what it declares. The
// canonical home's project path follows its backlog link; `declaredPlan`
// resolves the plan a recorded approach names, relative to that home, and the
// file `planPath` that plan link names. A plan naming the home's own document
// is canonical: it is read and digested as the home, and a Taken entry never
// links it. Any other plan is its own file, linked by `planTarget`. `read`
// reads the preparation recorded in a home's text, digesting the declared
// plan's `planSource` when one is given.
export function selectedPreparation(repository, href) {
  const homePath = within(
    repository,
    posix.join(dirname(backlogPath), splitHref(href).path),
  );
  return {
    homePath,
    declaredPlan(approach) {
      if (approach.kind !== "planned") return {};
      const declared = within(
        repository,
        posix.join(dirname(homePath), approach.plan),
      );
      const planPath = planFileOf(declared);
      const planHref = posix.relative(dirname(backlogPath), declared);
      if (sameDocument(planHref, href))
        return { planPath, planHref, planIsCanonical: true };
      return { planPath, planHref, planTarget: planHref };
    },
    read(source, { planIsCanonical } = {}, planSource) {
      return readStoryState(source, href, { planIsCanonical, planSource });
    },
  };
}

// A reader's refusal is a fact the derivation names as a gap.
function readFact(read) {
  try {
    return read();
  } catch (error) {
    if (error instanceof BacklogError)
      return { status: "unavailable", problem: error.message };
    throw error;
  }
}

// An investigation's unselected approach is why its assessment blocks.
function describeBlocker(blocker, approach) {
  if (blocker.kind === "assessment")
    return `${approach?.kind === "unselected" ? "published approach is unselected; " : ""}published preparation is ${blocker.status}`;
  return `published ${blocker.fact} ${blocker.fact === "dependencies" ? "are" : "is"} unavailable: ${blocker.problem}`;
}

// One refusal naming every blocker: the unsatisfied dependencies together,
// then the assessment or a gap.
function startRefusal({ blockers }, dependencies, approach) {
  return [
    ...(blockers.some((entry) => entry.kind === "dependency")
      ? [blockedByDependencies(dependencies.blocking)]
      : []),
    ...blockers
      .filter((entry) => entry.kind !== "dependency")
      .map((entry) => describeBlocker(entry, approach)),
  ].join("; and ");
}

export async function readPublishedExecutionSource(request, remoteRef) {
  const backlog = await show(request.repository, remoteRef, backlogPath);
  if (backlog === null) throw new Error("fetched trunk has no product backlog");
  const entry = parseBacklog(backlog).entries.find(
    (item) => item.identity === request.identity,
  );
  if (!entry || (entry.list !== queueHeading && entry.list !== takenHeading))
    throw new Error(
      "selected identity is not queued on fetched trunk; admit accepted work that no backlog list holds with --admit",
    );
  // Outside claim recovery, Taken work is a continuation: only the claim's
  // own publisher continues it, and only from ready published preparation.
  let claim;
  if (entry.list === takenHeading) {
    claim = await claimProvenance(
      request.repository,
      remoteRef,
      request.identity,
      backlogPath,
    );
    if (
      !request.retained &&
      (!claim?.publisher || claim.publisher !== request.publisherId)
    )
      return { existing: entry, claim };
  }
  const selection = selectedPreparation(request.repository, entry.href);
  const { homePath } = selection;
  const home = await show(request.repository, remoteRef, homePath);
  if (home === null)
    throw new Error("selected canonical home is absent on fetched trunk");
  const publishedClaimOwned =
    request.retained &&
    classifyOwnership({
      publisherId: request.publisherId,
      candidateSha: request.retained.candidateSha,
      candidateIsAncestor: await isAncestor(
        request.repository,
        request.retained.candidateSha,
        remoteRef,
      ),
      provenance: claim,
    }) === "owned";
  // A new Take (including publication retries) must use the fetched dependency
  // agreement. Already-owned Taken continuation does not interrupt execution,
  // so its dependencies are not consulted.
  const dependencyGated =
    !publishedClaimOwned && (entry.list === queueHeading || request.retained);
  const dependencies = dependencyGated
    ? readFact(() => readStoryDependencies(home, entry.href))
    : { status: "not-recorded", blocking: [] };
  const preview = readFact(() => selection.read(home));
  if (preview.status !== "unavailable" && preview.identity !== request.identity)
    throw new Error("selected canonical preparation or identity is unresolved");
  const eligibility = deriveStartEligibility({
    preparation: preview,
    dependencies,
  });
  if (eligibility.status !== "eligible")
    throw new Error(startRefusal(eligibility, dependencies, preview.approach));
  const declaredPlan = selection.declaredPlan(preview.approach);
  const { planPath, planHref, planTarget, planIsCanonical } = declaredPlan;
  let plan;
  if (preview.approach.kind === "planned") {
    if (!planIsCanonical) {
      plan = await show(request.repository, remoteRef, planPath);
      if (plan === null) throw new Error("published plan is absent");
    }
    // A link to a section of the declared plan links that plan.
    if (
      entry.plan &&
      (planTarget === undefined || !sameDocument(entry.plan.target, planTarget))
    )
      throw new Error(
        `${entry.list === takenHeading ? "Taken" : "queued"} plan link ` +
          `disagrees with preparation`,
      );
    // Continuation writes nothing: the recorder links a Taken entry's plan.
    if (claim && planTarget && !entry.plan)
      throw new Error(
        `Taken entry does not link the plan ${planTarget} its published ` +
          `preparation declares; record the planned approach again with ` +
          `record-state and publish it`,
      );
    // A requested section of the declared plan requests that plan.
    if (request.plan && !sameDocument(request.plan, planHref))
      throw new Error("requested plan disagrees with published preparation");
  } else if (preview.approach.kind === "unselected") {
    throw new Error("published approach is unselected");
  } else if (
    preview.approach.kind !== "planless" ||
    request.plan ||
    entry.plan
  ) {
    throw new Error("queued planless authority or plan link is inconsistent");
  }
  const preparation = selection.read(home, declaredPlan, plan);
  return {
    ...(claim ? { existing: entry, claim } : {}),
    entry,
    homePath,
    planPath,
    planTarget,
    preparation,
    selectedSource: sectionOf(home, entry.href).text,
    selectedSourceWithoutDependencies: sectionOf(
      withoutStoryDependencies(home, entry.href),
      entry.href,
    ).text,
    publishedClaimOwned: Boolean(publishedClaimOwned),
    planSource: plan,
  };
}
