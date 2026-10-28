// Discover live canonical homes only; Git history is supplier evidence, never
// a source of consumers. Reuse the home reader for section and identity meaning.
import { readdirSync, readFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { parseBacklog } from "./product-backlog-document.mjs";
import {
  contains,
  fetchAcceptedTarget,
} from "./product-backlog-accepted-integration.mjs";
import {
  git,
  gitLine,
  repositoryRoot,
} from "./product-backlog-git-repository.mjs";
import {
  discoveryCandidates,
  namedIdentity,
  readHome,
} from "./product-backlog-home-reader.mjs";
import { declaredPlanTarget } from "./product-backlog-story-state-home.mjs";
import { readStoryState } from "./product-backlog-story-state.mjs";
import { splitHref } from "./product-backlog-identity.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { pathFor } from "./product-backlog-story-dependencies-command.mjs";
import { readStoryDependencies } from "./product-backlog-story-dependencies.mjs";

function markdownFiles(directory, problems) {
  const files = [];
  try {
    for (const entry of readdirSync(directory, { withFileTypes: true })) {
      const path = resolve(directory, entry.name);
      if (entry.isDirectory()) files.push(...markdownFiles(path, problems));
      else if (entry.isFile() && entry.name.endsWith(".md")) files.push(path);
    }
  } catch (error) {
    problems.push({ path: directory, problem: error.message });
  }
  return files;
}

export function discoverConsumers(file, supplierIdentity) {
  requireField(supplierIdentity, "supplier-identity");
  const directory = dirname(file);
  const problems = [];
  const candidates = new Set();
  const listed = new Map();
  const separatePlans = new Set();
  for (const entry of parseBacklog(readFileSync(file, "utf8")).entries) {
    candidates.add(entry.href);
    listed.set(entry.href, entry.identity);
    if (
      entry.plan &&
      splitHref(entry.plan.target).path !== splitHref(entry.href).path
    )
      separatePlans.add(entry.plan.target);
  }
  for (const path of markdownFiles(directory, problems)) {
    let source;
    try {
      source = readFileSync(path, "utf8");
    } catch (error) {
      problems.push({ path, problem: error.message });
      continue;
    }
    const href = relative(directory, path).split("\\").join("/");
    const { anchors, wholeDocument, orphanedRecord } =
      discoveryCandidates(source);
    if (anchors.length) {
      for (const anchor of anchors) candidates.add(`${href}#${anchor}`);
    } else if (wholeDocument) candidates.add(href);
    else if (orphanedRecord)
      problems.push({
        path: href,
        problem: `${orphanedRecord} record has no canonical home.`,
      });
  }
  const homes = [];
  for (const href of candidates) {
    try {
      const source = readFileSync(pathFor(file, href), "utf8");
      const identity = namedIdentity(readHome(source, href));
      if (!identity) continue;
      if (listed.has(href) && listed.get(href) !== identity)
        throw new BacklogError(
          "Listed identity disagrees with its canonical home.",
        );
      const preparation = readStoryState(source, href);
      const plan = declaredPlanTarget(
        directory,
        href,
        preparation.approach?.kind,
        preparation.approach?.plan,
      );
      if (plan) separatePlans.add(plan);
      homes.push({ identity, href, ...readStoryDependencies(source, href) });
    } catch (error) {
      problems.push({
        path: splitHref(href).path,
        href,
        problem: error.message,
      });
    }
  }
  const canonical = homes.filter((home) => !separatePlans.has(home.href));
  const duplicates = new Set(
    canonical
      .filter((home) =>
        canonical.some(
          (other) => other !== home && other.identity === home.identity,
        ),
      )
      .map((home) => home.identity),
  );
  for (const identity of duplicates)
    problems.push({
      identity,
      problem: "Multiple current canonical homes name this identity.",
    });
  return {
    supplierIdentity,
    consumers: canonical
      .filter((home) => !duplicates.has(home.identity))
      .flatMap((home) =>
        home.dependencies
          .filter((entry) => entry.supplier.identity === supplierIdentity)
          .map((dependency) => ({
            identity: home.identity,
            href: home.href,
            basis: home.basis,
            dependency,
          })),
      ),
    problems,
  };
}

// A visit after accepted supplier integration discovers from a checkout that
// holds the fetched target, never from the branch a publisher returned to.
// A clean checkout behind the target fast-forwards; local changes or a
// divergent history stop with nothing changed. It never resets or switches.
// An input it refuses stays a refusal; any other failure is a stopped visit,
// never a thrown one, since the accepted integration it visits still stands.
export function visitCurrentConsumers(file, values) {
  try {
    return visitFromCurrentCheckout(file, values);
  } catch (error) {
    if (error instanceof BacklogError) throw error;
    return {
      status: "stopped",
      reason: "error",
      error: error.message,
      recovery:
        "Clear the reported failure, then run product-backlog.mjs visit-consumers with this accepted revision.",
    };
  }
}

function visitFromCurrentCheckout(file, values) {
  const supplierIdentity = values["supplier-identity"];
  requireField(supplierIdentity, "supplier-identity");
  const root = repositoryRoot(dirname(file));
  const { accepted, target } = fetchAcceptedTarget(values, root);
  const head = gitLine(["rev-parse", "HEAD"], root);
  const stop = (reason, recovery, fields = {}) => ({
    status: "stopped",
    reason,
    supplierIdentity,
    acceptedRevision: accepted,
    target,
    checkout: { path: root, head, ...fields },
    recovery,
  });
  if (!contains(accepted, target.sha, root))
    return stop(
      "accepted-not-on-target",
      "The fetched target does not contain the accepted supplier integration. Establish the accepted integration receipt before visiting consumers.",
    );
  const changes = git(["status", "--porcelain"], root)
    .split("\n")
    .filter(Boolean);
  if (changes.length)
    return stop(
      "local-changes",
      "Commit or deliberately retain these changes in the owned checkout, then run this visit again. Nothing was changed.",
      { changes },
    );
  let relation = "current";
  if (head !== target.sha) {
    if (contains(head, target.sha, root)) {
      git(["merge", "--ff-only", "--quiet", target.sha], root);
      relation = "fast-forwarded";
    } else if (contains(target.sha, head, root)) relation = "ahead";
    else
      return stop(
        "divergent",
        "The owned checkout and the fetched target diverged. Publish or reconcile its commits through the caller's publication workflow, then run this visit again. Nothing was changed.",
      );
  }
  return {
    status: "visited",
    supplierIdentity,
    acceptedRevision: accepted,
    target,
    checkout: {
      path: root,
      relation,
      from: head,
      revision: gitLine(["rev-parse", "HEAD"], root),
    },
    ...discoverConsumers(file, supplierIdentity),
  };
}

export async function visitConsumers(file, values) {
  const visit = visitCurrentConsumers(file, values);
  console.log(JSON.stringify(visit, null, 2));
  if (visit.status !== "visited") process.exitCode = 1;
}

export async function readConsumers(file, values) {
  console.log(
    JSON.stringify(
      discoverConsumers(file, values["supplier-identity"]),
      null,
      2,
    ),
  );
}
