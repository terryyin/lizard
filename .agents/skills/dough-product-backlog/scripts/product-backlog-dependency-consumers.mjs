// Discover live canonical homes only; Git history is supplier evidence, never
// a source of consumers. Reuse the home reader for section and identity meaning.
import { readdirSync, readFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { parseBacklog } from "./product-backlog-document.mjs";
import { namedIdentity, readHome } from "./product-backlog-home-reader.mjs";
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
    const anchors = [...source.matchAll(/^<a id="([^"\n]+)"><\/a> *$/gm)];
    if (anchors.length) {
      for (const anchor of anchors) candidates.add(`${href}#${anchor[1]}`);
    } else if (/^\*\*Identity:\*\*/m.test(source)) candidates.add(href);
    else if (source.includes("dough-story-dependencies"))
      problems.push({
        path: href,
        problem: "Dependency record has no canonical home.",
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

export async function readConsumers(file, values) {
  console.log(
    JSON.stringify(
      discoverConsumers(file, values["supplier-identity"]),
      null,
      2,
    ),
  );
}
