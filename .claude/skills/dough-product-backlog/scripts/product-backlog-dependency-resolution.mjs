// Records an agent's evidenced consumer judgment, never infers it
// from prose or from the disappearance of a supplier's active home.
import { resolve } from "node:path";
import { discoverConsumers } from "./product-backlog-dependency-consumers.mjs";
import { supplierCompletion } from "./product-backlog-dependency-completion.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { applyToFile, readFile } from "./product-backlog-store.mjs";
import { pathFor } from "./product-backlog-story-dependencies-command.mjs";
import { dependencyAgreement } from "./product-backlog-story-state-basis.mjs";
import {
  normalizeStoryDependency,
  readStoryDependencies,
  updateStoryDependency,
} from "./product-backlog-story-dependencies.mjs";

function agreement(entry) {
  return JSON.stringify(dependencyAgreement(entry));
}

function correctedResolution(held, resolution) {
  if (!held.resolution) return resolution;
  // Keep the original recoverable locator and accepted history. The new
  // judgment and its separately validated evidence stay in this same record.
  return {
    ...held.resolution,
    summary: `${held.resolution.summary}\nAgreement correction using ${resolution.revision}:${resolution.path}:\n${resolution.summary}`,
  };
}

export async function resolveDependency(file, values) {
  for (const field of [
    "identity",
    "link",
    "dependency-file",
    "expect-dependencies",
  ])
    requireField(values[field], field);
  let payload;
  try {
    payload = JSON.parse(
      readFile(
        resolve(values["dependency-file"]),
        "Dependency input file not found.",
      ),
    );
  } catch (error) {
    throw new BacklogError(
      `Resolution input must be valid JSON: ${error.message}`,
    );
  }
  const dependency = normalizeStoryDependency(payload, values.identity);
  const correcting = values["correct-agreement"] === true;
  if (correcting && dependency.state !== "satisfied")
    throw new BacklogError(
      "Agreement correction requires an evidenced satisfied condition judgment. Retain the agreement until its revised condition is proved. Nothing was written.",
    );
  if (!dependency.resolution)
    throw new BacklogError(
      "resolve-dependency requires recoverable supplier outcome evidence, including for an unresolved consumer.",
    );
  const discovery = discoverConsumers(file, dependency.supplier.identity);
  const consumer = discovery.consumers.find(
    (entry) => entry.identity === values.identity && entry.href === values.link,
  );
  if (!consumer)
    throw new BacklogError(
      "Selected consumer relationship is missing, unreadable, or ambiguous. Nothing was written.",
    );
  const resolution = await supplierCompletion(file, dependency, values);
  let outcome;
  await applyToFile(
    pathFor(file, values.link),
    (source) => {
      const current = readStoryDependencies(source, values.link);
      const held = current.dependencies.find(
        (entry) => entry.supplier.identity === dependency.supplier.identity,
      );
      const changed = held && agreement(held) !== agreement(dependency);
      if (
        current.basis !== values["expect-dependencies"] ||
        !held ||
        (!correcting && changed)
      )
        throw new BacklogError(
          "Consumer agreement changed since it was read. Reread its condition before resolving. Nothing was written.",
        );
      if (held.supplier.href !== dependency.supplier.href)
        throw new BacklogError(
          "Agreement correction cannot retarget the existing supplier canonical home. Nothing was written.",
        );
      // A repeat visit preserves the first resolution byte for byte.
      if (held.state === "satisfied" && !changed) {
        outcome = current;
        return source;
      }
      const written = updateStoryDependency(source, {
        identity: values.identity,
        href: values.link,
        expectedBasis: current.basis,
        dependency: {
          ...dependency,
          resolution: changed
            ? correctedResolution(held, resolution)
            : resolution,
        },
      });
      outcome = written.state;
      return written.source;
    },
    "Consumer canonical home not found.",
  );
  console.log(JSON.stringify(outcome, null, 2));
}
