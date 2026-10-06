// Records an agent's evidenced consumer judgment, never infers it
// from prose or from the disappearance of a supplier's active home.
import { resolve } from "node:path";
import { discoverConsumers } from "./product-backlog-dependency-consumers.mjs";
import { supplierCompletion } from "./product-backlog-dependency-completion.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { applyToFile, readFile } from "./product-backlog-store.mjs";
import { pathFor } from "./product-backlog-story-dependencies-command.mjs";
import {
  normalizeStoryDependency,
  readStoryDependencies,
  updateStoryDependency,
} from "./product-backlog-story-dependencies.mjs";

function agreement(entry) {
  const { supplier, implementation, rationale, condition } = entry;
  return JSON.stringify({ supplier, implementation, rationale, condition });
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
      if (
        current.basis !== values["expect-dependencies"] ||
        !held ||
        agreement(held) !== agreement(dependency)
      )
        throw new BacklogError(
          "Consumer agreement changed since it was read. Reread its condition before resolving. Nothing was written.",
        );
      // A repeat visit preserves the first resolution byte for byte.
      if (held.state === "satisfied") {
        outcome = current;
        return source;
      }
      const written = updateStoryDependency(source, {
        identity: values.identity,
        href: values.link,
        expectedBasis: current.basis,
        dependency: { ...dependency, resolution },
      });
      outcome = written.state;
      return written.source;
    },
    "Consumer canonical home not found.",
  );
  console.log(JSON.stringify(outcome, null, 2));
}
