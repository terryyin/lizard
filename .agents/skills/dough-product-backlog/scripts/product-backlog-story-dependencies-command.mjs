// Filesystem boundary: validate both canonical endpoints before replacing the
// consumer record under the same per-file lock used by preparation writers.
import { dirname, resolve, sep } from "node:path";
import { namedIdentity, readHome } from "./product-backlog-home-reader.mjs";
import { splitHref } from "./product-backlog-identity.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { applyToFile, readFile } from "./product-backlog-store.mjs";
import {
  normalizeStoryDependency,
  readStoryDependencies,
  updateStoryDependency,
} from "./product-backlog-story-dependencies.mjs";

export function pathFor(file, href) {
  const directory = dirname(file);
  const project = resolve(directory, "..");
  const path = resolve(directory, splitHref(href).path);
  if (!path.startsWith(project + sep))
    throw new BacklogError(
      `Canonical dependency home escapes the project: ${href}`,
    );
  return path;
}

export async function updateDependency(file, values) {
  requireField(values.identity, "identity");
  requireField(values.link, "link");
  requireField(values["dependency-file"], "dependency-file");
  requireField(values["expect-dependencies"], "expect-dependencies");
  let payload;
  try {
    payload = JSON.parse(
      readFile(
        resolve(values["dependency-file"]),
        "Dependency input file not found.",
      ),
    );
  } catch (error) {
    if (error instanceof BacklogError) throw error;
    throw new BacklogError(
      "Dependency input must be one valid JSON object. Nothing was written.",
    );
  }
  const dependency = normalizeStoryDependency(payload, values.identity);
  const supplierPath = pathFor(file, dependency.supplier.href);
  const validateSupplier = () => {
    const source = readFile(
      supplierPath,
      `Supplier canonical home not found: ${dependency.supplier.href}`,
    );
    if (
      namedIdentity(readHome(source, dependency.supplier.href)) !==
      dependency.supplier.identity
    )
      throw new BacklogError(
        "Supplier identity disagrees with its canonical home. Nothing was written.",
      );
  };
  const path = pathFor(file, values.link);
  let outcome;
  await applyToFile(
    path,
    (source) => {
      validateSupplier();
      outcome = updateStoryDependency(source, {
        identity: values.identity,
        href: values.link,
        dependency,
        expectedBasis: values["expect-dependencies"],
      });
      return outcome.source;
    },
    `Consumer canonical home not found: ${values.link}`,
  );
  console.log(JSON.stringify(outcome.state, null, 2));
}

export async function readDependencies(file, values) {
  requireField(values.link, "link");
  const source = readFile(
    pathFor(file, values.link),
    `Consumer canonical home not found: ${values.link}`,
  );
  console.log(
    JSON.stringify(readStoryDependencies(source, values.link), null, 2),
  );
}
