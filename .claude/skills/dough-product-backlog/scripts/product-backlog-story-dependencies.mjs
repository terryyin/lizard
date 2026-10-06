// Pure interpretation of exceptional prerequisites, stored in the consumer's
// canonical home. Preparation judgment and dependencies remain separate facts.
import { namedIdentity, readHome } from "./product-backlog-home-reader.mjs";
import { splitHref } from "./product-backlog-identity.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { joinSource } from "./product-backlog-source.mjs";
import { digestSource } from "./product-backlog-story-state-basis.mjs";

export const storyDependenciesFence = "json dough-story-dependencies";
export const storyDependenciesSchemaVersion = 1;

function refuse(message) {
  throw new BacklogError(`Story dependencies: ${message}`);
}

function text(value, field) {
  if (typeof value !== "string" || value.trim() === "")
    refuse(`${field} must be nonempty text.`);
  return value;
}

// Locator spelling is backlog-relative, shared by the consumer and supplier.
// Loading and validating the endpoint itself belongs to the command boundary.
function supplierOf(value) {
  if (!value || typeof value !== "object" || Array.isArray(value))
    refuse("supplier must name an identity and href.");
  const identity = text(value.identity, "supplier.identity");
  const href = text(value.href, "supplier.href");
  const { path } = splitHref(href);
  if (!path || /^(?:[a-z]+:|\/)/i.test(path))
    refuse("supplier.href must name a project canonical home.");
  return { identity, href };
}

export function normalizeStoryDependency(value, consumerIdentity) {
  if (!value || typeof value !== "object" || Array.isArray(value))
    refuse("each dependency must be one object.");
  const supplier = supplierOf(value.supplier);
  if (supplier.identity === consumerIdentity)
    refuse("a story cannot depend on itself.");
  if (!["waiting", "satisfied", "decision-needed"].includes(value.state))
    refuse("state must be waiting, satisfied, or decision-needed.");
  const dependency = {
    supplier,
    implementation: text(value.implementation, "implementation"),
    rationale: text(value.rationale, "rationale"),
    condition: text(value.condition, "condition"),
    state: value.state,
  };
  if (value.resolution !== undefined) {
    if (
      !value.resolution ||
      typeof value.resolution !== "object" ||
      Array.isArray(value.resolution)
    )
      refuse("resolution must name a recoverable revision, path, and summary.");
    const revision = text(value.resolution.revision, "resolution.revision");
    if (!/^[0-9a-f]{40,64}$/.test(revision))
      refuse("resolution.revision must be a full recoverable Git revision.");
    dependency.resolution = {
      revision,
      path: text(value.resolution.path, "resolution.path"),
      summary: text(value.resolution.summary, "resolution.summary"),
    };
  }
  if (value.state === "satisfied" && dependency.resolution === undefined)
    refuse("a satisfied dependency requires resolution evidence.");
  if (value.decision !== undefined)
    dependency.decision = text(value.decision, "decision");
  if (value.state === "decision-needed" && dependency.decision === undefined)
    refuse(
      "a decision-needed dependency requires the actual decision question.",
    );
  return dependency;
}

function blocksOf(home) {
  const { lines } = home.document;
  const blocks = [];
  for (let index = home.region.start; index < home.region.end; index += 1) {
    if (!lines[index].trim().startsWith("```json dough-story-dependencies"))
      continue;
    if (lines[index].trim() !== `\`\`\`${storyDependenciesFence}`)
      refuse(`malformed dependency fence at line ${index + 1}.`);
    const open = index;
    while (++index < home.region.end && lines[index].trim() !== "```") {
      if (lines[index].trim().startsWith("```"))
        refuse(`dependency fence at line ${open + 1} is not closed.`);
    }
    if (index === home.region.end)
      refuse(
        `dependency fence at line ${open + 1} is not closed inside the selected home.`,
      );
    blocks.push({
      open,
      close: index,
      body: lines.slice(open + 1, index).join("\n"),
    });
  }
  if (blocks.length > 1)
    refuse(
      `${home.key} holds multiple dependency blocks; a human decides which to keep.`,
    );
  return blocks;
}

function readPayload(block, identity) {
  let payload;
  try {
    payload = JSON.parse(block.body);
  } catch {
    refuse("present dependency block is not valid JSON.");
  }
  if (!payload || Array.isArray(payload) || typeof payload !== "object")
    refuse("dependency block must be one JSON object.");
  if (payload.schemaVersion !== storyDependenciesSchemaVersion)
    refuse(
      `unsupported dependency schema version ${JSON.stringify(payload.schemaVersion)}.`,
    );
  if (payload.identity !== identity)
    refuse("dependency record identity disagrees with its canonical home.");
  if (!Array.isArray(payload.dependencies))
    refuse("present dependency record must carry a dependencies array.");
  const dependencies = payload.dependencies.map((entry) =>
    normalizeStoryDependency(entry, identity),
  );
  if (
    new Set(dependencies.map((entry) => entry.supplier.identity)).size !==
    dependencies.length
  )
    refuse("a supplier identity occurs more than once.");
  return dependencies;
}

export function readStoryDependencies(source, href) {
  const home = readHome(source, href);
  const identity = namedIdentity(home);
  const [block] = blocksOf(home);
  const dependencies = block ? readPayload(block, identity) : [];
  return {
    status: block ? "recorded" : "not-recorded",
    identity,
    href,
    dependencies,
    blocking: dependencies.filter((entry) => entry.state !== "satisfied"),
    basis: digestSource(JSON.stringify({ identity, dependencies })),
    source: {
      path: home.relative,
      href,
      ...(block ? { startLine: block.open + 1, endLine: block.close + 1 } : {}),
    },
  };
}

// Upserts one explicitly decided supplier agreement and leaves every other
// dependency, preparation block, sibling section, and prose byte in place.
export function updateStoryDependency(source, request) {
  const home = readHome(source, request.href);
  const current = readStoryDependencies(source, request.href);
  if (!current.identity || request.identity !== current.identity)
    refuse("consumer identity disagrees with its canonical home.");
  if (!request.expectedBasis || request.expectedBasis !== current.basis)
    refuse(
      "dependency agreement changed since it was read; reread before updating. Nothing was written.",
    );
  const dependency = normalizeStoryDependency(
    request.dependency,
    current.identity,
  );
  const dependencies = [...current.dependencies];
  const index = dependencies.findIndex(
    (entry) => entry.supplier.identity === dependency.supplier.identity,
  );
  if (index === -1) dependencies.push(dependency);
  else dependencies[index] = dependency;
  const payload = {
    schemaVersion: storyDependenciesSchemaVersion,
    identity: current.identity,
    dependencies,
  };
  const written = [
    `\`\`\`${storyDependenciesFence}`,
    JSON.stringify(payload),
    "```",
  ];
  const lines = [...home.document.lines];
  const [block] = blocksOf(home);
  if (block) lines.splice(block.open, block.close - block.open + 1, ...written);
  else
    lines.splice(
      (home.recorded?.index ?? home.region.heading) + 1,
      0,
      ...written,
    );
  const next = joinSource({ ...home.document, lines });
  return { source: next, state: readStoryDependencies(next, request.href) };
}

export function requireResolvedStoryDependencies(source, href) {
  const state = readStoryDependencies(source, href);
  if (state.blocking.length > 0)
    refuse(
      `execution is blocked by ${state.blocking.map((entry) => `${entry.supplier.identity} (${entry.supplier.href}): ${entry.rationale}; completion condition: ${entry.condition}${entry.decision ? `; decision: ${entry.decision}` : ""}`).join("; ")}`,
    );
  return state;
}

// Comparison only for an already-published execution's retained claim. A new
// start still compares the full source, including the dependency agreement.
export function withoutStoryDependencies(source, href) {
  const home = readHome(source, href);
  const lines = [...home.document.lines];
  for (const block of blocksOf(home).reverse())
    lines.splice(block.open, block.close - block.open + 1);
  return joinSource({ ...home.document, lines });
}
