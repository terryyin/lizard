// Reads and records story state in canonical-home text shared by CLI and Dashboard.
import {
  namedIdentity,
  readHome,
  storyContextLines,
} from "./product-backlog-home-reader.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { joinSource } from "./product-backlog-source.mjs";
import {
  basesEqual,
  computeBasis,
} from "./product-backlog-story-state-basis.mjs";
import {
  findStoryStateBlocks,
  parseStoryStatePayload,
  storyStateBlockLines,
  storyStateSourceLocation,
} from "./product-backlog-story-state-block.mjs";
import {
  assessmentPayload,
  preparationPayload,
  readAssessmentPayload,
  readPreparationPayload,
  storyStateSchemaVersion,
} from "./product-backlog-story-state-schema.mjs";
export {
  computeBasis,
  digestSource,
} from "./product-backlog-story-state-basis.mjs";

function currentBasisFor(home, approach, options) {
  const source = joinSource({
    ...home.document,
    lines: storyContextLines(home),
  });
  const planSource =
    approach === "planned" && options.planIsCanonical !== true
      ? options.planSource
      : undefined;
  return computeBasis(source, planSource);
}

// Reads preparation facts and assessment view from already-loaded
// canonical-home text. Legacy absence and unsupported version stay distinct
// from recorded facts. Always returns the current content basis.
export function readStoryState(source, href, options = {}) {
  const home = readHome(source, href);
  const block = storyStateBlock(home, "");
  const reference = {
    identity: namedIdentity(home),
    href: home.href,
    key: home.key,
  };
  if (block === undefined) {
    return {
      status: "not-recorded",
      ...reference,
      source: { path: home.relative, href: home.href },
      basis: currentBasisFor(home, undefined, options),
      assessment: { status: "absent" },
    };
  }
  const payload = readBlockPayload(home, block);
  const preparation = readPreparationPayload(payload);
  const assessment =
    preparation.status === "recorded"
      ? readAssessmentPayload(payload)
      : { status: "absent" };
  const basis = currentBasisFor(home, preparation.approach?.kind, options);
  return {
    ...preparation,
    source: storyStateSourceLocation(home, block.open, block.close),
    ...reference,
    basis,
    assessment:
      assessment.status === "absent"
        ? assessment
        : {
            ...assessment,
            changedSinceReview: !basesEqual(assessment.basis, basis),
          },
  };
}

// Applies one preparation and optional assessment record to already-loaded
// canonical-home text, replacing only the selected story's state block.
// Leaves other stories and prose intact. A stale expected basis, duplicate
// blocks, unsupported existing schema, or identity mismatch leaves the
// candidate untouched.
export function recordStoryState(source, request, options = {}) {
  requireField(request.href, "link");
  const home = readHome(source, request.href);
  requirePreparationIdentity(home, request.identity);
  const payload = preparationPayload(request);
  const block = storyStateBlock(home, " Nothing was written.");
  if (block) {
    const existing = readBlockPayload(home, block);
    if (existing.schemaVersion !== storyStateSchemaVersion) {
      throw new BacklogError(
        `${home.key} already holds story-state schema version ` +
          `${JSON.stringify(existing.schemaVersion)}, which this recorder ` +
          `does not support. Nothing was written.`,
      );
    }
  }

  const basis = currentBasisFor(home, payload.approach, options);
  Object.assign(
    payload,
    assessmentPayload(request, basis, options.planIsCanonical === true),
  );

  const lines = [...home.document.lines];
  const written = storyStateBlockLines(payload);
  if (block) {
    lines.splice(block.open, block.close - block.open + 1, ...written);
  } else {
    // Keep the basis stable on first insertion by adding only the fence.
    const index = (home.recorded?.index ?? home.region.heading) + 1;
    lines.splice(index, 0, ...written);
  }

  const next = joinSource({ ...home.document, lines });
  const readBack = readStoryState(next, request.href, options);
  if (readBack.status !== "recorded") {
    throw new BacklogError(
      `Recorded story-state for ${home.key} did not read back as recorded ` +
        `preparation facts.`,
    );
  }
  if (
    readBack.refinement !== payload.refinement ||
    readBack.approach.kind !== payload.approach ||
    (payload.approach === "planned" && readBack.approach.plan !== payload.plan)
  ) {
    throw new BacklogError(
      `Recorded story-state for ${home.key} did not read back as supplied.`,
    );
  }
  if (payload.assessment !== undefined) {
    if (
      readBack.assessment.status !== payload.assessment ||
      JSON.stringify(readBack.assessment.reasons) !==
        JSON.stringify(payload.reasons)
    ) {
      throw new BacklogError(
        `Recorded assessment for ${home.key} did not read back as supplied.`,
      );
    }
  }

  return {
    source: next,
    result: block ? "replaced" : "recorded",
    state: readBack,
  };
}

function requirePreparationIdentity(home, identity) {
  requireField(identity, "identity");
  const named = namedIdentity(home);
  if (named === undefined) {
    throw new BacklogError(
      `${home.key} names no identity of its own, so nothing establishes that ` +
        `it is the canonical home of "${identity}".`,
    );
  }
  if (named !== identity) {
    throw new BacklogError(
      `${home.key} names identity "${named}", not "${identity}", so recording ` +
        `preparation there would claim a different story.`,
    );
  }
}

function storyStateBlock(home, refusalSuffix) {
  const blocks = findStoryStateBlocks(home.document.lines, home.region);
  if (blocks.length > 1) {
    throw new BacklogError(
      `${home.key} holds ${blocks.length} story-state blocks at lines ` +
        `${blocks.map((block) => block.open + 1).join(" and ")}; a human ` +
        `decides which one this story keeps.${refusalSuffix}`,
    );
  }
  return blocks[0];
}

function readBlockPayload(home, { body, open, close }) {
  return parseStoryStatePayload(
    body,
    `lines ${open + 1}-${close + 1} of ${home.relative}`,
  );
}
