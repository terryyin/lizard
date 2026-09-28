// One versioned story-state block inside a canonical home: preparation facts
// and an optional readiness assessment bound to content digests. Reading and
// writing the block from already-loaded text lives here so CLI and browser
// share one meaning without a second Markdown status grammar and without
// importing filesystem access.
//
// Missing structured fields are Not recorded — never inferred from free-form
// Status prose. Unsupported schema version and legacy absence are distinct
// results. Needs reassessment is reported on basis mismatch without rewriting
// the source. Recording never grants execution authority.

import {
  namedIdentity,
  readHome,
  storyContextLines,
} from "./product-backlog-home-reader.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { joinSource } from "./product-backlog-source.mjs";
import {
  assessmentFromPayload,
  normalizeAssessmentView,
  requireAssessmentConsistency,
  requireExpectedBasis,
} from "./product-backlog-story-state-assessment.mjs";
import { computeBasis } from "./product-backlog-story-state-basis.mjs";
import {
  findStoryStateBlocks,
  parseStoryStatePayload,
  storyStateBlockLines,
  storyStateSourceLocation,
} from "./product-backlog-story-state-block.mjs";
import {
  normalizeRecordedPreparation,
  preparationPayload,
  requirePreparationIdentity,
  storyStateSchemaVersion,
} from "./product-backlog-story-state-preparation.mjs";

export { storyStateFence } from "./product-backlog-story-state-block.mjs";
export { storyStateSchemaVersion };
export {
  computeBasis,
  digestSource,
} from "./product-backlog-story-state-basis.mjs";

function insertionIndex(home) {
  if (home.recorded) {
    return home.recorded.index + 1;
  }
  return home.region.heading + 1;
}

// The plan text a basis digests separately: none for planless work or for a
// plan that is the canonical document itself, which is digested once.
function separatePlanSource(approach, options) {
  if (approach?.kind !== "planned" || options.planIsCanonical === true) {
    return undefined;
  }
  return options.planSource;
}

// The story-scoped basis every fresh assessment records: this story's
// section, the seed's shared context, and a distinct plan. Other stories'
// sections are left out, so preparing, editing, or closing a sibling keeps
// this story's readiness.
function currentBasisFor(home, approach, options = {}) {
  const scoped = joinSource({
    ...home.document,
    lines: storyContextLines(home),
  });
  return computeBasis(scoped, separatePlanSource(approach, options));
}

// The whole-document basis a record from before story scoping carries. It
// still matches while the whole seed is unchanged.
function formerBasisFor(source, approach, options = {}) {
  return computeBasis(source, separatePlanSource(approach, options));
}

// Reads preparation facts and assessment view from already-loaded
// canonical-home text. Legacy absence and unsupported version stay distinct
// from recorded facts. Always returns the current content basis.
export function readStoryState(source, href, options = {}) {
  const home = readHome(source, href);
  const blocks = findStoryStateBlocks(home.document.lines, home.region);
  if (blocks.length === 0) {
    return {
      status: "not-recorded",
      identity: namedIdentity(home),
      href: home.href,
      key: home.key,
      source: { path: home.relative, href: home.href },
      basis: currentBasisFor(home, undefined, options),
      assessment: { status: "absent" },
    };
  }
  if (blocks.length > 1) {
    throw new BacklogError(
      `${home.key} holds ${blocks.length} story-state blocks at lines ` +
        `${blocks.map((block) => block.open + 1).join(" and ")}; a human ` +
        `decides which one this story keeps.`,
    );
  }
  const [block] = blocks;
  const location = `lines ${block.open + 1}-${block.close + 1} of ${home.relative}`;
  const payload = parseStoryStatePayload(block.body, location);
  const sourceInfo = storyStateSourceLocation(home, block.open, block.close);
  const normalized = normalizeRecordedPreparation(payload, sourceInfo);
  if (normalized.status !== "recorded") {
    return {
      ...normalized,
      identity: namedIdentity(home),
      href: home.href,
      key: home.key,
      basis: currentBasisFor(home, undefined, options),
      assessment: { status: "absent" },
    };
  }
  const recordedAssessment = assessmentFromPayload(payload);
  const basis = currentBasisFor(home, normalized.approach, options);
  const former = formerBasisFor(source, normalized.approach, options);
  return {
    ...normalized,
    identity: namedIdentity(home),
    href: home.href,
    key: home.key,
    basis,
    assessment: normalizeAssessmentView(recordedAssessment, basis, former),
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
  const blocks = findStoryStateBlocks(home.document.lines, home.region);
  if (blocks.length > 1) {
    throw new BacklogError(
      `${home.key} holds ${blocks.length} story-state blocks at lines ` +
        `${blocks.map((block) => block.open + 1).join(" and ")}; a human ` +
        `decides which one this story keeps. Nothing was written.`,
    );
  }
  if (blocks.length === 1) {
    const existing = parseStoryStatePayload(
      blocks[0].body,
      `lines ${blocks[0].open + 1}-${blocks[0].close + 1} of ${home.relative}`,
    );
    if (existing.schemaVersion !== storyStateSchemaVersion) {
      throw new BacklogError(
        `${home.key} already holds story-state schema version ` +
          `${JSON.stringify(existing.schemaVersion)}, which this recorder ` +
          `does not support. Nothing was written.`,
      );
    }
  }

  const planIsCanonical = options.planIsCanonical === true;
  const approachForBasis = {
    kind: payload.approach,
    plan: payload.plan,
  };
  const currentBasis = currentBasisFor(home, approachForBasis, {
    planSource: options.planSource,
    planIsCanonical,
  });

  if (request.assessment !== undefined) {
    const assessed = requireAssessmentConsistency(
      payload.refinement,
      payload.approach,
      request.assessment,
      request.reasons,
    );
    const basis = requireExpectedBasis(
      request.expectedBasis,
      currentBasis,
      payload.approach,
      planIsCanonical,
    );
    payload.assessment = assessed.status;
    payload.reasons = assessed.reasons;
    payload.basis = basis;
  } else if (
    request.reasons !== undefined ||
    request.expectedBasis !== undefined
  ) {
    throw new BacklogError(
      `Supply --assessment ready|not-ready when recording reasons or an ` +
        `expected basis.`,
    );
  }

  const lines = [...home.document.lines];
  const written = storyStateBlockLines(payload);
  if (blocks.length === 1) {
    const { open, close } = blocks[0];
    lines.splice(open, close - open + 1, ...written);
  } else {
    // Insert the fence with no surrounding blank so the first record does not
    // change digestable document bytes outside the excluded state block.
    lines.splice(insertionIndex(home), 0, ...written);
  }

  const next = joinSource({ ...home.document, lines });
  const readBack = readStoryState(next, request.href, {
    planSource: options.planSource,
    planIsCanonical,
  });
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
    result: blocks.length === 0 ? "recorded" : "replaced",
    state: readBack,
  };
}
