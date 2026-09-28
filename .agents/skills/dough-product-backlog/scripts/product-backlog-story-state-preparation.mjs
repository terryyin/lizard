// Preparation facts carried inside a story-state block: schema version,
// refinement, and approach. Assessment fields are composed beside these by
// the shared story-state reader/writer. Read and write paths share one
// approach shape rule and map shape errors to stored-block or CLI refusal
// wording.

import { namedIdentity } from "./product-backlog-home-reader.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";

export const storyStateSchemaVersion = 1;

const refinements = new Set(["not-refined", "refined"]);
const approaches = new Set(["unselected", "planned", "planless"]);

function approachShape(kind, plan) {
  if (!approaches.has(kind)) {
    return { error: "invalid-kind", kind };
  }
  if (kind === "planned") {
    if (typeof plan !== "string" || plan.trim() === "") {
      return { error: "planned-missing-plan" };
    }
    return { approach: { kind, plan } };
  }
  if (plan !== undefined) {
    return { error: "unexpected-plan", kind };
  }
  return { approach: { kind } };
}

function approachFromPayload(payload) {
  const shaped = approachShape(payload.approach, payload.plan);
  if (shaped.error === "invalid-kind") {
    throw new BacklogError(
      `Story-state approach must be unselected, planned, or planless; found ` +
        `${JSON.stringify(shaped.kind)}.`,
    );
  }
  if (shaped.error === "planned-missing-plan") {
    throw new BacklogError(
      `A planned approach requires a non-empty plan reference relative to ` +
        `the canonical file.`,
    );
  }
  if (shaped.error === "unexpected-plan") {
    throw new BacklogError(
      `Approach "${shaped.kind}" must not carry a plan reference.`,
    );
  }
  return shaped.approach;
}

export function normalizeRecordedPreparation(payload, source) {
  const schemaVersion = payload.schemaVersion;
  if (schemaVersion !== storyStateSchemaVersion) {
    return {
      status: "unsupported-version",
      schemaVersion,
      source,
    };
  }
  if (!refinements.has(payload.refinement)) {
    throw new BacklogError(
      `Story-state refinement must be not-refined or refined; found ` +
        `${JSON.stringify(payload.refinement)}.`,
    );
  }
  return {
    status: "recorded",
    schemaVersion,
    refinement: payload.refinement,
    approach: approachFromPayload(payload),
    source,
  };
}

export function requirePreparationIdentity(home, identity) {
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

export function preparationPayload(request) {
  requireField(request.refinement, "refinement");
  if (!refinements.has(request.refinement)) {
    throw new BacklogError(
      `--refinement accepts "not-refined" or "refined"; found ` +
        `"${request.refinement}".`,
    );
  }
  requireField(request.approach, "approach");
  const shaped = approachShape(request.approach, request.plan);
  if (shaped.error === "invalid-kind") {
    throw new BacklogError(
      `--approach accepts "unselected", "planned", or "planless"; found ` +
        `"${request.approach}".`,
    );
  }
  if (shaped.error === "planned-missing-plan") {
    requireField(
      request.plan,
      "plan",
      ` A planned approach needs --plan <path> relative to the canonical file.`,
    );
  }
  if (shaped.error === "unexpected-plan") {
    throw new BacklogError(
      `Supply --plan only with --approach planned; found approach ` +
        `"${request.approach}".`,
    );
  }
  const { approach } = shaped;
  if (approach.kind === "planned") {
    return {
      schemaVersion: storyStateSchemaVersion,
      refinement: request.refinement,
      approach: "planned",
      plan: approach.plan,
    };
  }
  return {
    schemaVersion: storyStateSchemaVersion,
    refinement: request.refinement,
    approach: approach.kind,
  };
}
