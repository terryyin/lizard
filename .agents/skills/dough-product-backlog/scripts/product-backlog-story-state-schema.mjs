// Version 1 preparation and assessment fields shared by the reader and recorder.
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { basesEqual } from "./product-backlog-story-state-basis.mjs";

export const storyStateSchemaVersion = 1;

const refinements = new Set(["not-refined", "refined"]);
const approaches = new Set(["unselected", "planned", "planless"]);

function isText(value) {
  return typeof value === "string" && value.trim() !== "";
}

function requirePreparation({ refinement, approach, plan }, writing) {
  if (writing) requireField(refinement, "refinement");
  if (!refinements.has(refinement)) {
    throw new BacklogError(
      writing
        ? `--refinement accepts "not-refined" or "refined"; found "${refinement}".`
        : `Story-state refinement must be not-refined or refined; found ${JSON.stringify(
            refinement,
          )}.`,
    );
  }
  if (writing) requireField(approach, "approach");
  if (!approaches.has(approach)) {
    throw new BacklogError(
      writing
        ? `--approach accepts "unselected", "planned", or "planless"; found "${approach}".`
        : `Story-state approach must be unselected, planned, or planless; found ${JSON.stringify(
            approach,
          )}.`,
    );
  }
  if (approach === "planned" && !isText(plan)) {
    if (writing) {
      requireField(
        plan,
        "plan",
        " A planned approach needs --plan <path> relative to the canonical file.",
      );
    }
    throw new BacklogError(
      "A planned approach requires a non-empty plan reference relative to the canonical file.",
    );
  }
  if (approach !== "planned" && plan !== undefined) {
    throw new BacklogError(
      writing
        ? `Supply --plan only with --approach planned; found approach "${approach}".`
        : `Approach "${approach}" must not carry a plan reference.`,
    );
  }
}

export function preparationPayload(request) {
  requirePreparation(request, true);
  const { refinement, approach, plan } = request;
  return {
    schemaVersion: storyStateSchemaVersion,
    refinement,
    approach,
    ...(plan === undefined ? {} : { plan }),
  };
}

export function readPreparationPayload(payload) {
  const { schemaVersion, refinement, approach, plan } = payload;
  if (schemaVersion !== storyStateSchemaVersion) {
    return { status: "unsupported-version", schemaVersion };
  }
  requirePreparation(payload, false);
  return {
    status: "recorded",
    schemaVersion,
    refinement,
    approach: { kind: approach, ...(plan === undefined ? {} : { plan }) },
  };
}

function assessmentReasons(assessment, supplied, writing) {
  if (assessment !== "ready" && assessment !== "not-ready") {
    throw new BacklogError(
      writing
        ? `--assessment accepts "ready" or "not-ready"; found "${assessment}".`
        : `Story-state assessment must be ready or not-ready; found ${JSON.stringify(
            assessment,
          )}.`,
    );
  }

  const reasons = supplied ?? [];
  if (!Array.isArray(reasons)) {
    throw new BacklogError(
      "Assessment reasons must be an array of non-empty strings.",
    );
  }
  const validated = [];
  for (const reason of reasons) {
    if (!isText(reason)) {
      throw new BacklogError(
        `Assessment reasons must be non-empty strings; found ${JSON.stringify(
          reason,
        )}.`,
      );
    }
    validated.push(reason);
  }
  if (assessment === "ready" && validated.length > 0) {
    throw new BacklogError(
      `A ready assessment cannot carry blocking reasons; found ${
        validated.length
      }.`,
    );
  }
  if (assessment === "not-ready" && validated.length === 0) {
    throw new BacklogError(
      "A not-ready assessment requires at least one blocking reason.",
    );
  }
  return validated;
}

function requireBasis(basis, label) {
  if (basis === null || typeof basis !== "object" || Array.isArray(basis)) {
    throw new BacklogError(`${label} must be one object with digest fields.`);
  }
  const { document, plan } = basis;
  if (!isText(document)) {
    throw new BacklogError(`${label} requires a non-empty document digest.`);
  }
  if (plan !== undefined && !isText(plan)) {
    throw new BacklogError(
      `${label} plan digest, when present, must be a non-empty string.`,
    );
  }
  return plan === undefined ? { document } : { document, plan };
}

export function readAssessmentPayload(payload) {
  if (payload.assessment === undefined) return { status: "absent" };
  return {
    status: payload.assessment,
    reasons: assessmentReasons(payload.assessment, payload.reasons, false),
    basis: requireBasis(payload.basis, "Story-state assessment basis"),
  };
}

export function assessmentPayload(request, currentBasis, planIsCanonical) {
  const { refinement, approach, assessment, reasons, expectedBasis } = request;
  if (assessment === undefined) {
    if (reasons !== undefined || expectedBasis !== undefined) {
      throw new BacklogError(
        "Supply --assessment ready|not-ready when recording reasons or an expected basis.",
      );
    }
    return {};
  }

  const recordedReasons = assessmentReasons(assessment, reasons, true);
  if (assessment === "ready") {
    if (refinement !== "refined") {
      throw new BacklogError(
        `A ready assessment requires refinement "refined"; found "${refinement}".`,
      );
    }
    if (approach !== "planned" && approach !== "planless") {
      throw new BacklogError(
        `A ready assessment requires approach "planned" or "planless"; found "${approach}".`,
      );
    }
  }

  requireField(
    expectedBasis?.document,
    "expect-document",
    " Recording an assessment needs the document digest you reviewed.",
  );
  const basis = requireBasis(expectedBasis, "Expected basis");
  if (approach === "planned" && !planIsCanonical) {
    if (basis.plan === undefined) {
      throw new BacklogError(
        "A planned assessment needs --expect-plan with the plan digest you reviewed.",
      );
    }
  } else if (basis.plan !== undefined) {
    throw new BacklogError(
      "Supply --expect-plan only for planned work whose plan is a distinct file from the canonical home.",
    );
  }
  if (!basesEqual(basis, currentBasis)) {
    throw new BacklogError(
      "Expected assessment basis no longer matches the current canonical " +
        "content (and plan, when planned). Re-read, review the change, and " +
        "submit a fresh assessment. Nothing was written.",
    );
  }
  return { assessment, reasons: recordedReasons, basis };
}
