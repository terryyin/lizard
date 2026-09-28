// Readiness assessment facts inside a story-state block: ready / not-ready,
// blocking reasons, and the content basis the agent reviewed. The recorder
// checks mechanical consistency only; it does not judge prose quality or grant
// execution authority. Needs reassessment is a read-time mismatch signal, not
// a stored status.

import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { basesEqual } from "./product-backlog-story-state-basis.mjs";

const assessments = new Set(["ready", "not-ready"]);

function requireReasons(reasons) {
  if (!Array.isArray(reasons)) {
    throw new BacklogError(
      `Assessment reasons must be an array of non-empty strings.`,
    );
  }
  const cleaned = [];
  for (const reason of reasons) {
    if (typeof reason !== "string" || reason.trim() === "") {
      throw new BacklogError(
        `Assessment reasons must be non-empty strings; found ` +
          `${JSON.stringify(reason)}.`,
      );
    }
    cleaned.push(reason);
  }
  return cleaned;
}

function requireReasonCount(assessment, reasons) {
  if (assessment === "ready" && reasons.length > 0) {
    throw new BacklogError(
      `A ready assessment cannot carry blocking reasons; found ` +
        `${reasons.length}.`,
    );
  }
  if (assessment === "not-ready" && reasons.length === 0) {
    throw new BacklogError(
      `A not-ready assessment requires at least one blocking reason.`,
    );
  }
}

function requireBasisShape(basis, label) {
  if (basis === null || typeof basis !== "object" || Array.isArray(basis)) {
    throw new BacklogError(`${label} must be one object with digest fields.`);
  }
  if (typeof basis.document !== "string" || basis.document.trim() === "") {
    throw new BacklogError(`${label} requires a non-empty document digest.`);
  }
  if (basis.plan !== undefined) {
    if (typeof basis.plan !== "string" || basis.plan.trim() === "") {
      throw new BacklogError(
        `${label} plan digest, when present, must be a non-empty string.`,
      );
    }
  }
  const shaped = { document: basis.document };
  if (basis.plan !== undefined) {
    shaped.plan = basis.plan;
  }
  return shaped;
}

// Validates ready/not-ready against preparation facts and reason counts.
export function requireAssessmentConsistency(
  refinement,
  approach,
  assessment,
  reasons,
) {
  if (!assessments.has(assessment)) {
    throw new BacklogError(
      `--assessment accepts "ready" or "not-ready"; found "${assessment}".`,
    );
  }
  const cleaned = requireReasons(reasons ?? []);
  requireReasonCount(assessment, cleaned);
  if (assessment === "ready") {
    if (refinement !== "refined") {
      throw new BacklogError(
        `A ready assessment requires refinement "refined"; found ` +
          `"${refinement}".`,
      );
    }
    if (approach !== "planned" && approach !== "planless") {
      throw new BacklogError(
        `A ready assessment requires approach "planned" or "planless"; found ` +
          `"${approach}".`,
      );
    }
    return { status: "ready", reasons: [] };
  }
  return { status: "not-ready", reasons: cleaned };
}

export function assessmentFromPayload(payload) {
  if (payload.assessment === undefined) {
    return undefined;
  }
  if (!assessments.has(payload.assessment)) {
    throw new BacklogError(
      `Story-state assessment must be ready or not-ready; found ` +
        `${JSON.stringify(payload.assessment)}.`,
    );
  }
  const reasons = requireReasons(payload.reasons ?? []);
  requireReasonCount(payload.assessment, reasons);
  const basis = requireBasisShape(
    payload.basis,
    "Story-state assessment basis",
  );
  return {
    status: payload.assessment,
    reasons,
    basis,
  };
}

// Read-time view: absent, matching ready/not-ready, or needs reassessment
// when the stored basis no longer matches current content. A stored basis
// matches the current basis, or the former basis a record written before
// story scoping carries; nothing is rewritten.
export function normalizeAssessmentView(recorded, currentBasis, formerBasis) {
  if (recorded === undefined) {
    return { status: "absent" };
  }
  if (
    !basesEqual(recorded.basis, currentBasis) &&
    !basesEqual(recorded.basis, formerBasis)
  ) {
    return {
      status: "needs-reassessment",
      recorded: recorded.status,
      reasons: recorded.reasons,
      basis: recorded.basis,
    };
  }
  return {
    status: recorded.status,
    reasons: recorded.reasons,
    basis: recorded.basis,
  };
}

export function requireExpectedBasis(
  expected,
  current,
  approachKind,
  planIsCanonical,
) {
  requireField(
    expected?.document,
    "expect-document",
    ` Recording an assessment needs the document digest you reviewed.`,
  );
  const shaped = requireBasisShape(expected, "Expected basis");
  if (approachKind === "planned" && !planIsCanonical) {
    if (shaped.plan === undefined) {
      throw new BacklogError(
        `A planned assessment needs --expect-plan with the plan digest you ` +
          `reviewed.`,
      );
    }
  } else if (shaped.plan !== undefined) {
    throw new BacklogError(
      `Supply --expect-plan only for planned work whose plan is a distinct ` +
        `file from the canonical home.`,
    );
  }
  if (!basesEqual(shaped, current)) {
    throw new BacklogError(
      `Expected assessment basis no longer matches the current canonical ` +
        `content (and plan, when planned). Re-read, review the change, and ` +
        `submit a fresh assessment. Nothing was written.`,
    );
  }
  return shaped;
}
