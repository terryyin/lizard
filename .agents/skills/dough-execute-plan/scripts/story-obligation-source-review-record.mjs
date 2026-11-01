import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import {
  commitmentId,
  readStoryObligations,
} from "./story-obligation-reader.mjs";
import {
  linkedSourceReviewPath,
  readSourceReview,
} from "./story-obligation-source-review.mjs";
import {
  renderCommitmentRow,
  replaceCommitmentLine,
  replaceFiles,
  upsertObligationEntry,
} from "./story-obligation-source-review-write.mjs";

// One supported recording path for a Commitments row and, when requested, its
// linked Story obligations entry. Readers and check stay authoritative for
// acceptance; this only writes shapes they already accept.

export function digest(source) {
  return createHash("sha256").update(source).digest("hex");
}

function problem(reason, fields = {}) {
  return { reason, ...fields };
}

function fail(problems) {
  return { ok: false, problems };
}

function validateEntry(entry) {
  if (!entry) return [];
  const problems = [];
  if (!commitmentId.test(entry.id ?? ""))
    problems.push(
      problem("invalid-input", { message: "obligation id is required" }),
    );
  if (!entry.title?.trim())
    problems.push(
      problem("invalid-input", { message: "obligation title is required" }),
    );
  if (!/^[1-9]\d*$/.test(String(entry.reportedSlice ?? "")))
    problems.push(
      problem("invalid-input", {
        message: "reported slice must be a positive integer",
      }),
    );
  if (!entry.reported?.trim())
    problems.push(
      problem("invalid-input", { message: "reported text is required" }),
    );
  if (!entry.storyClause?.trim())
    problems.push(
      problem("invalid-input", { message: "story clause is required" }),
    );
  if (!entry.disposition?.trim())
    problems.push(
      problem("invalid-input", { message: "disposition is required" }),
    );
  if (
    entry.commitments &&
    (!entry.commitments.length ||
      !entry.commitments.every((id) => commitmentId.test(id)))
  )
    problems.push(
      problem("invalid-input", {
        message: "Commitment field must name valid commitment ids",
      }),
    );
  return problems;
}

// Records one commitment's slice allocation, observation, and/or linked Story
// obligation. Unknown ids and stale expected digests leave both files intact.
export async function recordStoryCommitment(planPath, request = {}) {
  const commitment = request.commitment;
  if (!commitmentId.test(commitment ?? ""))
    return fail([
      problem("invalid-input", { message: "commitment id is required" }),
    ]);
  const hasObserved = Object.hasOwn(request, "observed");
  const hasObligation = Object.hasOwn(request, "obligation");
  const hasSlice = Object.hasOwn(request, "slice");
  if (hasObserved && hasObligation)
    return fail([
      problem("invalid-input", {
        message: "supply observed text or an obligation id, not both",
      }),
    ]);
  if (!hasObserved && !hasObligation && !hasSlice && !request.entry)
    return fail([
      problem("invalid-input", {
        message: "supply a slice, observation, obligation, or obligation entry",
      }),
    ]);
  if (
    hasObligation &&
    request.obligation &&
    !commitmentId.test(request.obligation)
  )
    return fail([
      problem("invalid-input", { message: "obligation id is malformed" }),
    ]);
  if (
    hasSlice &&
    request.slice !== null &&
    request.slice !== undefined &&
    !/^[1-9]\d*$/.test(String(request.slice))
  )
    return fail([
      problem("invalid-input", { message: "slice must be a positive integer" }),
    ]);
  const entryProblems = validateEntry(request.entry);
  if (entryProblems.length) return fail(entryProblems);

  let planSource;
  try {
    planSource = await readFile(planPath, "utf8");
  } catch (error) {
    return fail([problem("invalid-input", { message: error.message })]);
  }
  if (
    request.expectedPlan !== undefined &&
    digest(planSource) !== request.expectedPlan
  )
    return fail([problem("stale-input", { file: "plan" })]);

  const reviewPath = linkedSourceReviewPath(planPath, planSource);
  if (!reviewPath)
    return fail([
      problem("unlinked-source-review", {
        message:
          "Link the source review with one “**Source review:** [text](source-review.md)” line.",
      }),
    ]);

  let reviewSource;
  try {
    reviewSource = await readFile(reviewPath, "utf8");
  } catch (error) {
    return fail([
      problem("unreadable-source-review", { message: error.message }),
    ]);
  }
  if (
    request.expectedReview !== undefined &&
    digest(reviewSource) !== request.expectedReview
  )
    return fail([problem("stale-input", { file: "source-review" })]);

  const review = readSourceReview(reviewSource);
  if (review.problems.length) return fail(review.problems);
  const current = review.commitments.find((item) => item.id === commitment);
  if (!current) return fail([problem("unknown-commitment", { commitment })]);

  const next = {
    id: current.id,
    text: current.text,
  };
  if (hasSlice) {
    if (request.slice !== null && request.slice !== undefined)
      next.slice = Number(request.slice);
  } else if (current.slice !== undefined && !(hasObligation || request.entry)) {
    next.slice = current.slice;
  }
  if (hasObserved) {
    if (!/^\S/.test(request.observed ?? ""))
      return fail([
        problem("invalid-input", {
          message: "observed location must be non-empty",
        }),
      ]);
    next.observed = request.observed;
  } else if (hasObligation) {
    next.obligation = request.obligation;
  } else {
    if (current.observed !== undefined) next.observed = current.observed;
    if (current.obligation !== undefined) next.obligation = current.obligation;
  }

  const row = renderCommitmentRow(next);
  const nextReview = replaceCommitmentLine(reviewSource, commitment, row);
  if (nextReview === undefined)
    return fail([
      problem("unreadable-source-review", {
        message: "The commitment row could not be located for rewrite.",
      }),
    ]);

  let nextPlan = planSource;
  if (request.entry) {
    const entry = {
      ...request.entry,
      reportedSlice: Number(request.entry.reportedSlice),
      commitments:
        request.entry.commitments ??
        (hasObligation || request.obligation || next.obligation
          ? [commitment]
          : undefined),
    };
    if (entry.commitments && !entry.commitments.includes(commitment))
      entry.commitments = [...entry.commitments, commitment];
    nextPlan = upsertObligationEntry(planSource, entry);
    const parsed = readStoryObligations(nextPlan);
    const written = parsed.entries.find((item) => item.id === entry.id);
    if (!written || parsed.problems.some((item) => item.entry === entry.id))
      return fail([
        problem("invalid-input", {
          message: "the obligation entry would not match the reader",
          ...(parsed.problems.length && { problems: parsed.problems }),
        }),
      ]);
  }

  // Re-check CAS immediately before write so a racing edit is refused.
  const freshReview = await readFile(reviewPath, "utf8");
  if (freshReview !== reviewSource)
    return fail([problem("stale-input", { file: "source-review" })]);
  if (nextPlan !== planSource) {
    const freshPlan = await readFile(planPath, "utf8");
    if (freshPlan !== planSource)
      return fail([problem("stale-input", { file: "plan" })]);
  }

  const updates = [
    { path: reviewPath, contents: nextReview, previous: reviewSource },
  ];
  if (nextPlan !== planSource)
    updates.push({ path: planPath, contents: nextPlan, previous: planSource });
  await replaceFiles(updates);

  return {
    ok: true,
    commitment: next,
    ...(request.entry && { entry: request.entry.id }),
    reviewDigest: digest(nextReview),
    ...(nextPlan !== planSource && { planDigest: digest(nextPlan) }),
  };
}
