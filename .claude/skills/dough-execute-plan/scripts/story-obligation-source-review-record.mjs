import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import {
  commitmentId,
  readStoryObligations,
} from "./story-obligation-reader.mjs";
import {
  lifecycleOwners,
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

const invalid = (message) => problem("invalid-input", { message });
const positiveInteger = (value) => /^[1-9]\d*$/.test(String(value ?? ""));

function entryProblems(entry) {
  if (!entry) return [];
  return [
    [!commitmentId.test(entry.id ?? ""), "obligation id is required"],
    [!entry.title?.trim(), "obligation title is required"],
    [
      !positiveInteger(entry.reportedSlice),
      "reported slice must be a positive integer",
    ],
    [!entry.reported?.trim(), "reported text is required"],
    [!entry.storyClause?.trim(), "story clause is required"],
    [!entry.disposition?.trim(), "disposition is required"],
    [
      entry.commitments &&
        (!entry.commitments.length ||
          !entry.commitments.every((id) => commitmentId.test(id))),
      "Commitment field must name valid commitment ids",
    ],
  ].flatMap(([failed, message]) => (failed ? [invalid(message)] : []));
}

// The first input refusal for a record request, before either file is read.
function requestProblems(request, supplied) {
  const { hasObserved, hasObligation, hasSlice, hasLifecycle } = supplied;
  const cellOptions = [hasSlice, hasObserved, hasObligation, request.entry];
  if (!commitmentId.test(request.commitment ?? ""))
    return [invalid("commitment id is required")];
  if (hasLifecycle && !lifecycleOwners.includes(request.lifecycle))
    return [
      invalid(`lifecycle owner must be one of: ${lifecycleOwners.join(", ")}`),
    ];
  if (hasLifecycle && cellOptions.some(Boolean))
    return [
      invalid(
        "a lifecycle owner takes no slice, observation, obligation, or obligation entry",
      ),
    ];
  if (hasObserved && hasObligation)
    return [invalid("supply observed text or an obligation id, not both")];
  if (!hasLifecycle && !cellOptions.some(Boolean))
    return [
      invalid(
        "supply a slice, lifecycle owner, observation, obligation, or obligation entry",
      ),
    ];
  if (
    hasObligation &&
    request.obligation &&
    !commitmentId.test(request.obligation)
  )
    return [invalid("obligation id is malformed")];
  const slice = request.slice;
  if (
    hasSlice &&
    slice !== null &&
    slice !== undefined &&
    !positiveInteger(slice)
  )
    return [invalid("slice must be a positive integer")];
  return entryProblems(request.entry);
}

// Records one commitment's slice allocation or lifecycle owner, observation,
// and/or linked Story obligation. Unknown ids and stale expected digests leave
// both files intact.
export async function recordStoryCommitment(planPath, request = {}) {
  const commitment = request.commitment;
  const hasObserved = Object.hasOwn(request, "observed");
  const hasObligation = Object.hasOwn(request, "obligation");
  const hasSlice = Object.hasOwn(request, "slice");
  const hasLifecycle = Object.hasOwn(request, "lifecycle");
  const inputProblems = requestProblems(request, {
    hasObserved,
    hasObligation,
    hasSlice,
    hasLifecycle,
  });
  if (inputProblems.length) return fail(inputProblems);

  let planSource;
  try {
    planSource = await readFile(planPath, "utf8");
  } catch (error) {
    return fail([invalid(error.message)]);
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
  if (hasLifecycle) {
    // Its owner observes it after execution; any earlier state is dropped.
    next.lifecycle = request.lifecycle;
  } else if (hasSlice) {
    if (request.slice !== null && request.slice !== undefined)
      next.slice = Number(request.slice);
  } else if (!(hasObligation || request.entry)) {
    if (current.slice !== undefined) next.slice = current.slice;
    if (current.lifecycle !== undefined) next.lifecycle = current.lifecycle;
  }
  if (hasObserved && next.lifecycle)
    return fail([
      problem("observed-lifecycle-commitment", {
        commitment,
        owner: next.lifecycle,
        message:
          "Its owner observes a lifecycle commitment after execution; record no observation for it.",
      }),
    ]);
  if (hasObserved) {
    if (!/^\S/.test(request.observed ?? ""))
      return fail([invalid("observed location must be non-empty")]);
    next.observed = request.observed;
  } else if (hasObligation) {
    next.obligation = request.obligation;
  } else if (!hasLifecycle) {
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
        {
          ...invalid("the obligation entry would not match the reader"),
          ...(parsed.problems.length && { problems: parsed.problems }),
        },
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
