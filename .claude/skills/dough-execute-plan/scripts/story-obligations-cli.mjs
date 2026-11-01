import { resolve } from "node:path";
import { parseArgs } from "node:util";

const usage =
  'usage: story-obligations.mjs check --plan <PLAN.md> [--slice <N>] [--completion] | --planned-completion; or list --plan <PLAN.md> --slice <N>; or record --plan <PLAN.md> --commitment <id> [--slice <N>|""] [--observed <text> | --obligation <id>] [--entry-id <id> --title <text> --reported <text> --reported-slice <N> --story-clause <text> --disposition <text>] [--expected-review <sha256>] [--expected-plan <sha256>]';

function parseRecordRequest(values) {
  const request = { commitment: values.commitment };
  if (values.slice !== undefined) {
    if (values.slice === "") request.slice = null;
    else request.slice = Number(values.slice);
  }
  if (values.observed !== undefined) request.observed = values.observed;
  if (values.obligation !== undefined) request.obligation = values.obligation;
  if (values["expected-review"] !== undefined)
    request.expectedReview = values["expected-review"];
  if (values["expected-plan"] !== undefined)
    request.expectedPlan = values["expected-plan"];
  const entryFields = [
    values["entry-id"],
    values.title,
    values.reported,
    values["reported-slice"],
    values["story-clause"],
    values.disposition,
  ];
  if (entryFields.some((value) => value !== undefined)) {
    if (entryFields.some((value) => value === undefined)) {
      throw new Error(
        "record with a Story obligations entry needs --entry-id, --title, --reported, --reported-slice, --story-clause, and --disposition",
      );
    }
    request.entry = {
      id: values["entry-id"],
      title: values.title,
      reported: values.reported,
      reportedSlice: Number(values["reported-slice"]),
      storyClause: values["story-clause"],
      disposition: values.disposition,
    };
    if (values["commitment-field"] !== undefined) {
      request.entry.commitments = values["commitment-field"]
        .split(/,\s*/)
        .filter(Boolean);
    }
  }
  return request;
}

// Operations are injected so this module never imports story-obligations.mjs
// (that entry file loads the CLI and would cycle).
export async function runStoryObligationsCli({
  checkStoryObligations,
  listStoryObligations,
  recordStoryCommitment,
}) {
  let result;
  try {
    const { values, positionals } = parseArgs({
      options: {
        plan: { type: "string" },
        slice: { type: "string" },
        completion: { type: "boolean" },
        "planned-completion": { type: "boolean" },
        commitment: { type: "string" },
        observed: { type: "string" },
        obligation: { type: "string" },
        "entry-id": { type: "string" },
        title: { type: "string" },
        reported: { type: "string" },
        "reported-slice": { type: "string" },
        "story-clause": { type: "string" },
        disposition: { type: "string" },
        "commitment-field": { type: "string" },
        "expected-review": { type: "string" },
        "expected-plan": { type: "string" },
      },
      allowPositionals: true,
    });
    if (
      positionals.length !== 1 ||
      !["check", "list", "record"].includes(positionals[0]) ||
      !values.plan
    ) {
      throw new Error(usage);
    }
    if (positionals[0] === "record") {
      if (
        !values.commitment ||
        values.completion ||
        values["planned-completion"] ||
        (values.slice !== undefined &&
          values.slice !== "" &&
          !/^[1-9]\d*$/.test(values.slice)) ||
        (values["reported-slice"] !== undefined &&
          !/^[1-9]\d*$/.test(values["reported-slice"]))
      ) {
        throw new Error(usage);
      }
      result = await recordStoryCommitment(
        resolve(values.plan),
        parseRecordRequest(values),
      );
    } else if (
      (values.slice !== undefined && !/^[1-9]\d*$/.test(values.slice)) ||
      (values["planned-completion"] &&
        (values.slice !== undefined || values.completion)) ||
      (positionals[0] === "list" &&
        (!values.slice || values.completion || values["planned-completion"])) ||
      values.commitment !== undefined ||
      values.observed !== undefined ||
      values.obligation !== undefined
    ) {
      throw new Error(usage);
    } else {
      const slice =
        values.slice === undefined ? undefined : Number(values.slice);
      result =
        positionals[0] === "list"
          ? await listStoryObligations(resolve(values.plan), slice)
          : await checkStoryObligations(resolve(values.plan), {
              slice,
              completion: values.completion,
              plannedCompletion: values["planned-completion"],
            });
    }
  } catch (error) {
    result = {
      ok: false,
      problems: [{ reason: "invalid-input", message: error.message }],
    };
  }
  process.stdout.write(`${JSON.stringify(result)}\n`);
  if (!result.ok) process.exitCode = 1;
}
