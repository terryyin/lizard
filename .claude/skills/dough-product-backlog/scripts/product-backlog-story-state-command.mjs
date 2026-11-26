// The record-state and read-state operations: record or read one story's
// preparation in its canonical home, and keep a Taken entry linked to the
// plan its recorded preparation declares.

import { existsSync } from "node:fs";
import { dirname } from "node:path";
import { takenHeading } from "./product-backlog-document.mjs";
import { requireField } from "./product-backlog-refusal.mjs";
import { deriveStartEligibility } from "./product-backlog-start-eligibility.mjs";
import { applyToBacklog, readFile } from "./product-backlog-store.mjs";
import {
  declaredPlanTarget,
  readStartFacts,
  recordPreparation,
} from "./product-backlog-story-state-home.mjs";
import { linkTakenPlan } from "./product-backlog-take.mjs";

// The basis a preparation record was read from, when the caller states one.
function readExpectedBasis(values) {
  if (
    values["expect-document"] === undefined &&
    values["expect-plan"] === undefined
  )
    return undefined;
  return { document: values["expect-document"], plan: values["expect-plan"] };
}

// A planned approach recorded for work already in "## Taken" also links that
// entry to its plan, as taking planned work does. The link is checked against
// the backlog before the home is written, so a Taken entry linking another
// plan refuses with nothing written. The backlog is written only when the
// entry gains its link, once the home holds the plan it names.
export async function recordState(file, values) {
  requireField(values.identity, "identity");
  requireField(values.link, "link");
  const request = {
    identity: values.identity,
    href: values.link,
    refinement: values.refinement,
    approach: values.approach,
    plan: values.plan,
    assessment: values.assessment,
    reasons: values.reason,
    expectedBasis: readExpectedBasis(values),
  };
  const backlogDirectory = dirname(file);
  const target = declaredPlanTarget(
    backlogDirectory,
    request.href,
    request.approach,
    request.plan,
  );
  const linkPlan = (source) =>
    linkTakenPlan(source, {
      identity: request.identity,
      plan: target,
      backlogDirectory,
    });
  let linking = false;
  const outcome = await recordPreparation(backlogDirectory, request, () => {
    if (target !== undefined && existsSync(file)) {
      const backlog = readFile(file, `Backlog file not found: ${file}`);
      linking = linkPlan(backlog)?.result === "linked";
    }
  });
  let linked;
  if (linking) {
    await applyToBacklog(file, (source) => {
      linked = linkPlan(source) ?? { source, result: "unchanged" };
      return linked.source;
    });
  }
  console.log(reportRecordState({ ...outcome, linked, target }, values.file));
}

function reportRecordState(outcome, file) {
  const { identity, key, refinement, approach, assessment } = outcome.state;
  const approachText =
    approach.kind === "planned" ? `planned (${approach.plan})` : approach.kind;
  const verb = outcome.result === "recorded" ? "Recorded" : "Replaced";
  const assessmentText =
    assessment?.status === "ready" || assessment?.status === "not-ready"
      ? ` Assessment ${assessment.status}${
          assessment.status === "not-ready"
            ? ` (${assessment.reasons.join("; ")})`
            : ""
        }.`
      : "";
  const backlogText =
    outcome.linked?.result === "linked"
      ? `Linked its ${takenHeading} entry to the plan ${outcome.target} in ` +
        `${file}; other stories and entries were not changed.`
      : `Other stories and the backlog were not changed.`;
  return (
    `${verb} preparation for "${identity}" in ${key}: ` +
    `refinement ${refinement}, approach ${approachText}.` +
    `${assessmentText} ${backlogText}`
  );
}

// Prints the recorded preparation with the start eligibility derived from it
// and the dependency agreement. Eligibility grants no permission and starts
// nothing.
export async function readState(file, values) {
  requireField(values.link, "link");
  const { preparation, dependencies } = readStartFacts(
    dirname(file),
    values.link,
  );
  const startEligibility = deriveStartEligibility({
    preparation,
    dependencies,
  });
  console.log(JSON.stringify({ ...preparation, startEligibility }, null, 2));
}
