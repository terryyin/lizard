// The record-state and read-state operations: record or read one story's
// preparation in its canonical home, and keep a Taken entry linked to the
// plan its recorded preparation declares.

import { existsSync } from "node:fs";
import { dirname } from "node:path";
import { requireField } from "./product-backlog-refusal.mjs";
import { reportRecordState } from "./product-backlog-report.mjs";
import { readExpectedBasis } from "./product-backlog-request.mjs";
import { applyToBacklog, readFile } from "./product-backlog-store.mjs";
import {
  declaredPlanTarget,
  readPreparation,
  recordPreparation,
} from "./product-backlog-story-state-home.mjs";
import { linkTakenPlan } from "./product-backlog-take.mjs";

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

export async function readState(file, values) {
  requireField(values.link, "link");
  const state = readPreparation(dirname(file), values.link);
  console.log(JSON.stringify(state, null, 2));
}
