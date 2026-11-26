import { dirname } from "node:path";
import { addQueueEntry } from "./product-backlog-add.mjs";
import { queueHeading, takenHeading } from "./product-backlog-document.mjs";
import { placeEntry } from "./product-backlog-place.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import {
  applyReportedChange,
  applyToBacklog,
} from "./product-backlog-store.mjs";
import { takeEntry } from "./product-backlog-take.mjs";

function readPlacement(values) {
  const chosen = ["after", "before", "position"].filter(
    (name) => values[name] !== undefined,
  );
  if (chosen.length !== 1) {
    throw new BacklogError(
      `Supply exactly one of --after, --before, or --position; found ${chosen.length}.`,
    );
  }
  if (
    values.position !== undefined &&
    !["first", "last"].includes(values.position)
  ) {
    throw new BacklogError(
      `--position accepts "first" or "last"; found "${values.position}".`,
    );
  }
  return {
    after: values.after,
    before: values.before,
    position: values.position,
  };
}

export async function add(file, values) {
  const request = {
    identity: values.identity,
    title: values.title,
    href: values.link,
    backlogDirectory: dirname(file),
    ...readPlacement(values),
  };
  await applyToBacklog(file, (source) => addQueueEntry(source, request));
  console.log(
    `Added "${request.identity}" to "## ${queueHeading}" in ${values.file}.`,
  );
}

export async function place(file, values) {
  const request = {
    identity: values.identity,
    returning: values.return,
    ...readPlacement(values),
  };
  const outcome = await applyReportedChange(file, (source) =>
    placeEntry(source, request),
  );
  const { identity } = outcome.entry;
  const from = outcome.returned
    ? `Returned "${identity}" from "## ${takenHeading}" to`
    : `Placed "${identity}" in`;
  console.log(
    `${from} "## ${queueHeading}" in ${values.file}, at the requested position.`,
  );
}

// The caller always states whether the work has an active plan, so a planned
// story can never be taken without its link by leaving an option out.
function readTakePlan(values) {
  if ((values.plan !== undefined) === values["no-plan"]) {
    throw new BacklogError(
      `Supply exactly one of --plan <path> or --no-plan, so that taking work ` +
        `always states whether it has an active plan.`,
    );
  }
  return values.plan;
}

export async function take(file, values) {
  const request = {
    identity: values.identity,
    plan: readTakePlan(values),
    backlogDirectory: dirname(file),
  };
  const outcome = await applyReportedChange(file, (source) =>
    takeEntry(source, request),
  );
  const { identity } = outcome.entry;
  if (outcome.result === "taken") {
    console.log(
      `Took "${identity}" into "## ${takenHeading}" in ${values.file}.`,
    );
    return;
  }
  const ending =
    outcome.result === "linked"
      ? "; its plan link was added and its place kept."
      : ", unchanged.";
  console.log(
    `"${identity}" is already in "## ${takenHeading}" in ${values.file}${ending}`,
  );
}
