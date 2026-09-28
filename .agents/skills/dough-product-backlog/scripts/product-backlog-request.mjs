// What a command line says a request is: every option the tool accepts, and
// the readings that turn a parsed command line into one request. A reading
// here refuses a command line that does not state a decision the operation
// needs; what that decision means to the backlog belongs with the operation.

import { resolve } from "node:path";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { defaultBacklogPath } from "./product-backlog-store.mjs";

export const options = {
  identity: { type: "string" },
  title: { type: "string" },
  link: { type: "string" },
  after: { type: "string" },
  before: { type: "string" },
  position: { type: "string" },
  plan: { type: "string" },
  "no-plan": { type: "boolean", default: false },
  refinement: { type: "string" },
  approach: { type: "string" },
  assessment: { type: "string" },
  reason: { type: "string", multiple: true },
  "expect-document": { type: "string" },
  "expect-plan": { type: "string" },
  text: { type: "string" },
  clear: { type: "boolean", default: false },
  expect: { type: "string" },
  "expect-none": { type: "boolean", default: false },
  ancestor: { type: "string" },
  branch: { type: "string", multiple: true },
  return: { type: "boolean", default: false },
  all: { type: "boolean", default: false },
  file: { type: "string", default: defaultBacklogPath },
  help: { type: "boolean", default: false },
};

// Every path a request names is read the same way the destination is: against
// the directory the command was run in.
export function resolvePath(path) {
  return path === undefined ? undefined : resolve(process.cwd(), path);
}

export function readPlacement(values) {
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

// The caller always states whether the work has an active plan, so a planned
// story can never be taken without its link by leaving an option out.
export function readPlan(values) {
  if ((values.plan !== undefined) === values["no-plan"]) {
    throw new BacklogError(
      `Supply exactly one of --plan <path> or --no-plan, so that taking work ` +
        `always states whether it has an active plan.`,
    );
  }
  return values.plan;
}

// The basis a preparation record was read from, when the caller states one.
export function readExpectedBasis(values) {
  if (
    values["expect-document"] === undefined &&
    values["expect-plan"] === undefined
  )
    return undefined;
  return { document: values["expect-document"], plan: values["expect-plan"] };
}
