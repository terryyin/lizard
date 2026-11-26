// Shared option grammar and project-relative path resolution for the CLI.

import { resolve } from "node:path";
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
  "dependency-file": { type: "string" },
  "expect-dependencies": { type: "string" },
  "correct-agreement": { type: "boolean", default: false },
  "supplier-identity": { type: "string" },
  "accepted-revision": { type: "string" },
  remote: { type: "string" },
  target: { type: "string" },
  "planless-complete": { type: "boolean", default: false },
  "completion-file": { type: "string" },
  text: { type: "string" },
  clear: { type: "boolean", default: false },
  expect: { type: "string" },
  "expect-none": { type: "boolean", default: false },
  return: { type: "boolean", default: false },
  dropped: { type: "boolean", default: false },
  "intent-coverage": { type: "string" },
  "intent-coverage-unavailable": { type: "string" },
  seeds: { type: "string" },
  file: { type: "string", default: defaultBacklogPath },
  help: { type: "boolean", default: false },
};

// Every path a request names is read the same way the destination is: against
// the directory the command was run in.
export function resolvePath(path) {
  return path === undefined ? undefined : resolve(process.cwd(), path);
}
