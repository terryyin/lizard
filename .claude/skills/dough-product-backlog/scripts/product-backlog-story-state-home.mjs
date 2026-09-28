// Filesystem boundary for story-state preparation and assessment records:
// opens a canonical home, serializes cooperating writers per file, loads a
// planned path relative to that home when needed for digests, and applies the
// pure record/read operations. Backlog bytes are never rewritten here; the
// caller links a Taken entry's plan through the backlog's own take rule.

import { dirname, relative as relativePath, resolve, sep } from "node:path";
import { splitHref } from "./product-backlog-identity.mjs";
import { sameDocument } from "./product-backlog-plan.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { applyToFile, readFile } from "./product-backlog-store.mjs";
import { preparationPayload } from "./product-backlog-story-state-preparation.mjs";
import {
  readStoryState,
  recordStoryState,
} from "./product-backlog-story-state.mjs";

function homePath(backlogDirectory, href) {
  const { path: relative } = splitHref(href);
  return {
    relative,
    path: resolve(backlogDirectory, relative),
  };
}

// The plan a planned approach declares beside the canonical home `href` names:
// its backlog-relative link target, as declared, and the file it names. A plan
// that is the home itself is its own plan, digested once and never linked as
// a second reference, so it names no separate plan.
function separatePlan(backlogDirectory, href, plan) {
  const home = homePath(backlogDirectory, href).path;
  const { path, anchor } = splitHref(plan);
  const file = resolve(dirname(home), path);
  const target = backlogLink(backlogDirectory, file, anchor);
  return sameDocument(target, backlogLink(backlogDirectory, home))
    ? undefined
    : { target, file };
}

// How the backlog links `file`, and a section of it when `anchor` names one.
function backlogLink(backlogDirectory, file, anchor = "") {
  const relative = relativePath(resolve(backlogDirectory), file)
    .split(sep)
    .join("/");
  return anchor === "" ? relative : `${relative}#${anchor}`;
}

// The backlog-relative link target of the separate plan a planned approach
// declares beside the canonical home `href` names, or undefined when the
// approach names no separate plan.
export function declaredPlanTarget(backlogDirectory, href, approach, plan) {
  if (approach !== "planned" || typeof plan !== "string") {
    return undefined;
  }
  return separatePlan(backlogDirectory, href, plan)?.target;
}

// Loads plan text beside the canonical home when digests need it. A plan that
// is the home itself is marked canonical so the basis digests it once.
function planLoadOptions(backlogDirectory, href, approach, plan) {
  if (approach !== "planned") {
    return { planSource: undefined, planIsCanonical: false };
  }
  const separate = separatePlan(backlogDirectory, href, plan);
  if (separate === undefined) {
    return { planSource: undefined, planIsCanonical: true };
  }
  const planSource = readFile(
    separate.file,
    `Unresolved plan: ${plan} is not there, relative to the canonical file. ` +
      `Create the plan first, or supply the path the story should associate.`,
  );
  return { planSource, planIsCanonical: false };
}

// Reads preparation facts and assessment view from the canonical home a link
// names, including the current content basis.
export function readPreparation(backlogDirectory, href) {
  const { relative, path } = homePath(backlogDirectory, href);
  const source = readFile(path, `canonical home not found: ${relative}`);
  const preview = readStoryState(source, href);
  if (preview.status === "recorded" && preview.approach.kind === "planned") {
    const options = planLoadOptions(
      backlogDirectory,
      href,
      "planned",
      preview.approach.plan,
    );
    return readStoryState(source, href, options);
  }
  return preview;
}

// Records preparation and optional assessment for one story under a
// cooperating per-file lock. Other stories in the same seed, identity lines,
// and the backlog file are left untouched. A planned approach must resolve
// beside the canonical file; planless needs no plan file. `confirm`, when
// given, sees the validated outcome before the home is written and refuses by
// throwing, so a refusal it raises leaves the home unchanged.
export async function recordPreparation(
  backlogDirectory,
  request,
  confirm = () => {},
) {
  const { relative, path } = homePath(backlogDirectory, request.href);
  let outcome;
  await applyToFile(
    path,
    (source) => {
      const preparation = preparationPayload(request);
      const options = planLoadOptions(
        backlogDirectory,
        request.href,
        preparation.approach,
        preparation.plan,
      );
      outcome = recordStoryState(source, request, options);
      confirm(outcome);
      return outcome.source;
    },
    `canonical home not found: ${relative}`,
  );
  return outcome;
}

// Re-export the pure reader for callers that already hold source text.
export { readStoryState };

// Surface lock/refusal cases that are about the home, not the backlog queue.
export function preparationRefusal(error) {
  if (!(error instanceof BacklogError)) {
    return undefined;
  }
  return error.refusal.replace(
    /\nThe backlog was not changed\.$/,
    "\nNothing was written.",
  );
}
