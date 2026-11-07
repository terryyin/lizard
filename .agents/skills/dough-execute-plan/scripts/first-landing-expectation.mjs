// Durable first-landing expectation beside the git common dir (management
// context), keyed to one launch workspace and branch. Preparation plants it
// when it writes landing-context.json; accepted capture clears it. Publication
// may later resolve an open match when --landing-context is omitted
// (resolve-publication-landing-context.mjs). This module stays free of Git
// helpers so the reporting-dir copy used by accepted capture can clear alone.
import { createHash } from "node:crypto";
import { mkdir, readFile, unlink, writeFile } from "node:fs/promises";
import path from "node:path";

const sidecarName = "first-landing-expectation.json";

function expectationKey(workspace, branch) {
  return createHash("sha256").update(`${workspace}\n${branch}`).digest("hex");
}

// Absolute path of the durable expectation file for this repository key.
export function firstLandingExpectationPath(repository, workspace, branch) {
  return path.join(
    repository,
    "open-dough",
    "first-landing-expectations",
    `${expectationKey(workspace, branch)}.json`,
  );
}

function sidecarPath(landingContext) {
  return path.join(path.dirname(path.resolve(landingContext)), sidecarName);
}

// Plant (or replace) the open expectation for this workspace/branch, naming the
// absolute prepared landing-context path. Also writes a sidecar beside that
// context so accepted capture can clear without re-deriving publish args.
export async function plantFirstLandingExpectation({
  repository,
  workspace,
  branch,
  landingContext,
}) {
  const absoluteContext = path.resolve(landingContext);
  const file = firstLandingExpectationPath(repository, workspace, branch);
  await mkdir(path.dirname(file), { recursive: true });
  const record = {
    workspace,
    branch,
    landingContext: absoluteContext,
  };
  await writeFile(file, `${JSON.stringify(record, null, 2)}\n`, {
    mode: 0o600,
  });
  await writeFile(
    sidecarPath(absoluteContext),
    `${JSON.stringify({ repository, workspace, branch }, null, 2)}\n`,
    { mode: 0o600 },
  );
  return file;
}

export async function clearFirstLandingExpectation({
  repository,
  workspace,
  branch,
}) {
  try {
    await unlink(firstLandingExpectationPath(repository, workspace, branch));
  } catch (error) {
    if (error.code !== "ENOENT") throw error;
  }
}

// Clear from the prepared context path after accepted capture. No-op when no
// expectation was planted for that context.
export async function clearFirstLandingExpectationForContext(landingContext) {
  const sidecar = sidecarPath(landingContext);
  let keys;
  try {
    keys = JSON.parse(await readFile(sidecar, "utf8"));
  } catch (error) {
    if (error.code === "ENOENT") return;
    throw error;
  }
  await clearFirstLandingExpectation(keys);
  try {
    await unlink(sidecar);
  } catch (error) {
    if (error.code !== "ENOENT") throw error;
  }
}

// Open expectation matching this workspace/branch, or undefined when none.
// First trunk publication may resolve this when --landing-context is omitted.
export async function openFirstLandingExpectation({
  repository,
  workspace,
  branch,
}) {
  const file = firstLandingExpectationPath(repository, workspace, branch);
  let record;
  try {
    record = JSON.parse(await readFile(file, "utf8"));
  } catch (error) {
    if (error.code === "ENOENT") return undefined;
    throw error;
  }
  if (
    record.workspace !== workspace ||
    record.branch !== branch ||
    typeof record.landingContext !== "string" ||
    !record.landingContext
  )
    return undefined;
  return record;
}
