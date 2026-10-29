// An omitted landingContext resolves only for the open expectation's remote
// and target. Explicit landingContext still wins and is validated by its caller. Kept apart
// from first-landing-expectation.mjs so the reporting-dir clear copy never
// needs publication-git.
import { readFile } from "node:fs/promises";
import { openFirstLandingExpectation } from "./first-landing-expectation.mjs";
import { managementContext } from "./publication-git.mjs";

export async function resolvePublicationLandingContext({
  landingContext,
  workspace,
  branch,
  remote,
  targetRef,
}) {
  if (typeof landingContext === "string" && landingContext !== "") {
    return landingContext;
  }
  if (
    typeof workspace !== "string" ||
    workspace === "" ||
    typeof branch !== "string" ||
    branch === "" ||
    typeof remote !== "string" ||
    remote === "" ||
    typeof targetRef !== "string" ||
    targetRef === ""
  ) {
    return undefined;
  }
  let repository;
  try {
    repository = await managementContext(workspace);
  } catch {
    return undefined;
  }
  const open = await openFirstLandingExpectation({
    repository,
    workspace,
    branch,
  });
  if (!open) return undefined;
  const context = JSON.parse(await readFile(open.landingContext, "utf8"));
  return context.remote === remote && context.target === targetRef
    ? open.landingContext
    : undefined;
}
