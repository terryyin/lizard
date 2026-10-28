// First-trunk publication resolves an omitted landingContext from an open
// first-landing expectation. Explicit landingContext still wins. Kept apart
// from first-landing-expectation.mjs so the reporting-dir clear copy never
// needs publication-git.
import { openFirstLandingExpectation } from "./first-landing-expectation.mjs";
import { managementContext } from "./publication-git.mjs";

export async function resolvePublicationLandingContext({
  landingContext,
  workspace,
  branch,
}) {
  if (typeof landingContext === "string" && landingContext !== "") {
    return landingContext;
  }
  if (
    typeof workspace !== "string" ||
    workspace === "" ||
    typeof branch !== "string" ||
    branch === ""
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
  return open?.landingContext;
}
