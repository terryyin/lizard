// An omitted landingContext resolves only for the open expectation's remote
// and target; `null` names a publication that is no landing, such as a
// session record commit. Explicit landingContext still wins and is validated
// by its caller. Kept apart
// from first-landing-expectation.mjs so the reporting-dir clear copy never
// needs publication-git.
import { readFile } from "node:fs/promises";
import {
  openFirstLandingExpectation,
  openLaunches,
} from "./first-landing-expectation.mjs";
import { managementContext } from "./publication-git.mjs";

// The expectation keys for a checkout's launches, or undefined outside a
// Git checkout, where no launch can be open.
async function launchKeys(workspace, branch) {
  try {
    return {
      repository: await managementContext(workspace),
      workspace,
      branch,
    };
  } catch {
    return undefined;
  }
}

// Whether several launches are open in this checkout, so an omitted landing
// context cannot name one of them.
export async function severalLaunchesOpen({ workspace, branch }) {
  if (typeof branch !== "string" || branch === "") return false;
  const keys = await launchKeys(workspace, branch);
  return keys !== undefined && (await openLaunches(keys)).length > 1;
}

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
  if (landingContext === null) return undefined;
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
  const keys = await launchKeys(workspace, branch);
  const open = keys && (await openFirstLandingExpectation(keys));
  if (!open) return undefined;
  const context = JSON.parse(await readFile(open.landingContext, "utf8"));
  return context.remote === remote && context.target === targetRef
    ? open.landingContext
    : undefined;
}

// The publication's landing context, resolved once, and the launch reference
// it names, so the accepted pair joins that session's evidence. Kept out of
// execution-session-evidence.mjs so execution start never needs landing.
export async function resolveLaunchLanding(request) {
  const landingContext = await resolvePublicationLandingContext(request);
  const reference = landingContext
    ? JSON.parse(await readFile(landingContext, "utf8")).reference
    : undefined;
  return { landingContext, reference };
}
