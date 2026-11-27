// Resolve the close's session before any push. Native evidence is a fallback
// only when the checkout has no competing Dashboard launches.
import { readFile } from "node:fs/promises";
import { readSessionEvidence } from "./execution-session-evidence.mjs";
import {
  resolvePublicationLandingContext,
  severalLaunchesOpen,
} from "./resolve-publication-landing-context.mjs";
import { sessionRecordPath } from "./sealed-session-record.mjs";
import { readSharedCheckoutNote } from "./read-shared-checkout-note.mjs";
import { hasSessionStory } from "./session-record.mjs";

async function namedSession(retained, landingContext, checkout) {
  if (landingContext) {
    const launch = JSON.parse(await readFile(landingContext, "utf8"));
    return {
      sessionId: launch.reference,
      story: launch.identity,
      sharedCheckout: await readSharedCheckoutNote(landingContext),
    };
  }
  if (retained || (await severalLaunchesOpen(checkout))) return {};
  const kept = readSessionEvidence(checkout.workspace);
  return kept && !kept.closedAt
    ? { sessionId: kept.sessionId, story: kept.story, native: true }
    : {};
}

export async function closeTarget({
  session: { sessionId: retained, story },
  backlogPath,
  ...request
}) {
  const landingContext = await resolvePublicationLandingContext({
    remote: "origin",
    ...request,
  });
  const named = await namedSession(retained, landingContext, request);
  const boundStory =
    landingContext === undefined ? (named.story ?? story) : named.story;
  const refusal =
    identityRefusal(retained, named.sessionId) ??
    storyRefusal(story, named.story, boundStory);
  if (refusal) {
    return {
      refused: {
        work: { ok: false, ...refusal },
        record: { status: "not-published" },
      },
    };
  }
  const sessionId = named.sessionId ?? retained;
  // An invalid UUID throws here, before the work is pushed.
  sessionRecordPath(sessionId, backlogPath);
  return {
    sessionId,
    story: boundStory,
    native: named.native === true,
    landingContext,
    sharedCheckout: named.sharedCheckout,
  };
}

function identityRefusal(retained, named) {
  if (named && retained && named !== retained) {
    return {
      status: "session-identity-conflict",
      error: `retained session ${retained} differs from launch reference ${named}`,
    };
  }
  if (!named && !retained) {
    return {
      status: "session-identity-missing",
      error:
        "no landing context reference or retained session id names this session",
    };
  }
  return undefined;
}

function storyRefusal(retained, named, bound) {
  if (
    named &&
    retained !== undefined &&
    retained !== null &&
    named !== retained
  )
    return {
      status: "session-story-conflict",
      error: `retained story ${retained} differs from the session's story ${named}`,
    };
  if (!hasSessionStory(bound))
    return {
      status: "session-story-missing",
      error: "no story identity is bound to this session",
    };
  return undefined;
}
