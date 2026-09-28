// Opens and records a work item's identity in the canonical home a backlog
// link points at. Interpretation of region and recorded identity is shared
// with other consumers through the pure home reader; this module owns only the
// filesystem load and the write that places an identity under the heading,
// and the same load of the plan file an active plan link names.
//
// This owns where an identity is written, not which identity a work item has.

import { resolve } from "node:path";
import {
  identityLine,
  impliedIdentity,
  namedIdentity,
  readHome,
} from "./product-backlog-home-reader.mjs";
import { splitHref } from "./product-backlog-identity.mjs";
import { planFileOf } from "./product-backlog-plan.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { joinSource } from "./product-backlog-source.mjs";
import { readFile, replaceFile } from "./product-backlog-store.mjs";

// Pure interpretation stays importable from the home reader. Re-export only the
// helpers existing filesystem callers already take from this module.
export { impliedIdentity, namedIdentity };

// Opens the canonical home a backlog link names and reports what it already
// records. Throws a BacklogError describing the home when it cannot be used.
export function openHome(backlogDirectory, href) {
  const { path: relative } = splitHref(href);
  const path = resolve(backlogDirectory, relative);
  const source = readFile(path, `canonical home not found: ${relative}`);
  return {
    backlogDirectory,
    path,
    ...readHome(source, href),
  };
}

// Where an active plan link has to point: claiming work writes that link and
// refreshing a reference repoints it, so both check it here. The check is
// mechanical and existence-only: the plan file the target names must resolve
// relative to the backlog's own directory. `hint` says what the caller of that
// particular operation can do about a plan that is not there, because claiming
// work and repointing an established link differ in that.
export function requireResolvedPlan(backlogDirectory, target, hint) {
  readFile(
    resolve(backlogDirectory, planFileOf(target)),
    `Unresolved plan: ${target} is not there, relative to the backlog. ${hint}`,
  );
}

// Writes the identity into the home. The home is read again here, because one
// seed can be the home of several stories and because another run may have
// allocated in the meantime; an identity already recorded is never replaced.
export function recordIdentity(home, identity) {
  const current = openHome(home.backlogDirectory, home.href);
  if (current.recorded) {
    if (current.recorded.identity === identity) {
      return;
    }
    throw new BacklogError(
      `${home.relative} now records identity ` +
        `"${current.recorded.identity}", allocated while this adoption was ` +
        `running, and not "${identity}".`,
    );
  }
  const lines = [...current.document.lines];
  lines.splice(current.region.heading + 1, 0, "", identityLine(identity));
  replaceFile(current.path, joinSource({ ...current.document, lines }));
}

// Whether a document still claims an identity, asked of a reference that is
// about to be dropped. A path that is gone, or that no longer reads as a
// canonical home at all, claims nothing: only a home still recording this
// exact identity would leave two documents claiming one work item.
export function stillRecords(backlogDirectory, href, identity) {
  try {
    const home = openHome(backlogDirectory, href);
    return Boolean(home.recorded) && home.recorded.identity === identity;
  } catch (error) {
    if (error instanceof BacklogError) {
      return false;
    }
    throw error;
  }
}
