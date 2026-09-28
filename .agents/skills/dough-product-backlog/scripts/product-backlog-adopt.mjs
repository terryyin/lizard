// Records one identity for every active backlog entry in its canonical homes,
// and writes that identity into the entry in full.
//
// Adoption reuses the ID a canonical home already carries — a seed's own ID
// with the story's stable anchor, or the document's canonical path when the
// item has no seed. It never invents a number and never consults a registry.
// An entry that already means the right identity but spells it in the older
// shorthand keeps that identity and is rewritten to record it in full, so the
// value stops depending on where the link happens to point.
// It also never queues, takes, reorders, or removes work: membership, order,
// titles, links, and the direction keep the meaning they already had.
//
// The whole request is validated before anything is written. Homes are then
// recorded one at a time and the backlog is published last, so an interrupted
// run leaves recorded identities in place for the next run to reuse.

import {
  parseBacklog,
  renderBacklog,
  renderEntry,
} from "./product-backlog-document.mjs";
import {
  impliedIdentity,
  openHome,
  recordIdentity,
} from "./product-backlog-home.mjs";
import { planFileOf, planNamesHome } from "./product-backlog-plan.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";

const humanStop =
  "A human decides how each of these is resolved; nothing was recorded.";

function describe(entry) {
  return `line ${entry.index + 1} "${entry.title}"`;
}

// The identity this work item keeps. An entry already recording its identity
// in full keeps it: allocation happened once, and a canonical home that has
// since been renamed, moved, or re-anchored does not run it again. An anchored
// story that has never recorded one takes its seed's own ID with the anchor it
// is filed under; anything else keeps what its written entry already reads as.
function adoptedIdentity(entry, home) {
  if (home.anchor === "" || entry.recordsIdentityInFull) {
    return entry.identity;
  }
  const identity = impliedIdentity(home);
  if (identity === undefined) {
    throw new BacklogError(
      `${home.relative} carries no "id:" of its own, so the identity of the ` +
        `story anchored at "${home.anchor}" cannot be established without a ` +
        `human decision.`,
    );
  }
  if (entry.identity !== entry.href && entry.identity !== identity) {
    throw new BacklogError(
      `the entry reads as identity "${entry.identity}" but ${home.relative} ` +
        `names "${identity}".`,
    );
  }
  return identity;
}

function homesFor(backlogDirectory, entry, home) {
  const homes = [home];
  if (entry.plan && !planNamesHome(entry.plan.target, entry.href)) {
    homes.push(openHome(backlogDirectory, planFileOf(entry.plan.target)));
  }
  return homes;
}

function planEntry(backlogDirectory, entry) {
  const home = openHome(backlogDirectory, entry.href);
  const identity = adoptedIdentity(entry, home);
  const homes = homesFor(backlogDirectory, entry, home);

  for (const each of homes) {
    if (each.recorded && each.recorded.identity !== identity) {
      throw new BacklogError(
        `${each.relative} already records identity ` +
          `"${each.recorded.identity}" for this work item, but the backlog ` +
          `names "${identity}".`,
      );
    }
  }
  // Written now so an unwritable entry line is refused before any home is
  // recorded. Only the recorded identity changes; the title and links are
  // kept, and an entry already recording it in full is left byte for byte.
  const written = renderEntry({ ...entry, identity });
  return {
    entry,
    identity,
    homes,
    line: written === entry.line ? undefined : written,
  };
}

// Two work items may never end up sharing an identity or a canonical home.
function requireDistinctAdoptions(records, problems) {
  const byIdentity = new Map();
  const byHome = new Map();
  for (const record of records) {
    const clash = byIdentity.get(record.identity);
    if (clash) {
      problems.push(
        `${describe(record.entry)}: identity "${record.identity}" is already ` +
          `being adopted by ${describe(clash.entry)}; two work items would ` +
          `be combined into one.`,
      );
    } else {
      byIdentity.set(record.identity, record);
    }
    for (const home of record.homes) {
      const other = byHome.get(home.key);
      if (other && other.identity !== record.identity) {
        problems.push(
          `${describe(record.entry)}: ${home.key} is also the canonical ` +
            `home of ${describe(other.entry)} under identity ` +
            `"${other.identity}".`,
        );
      } else {
        byHome.set(home.key, record);
      }
    }
  }
}

function planAdoption(document, backlogDirectory) {
  const records = [];
  const problems = [];
  for (const entry of document.entries) {
    try {
      records.push(planEntry(backlogDirectory, entry));
    } catch (error) {
      if (!(error instanceof BacklogError)) {
        throw error;
      }
      problems.push(`${describe(entry)}: ${error.message}`);
    }
  }
  requireDistinctAdoptions(records, problems);
  if (problems.length > 0) {
    throw new BacklogError(
      `Adoption was refused and no identity was recorded:\n` +
        `${problems.map((problem) => `  ${problem}`).join("\n")}\n${humanStop}`,
    );
  }
  return records;
}

function interrupted(written, remaining, cause) {
  const done =
    written.length === 0
      ? "  (none)"
      : written
          .map((home) => `  ${home.identity} in ${home.relative}`)
          .join("\n");
  return new BacklogError(
    `Adoption stopped after recording ${written.length} identit` +
      `${written.length === 1 ? "y" : "ies"}: ${cause}\n` +
      `Recorded already, and kept:\n${done}\n` +
      `Still to record: ${remaining.length}. Re-run the same command once ` +
      `the cause is resolved; it reuses every identity already recorded and ` +
      `keeps every active entry.`,
  );
}

function recordHomes(records) {
  const pending = records.flatMap((entry) =>
    entry.homes
      .filter((home) => !home.recorded)
      .map((home) => ({ home, identity: entry.identity })),
  );
  const written = [];
  for (const [position, item] of pending.entries()) {
    try {
      recordIdentity(item.home, item.identity);
    } catch (error) {
      throw interrupted(written, pending.slice(position), error.message);
    }
    written.push({ identity: item.identity, relative: item.home.relative });
  }
  return written;
}

// Applies the whole adoption to one backlog document, recording identities in
// the canonical homes and returning the backlog to publish.
export function adoptIdentities(source, backlogDirectory) {
  const document = parseBacklog(source);
  const records = planAdoption(document, backlogDirectory);

  const relabelled = records.filter((entry) => entry.line !== undefined);
  const written = recordHomes(records);

  for (const entry of relabelled) {
    document.lines[entry.entry.index] = entry.line;
  }
  return {
    source: renderBacklog(document),
    written,
    relabelled: relabelled.map((entry) => ({
      identity: entry.identity,
      title: entry.entry.title,
    })),
    entries: records.length,
  };
}
