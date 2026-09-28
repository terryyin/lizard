// Reconciles three versions of one backlog — the ancestor both branches
// started from, and each branch's version of it — into one complete, validated
// backlog, or into nothing at all.
//
// Everything a backlog holds is asked the same question, and it is asked next
// door: membership, each entry's title, link, list and plan, each list's
// order, the direction, and the human text around the lists. This decides
// which values there are to ask about, gathers every answer before publishing
// any of them, and reads the whole candidate back before it is written, so a
// reconciliation that cannot be made is handed to a human whole rather than
// applied in part.
//
// This reconciles supplied files. It is not Git-aware: which files hold the
// ancestor and the two branch versions is the caller's to establish.
import {
  mergeValue,
  mergeWork,
  reorderedWork,
  sameState,
} from "./product-backlog-combine.mjs";
import { directionHeading } from "./product-backlog-direction.mjs";
import { queueHeading, takenHeading } from "./product-backlog-document.mjs";
import { listOrder } from "./product-backlog-order.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import {
  publishableCandidate,
  readVersion,
  stateOf,
} from "./product-backlog-version.mjs";
import { groupWork } from "./product-backlog-work.mjs";

const ancestorLabel = "the ancestor version";
const branchLabels = ["the first branch version", "the second branch version"];

function requestedVersions(request) {
  requireField(
    request.ancestor,
    "ancestor",
    ` It names the version both branches started from, which is what makes ` +
      `each branch's change readable as a change.`,
  );
  const branches = request.branches ?? [];
  if (branches.length !== 2) {
    throw new BacklogError(
      `Supply --branch <path> exactly twice, once for each branch's version ` +
        `of the backlog; found ${branches.length}.`,
    );
  }
  return [
    readVersion(request.ancestor, ancestorLabel),
    ...branches.map((path, at) => readVersion(path, branchLabels[at])),
  ];
}

function refuse(versions, conflicts) {
  throw new BacklogError(
    [
      `The two branch versions make different changes to the same meaning, ` +
        `so nothing was merged and no part of the result was written.`,
      ...versions.map((version) => `  ${version.label}: ${version.path}`),
      ...conflicts,
      `Decide what each of these should say, repair the versions by hand, ` +
        `and merge them again.`,
    ].join("\n"),
  );
}

// Everything one version holds outside the two lists: the direction, and the
// human text above and below the lists that no backlog operation writes. Each
// is one value carried across whole, named once here both for the refusal when
// the branches give it different text and for the report when one of them
// turns out to have changed it. A value named in only one of those places
// would either refuse without saying why or publish without saying what.
const wholeValues = {
  direction: {
    clash:
      `"## ${directionHeading}": the versions give it different text, and ` +
      `this tool never writes, summarises, or chooses strategy text.`,
    change: `changed the "## ${directionHeading}"`,
  },
  preamble: {
    clash:
      `The text above the two lists differs between the versions, and no ` +
      `backlog operation writes it.`,
    change: "changed the text above the two lists",
  },
  epilogue: {
    clash:
      `The text below the two lists differs between the versions, and no ` +
      `backlog operation writes it.`,
    change: "changed the text below the two lists",
  },
};

// Reconciles the three versions and returns the backlog to publish alongside
// the transitions both branches turned out to have made.
export function mergeBacklogs(request) {
  const versions = requestedVersions(request);
  const [ancestor, one, other] = versions;
  const { groups, keyOf } = groupWork(versions);

  const orderIn = (version, list) =>
    version.entries
      .filter((entry) => entry.list === list)
      .map((entry) => keyOf.get(entry));

  // Which work an order gives another place in the queue than the ancestor
  // gave it. A place in the queue is a priority, so it is part of what a
  // version says about that work; the Taken list is the display order of
  // claimed work rather than a priority, so a place in it says nothing about
  // the work itself.
  const ancestralQueue = orderIn(ancestor, queueHeading);
  const reprioritizedBy = (order) => reorderedWork(ancestralQueue, order);

  // Established once per branch, and then asked of each work item.
  const reprioritizedIn = new Map(
    [one, other].map((version) => [
      version.label,
      reprioritizedBy(orderIn(version, queueHeading)),
    ]),
  );
  const reprioritized = (label, key) => reprioritizedIn.get(label).has(key);

  const conflicts = [];
  const merged = new Map();
  for (const group of groups) {
    const outcome = mergeWork(group, versions, reprioritized);
    if (outcome.conflict) {
      conflicts.push(outcome.conflict);
    } else {
      merged.set(group.key, outcome.state);
    }
  }

  const listed = {};
  for (const list of [takenHeading, queueHeading]) {
    const held = new Set(
      [...merged]
        .filter(([, state]) => state !== null && state.list === list)
        .map(([key]) => key),
    );
    const outcome = listOrder(
      list,
      versions.map((version) => orderIn(version, list)),
      held,
      {
        labels: branchLabels,
        identityOf: (key) => merged.get(key).identity,
      },
    );
    if ("value" in outcome) {
      listed[list] = outcome.value;
    } else {
      conflicts.push(...outcome.conflicts);
    }
  }

  // Each value outside the two lists, carried across as the bytes it was.
  const around = {};
  for (const [value, said] of Object.entries(wholeValues)) {
    const outcome = mergeValue(ancestor[value], one[value], other[value]);
    if ("value" in outcome) {
      around[value] = outcome.value;
    } else {
      conflicts.push(said.clash);
    }
  }

  if (conflicts.length > 0) {
    refuse(versions, conflicts);
  }

  const candidate = publishableCandidate(ancestor, {
    ...around,
    taken: listed[takenHeading].map((key) => merged.get(key)),
    queue: listed[queueHeading].map((key) => merged.get(key)),
  });

  return {
    source: candidate,
    changes: transitions(groups, ancestor, merged, {
      ...around,
      // The same account the merge itself is made of, asked once more of the
      // queue about to be published rather than of either branch's.
      reprioritized: reprioritizedBy(listed[queueHeading]),
    }),
    entries: [...merged.values()].filter((state) => state !== null).length,
  };
}

// What both branches turned out to have changed, read from the ancestor and
// the merged result rather than from either branch's request, so a caller sees
// the sibling removals and claims it is about to publish.
//
// Everything a merge decides is read here, not only each work item's own
// values: a place in the queue is a priority, and the direction and the human
// text around the lists are carried across whole. A change this did not read
// would be published while the report said nothing had changed at all.
function transitions(groups, ancestor, merged, published) {
  const changes = [];
  for (const group of groups) {
    const was = stateOf(group.states.get(ancestor.label));
    const now = merged.get(group.key);
    if (was === null && now !== null) {
      changes.push(`added "${now.identity}" to "## ${now.list}"`);
    } else if (was !== null && now === null) {
      changes.push(`removed "${was.identity}" from "## ${was.list}"`);
    } else if (was !== null && now !== null) {
      if (!sameState(was, now)) {
        changes.push(`changed "${now.identity}", now in "## ${now.list}"`);
      }
      if (published.reprioritized.has(group.key)) {
        changes.push(`reprioritized "${now.identity}" in "## ${queueHeading}"`);
      }
    }
  }
  for (const [value, said] of Object.entries(wholeValues)) {
    if (published[value] !== ancestor[value]) {
      changes.push(said.change);
    }
  }
  return changes;
}
