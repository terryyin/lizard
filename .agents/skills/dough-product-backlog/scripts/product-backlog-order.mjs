// The merged order of one of the backlog's two lists.
//
// The order the versions settle between them is the one they already had an
// order for: the entries the ancestor listed here that survive. An entry newly
// listed here — added, or moved from the other list — has no settled place, so
// where it belongs is asked separately, and the two lists answer that
// differently because they mean different things. Taken is the display order
// of claimed work, so it appends; a queue position is a priority, so it keeps
// the place the version that listed it gave it, and a place the versions do
// not establish is handed back for a human to decide rather than chosen here.

import {
  mergeOrder,
  mergeValue,
  sameOrder,
} from "./product-backlog-combine.mjs";
import { takenHeading } from "./product-backlog-document.mjs";

// Where an entry the settled order does not place sits: the settled entry it
// comes after in the version that listed it there, or the start of the list.
// That relative position is the only thing beyond the settled order a merge
// knows about where an entry belongs, and the one rule decides it like any
// other value, so a place one version never named is no evidence at all about
// the place the other version gave it.
function placementsIn(order, settled) {
  const held = new Set(settled);
  const placements = new Map();
  let after = null;
  for (const key of order) {
    if (held.has(key)) {
      after = key;
    } else {
      placements.set(key, after);
    }
  }
  return placements;
}

// What one version puts in one place, in the order it lists them.
function gapIn(placements, anchor) {
  return [...placements].filter(([, at]) => at === anchor).map(([key]) => key);
}

// What one place holds once both versions have had their say. A version can
// settle it only when it already holds everything the other version put there,
// in the same order. Two versions each holding an entry the other does not
// leave nothing to order those entries by.
function gapOrder(held, also) {
  if (!sameOrder(held, also)) {
    return null;
  }
  if (also.every((key) => held.includes(key))) {
    return held;
  }
  if (held.every((key) => also.includes(key))) {
    return also;
  }
  return null;
}

function placeName(anchor, naming) {
  return anchor === null
    ? "at the start of the list"
    : `after "${naming.identityOf(anchor)}"`;
}

function nameEach(keys, naming) {
  return keys.map((key) => `"${naming.identityOf(key)}"`).join(", ");
}

// The queue's merged order: every settled entry, each followed by whatever the
// versions place after it. Where an entry sits in the queue is its priority, so
// an order the versions do not establish is handed back for a human to decide
// rather than chosen here.
function anchoredOrder(list, settled, one, other, naming) {
  const [first, second] = [one, other].map((order) =>
    placementsIn(order, settled),
  );
  const conflicts = [];

  for (const key of new Set([...first.keys(), ...second.keys()])) {
    const [held, also] = [first.get(key), second.get(key)];
    if ("value" in mergeValue(undefined, held, also)) {
      continue;
    }
    conflicts.push(
      `"## ${list}": ${naming.labels[0]} puts ` +
        `"${naming.identityOf(key)}" ${placeName(held, naming)} and ` +
        `${naming.labels[1]} puts it ${placeName(also, naming)}. Where work ` +
        `sits in this list is its priority, and these versions do not ` +
        `establish which place it has.`,
    );
  }

  const value = [];
  for (const anchor of [null, ...settled]) {
    if (anchor !== null) {
      value.push(anchor);
    }
    const [held, also] = [first, second].map((placements) =>
      gapIn(placements, anchor),
    );
    const gap = gapOrder(held, also);
    if (gap !== null) {
      value.push(...gap);
      continue;
    }
    conflicts.push(
      `"## ${list}": ${naming.labels[0]} puts ` +
        `${nameEach(held, naming)} and ${naming.labels[1]} puts ` +
        `${nameEach(also, naming)} in the same place, ` +
        `${placeName(anchor, naming)}, and nothing in the versions says which ` +
        `of them comes first. Where work sits in this list is its priority, ` +
        `which is a decision for a human.`,
    );
  }

  return conflicts.length > 0 ? { conflicts } : { value };
}

// The Taken list's merged order: the entries that survive, in their settled
// order, then the entries each version added, keeping each version's addition
// order and taking the lexically smallest identity whenever both versions have
// one waiting. Taken is the display order of claimed work rather than a
// priority, which is why it can be settled here instead of handed back — and
// why the queue above must not be ordered this way. It never refuses, so it
// never has to name the list it is ordering.
function appendedOrder(settled, one, other, naming) {
  const held = new Set(settled);
  const added = [one, other].map((order) =>
    order.filter((key) => !held.has(key)),
  );
  const wanted = new Set([...added[0], ...added[1]]);

  const value = [...settled];
  const done = new Set();
  const at = [0, 0];
  while (done.size < wanted.size) {
    const waiting = added.map((side, which) => {
      while (at[which] < side.length && done.has(side[at[which]])) {
        at[which] += 1;
      }
      return side[at[which]];
    });
    const next = waiting
      .filter((key) => key !== undefined)
      .reduce((best, key) =>
        naming.identityOf(key) < naming.identityOf(best) ? key : best,
      );
    value.push(next);
    done.add(next);
  }

  return { value };
}

// One list's merged order, from the order each of the three versions lists it
// in and the entries that survive the merge in it. The settled spine is what
// the versions agree on and the ancestor already had; everything else finds
// its place by what this particular list means.
export function listOrder(list, orders, held, naming) {
  const [ancestral, ...sides] = orders;
  const order = mergeOrder(ancestral, sides[0], sides[1]);
  if (!("value" in order)) {
    return {
      conflicts: [
        `"## ${list}": the versions put the entries they share in different ` +
          `orders, and neither order is the one the ancestor had.`,
      ],
    };
  }

  const listed = new Set(ancestral);
  const settled = order.value.filter((key) => held.has(key) && listed.has(key));
  const [one, other] = sides.map((side) => side.filter((key) => held.has(key)));
  return list === takenHeading
    ? appendedOrder(settled, one, other, naming)
    : anchoredOrder(list, settled, one, other, naming);
}
