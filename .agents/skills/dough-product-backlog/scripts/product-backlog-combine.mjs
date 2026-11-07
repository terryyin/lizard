// The one rule this whole reconciliation is made of, asked of one value, of
// one work item, and of one list's order.
//
// A value a branch left as the ancestor wrote it accepts the other branch's
// change; a change both branches made the same way is applied once; and two
// different changes to one value are a decision this tool does not make and
// hands back instead. Nothing here prefers a branch or unions what survives: a
// value that is unchanged on one side is no evidence at all about the other
// side's change, which is what keeps an entry no branch touched from bringing
// back an entry a branch removed.

import { recordsOwnIdentity } from "./product-backlog-identity.mjs";
import { stateOf, valueNames } from "./product-backlog-version.mjs";

// The one rule, on one value.
export function mergeValue(ancestor, one, other) {
  if (one === other) {
    return { value: one };
  }
  if (one === ancestor) {
    return { value: other };
  }
  if (other === ancestor) {
    return { value: one };
  }
  return {};
}

export function sameState(one, other) {
  if (one === null || other === null) {
    return one === other;
  }
  return Object.keys(valueNames).every((name) => one[name] === other[name]);
}

// The one wording for two branches that simply gave one value two different
// readings, whichever value it is asked about.
function differentValueClash(named, label, versions, one, other) {
  return (
    `"${named}": the versions give it different ${label} — ` +
    `${versions[1].label} has "${one}" and ${versions[2].label} has "${other}".`
  );
}

// The one rule for a work item's identity, owning both ways this question is
// disputed rather than splitting them between the generic per-value loop and
// a check bolted onto its result. An identity names the work rather than
// saying something about it, so it is not merged as an ordinary value:
//
// - A version that did record an identity of its own is never overruled by a
//   link its home has only come to share with a different recorded identity.
//   An entry whose identity is the link beside it has not been given one of
//   its own yet, and gaining one is how a branch that adopted an identity and
//   a branch that has not are still talking about one item; a home that has
//   come to be named by a second recorded identity says nothing about which
//   work item it belongs to.
// - Failing that, an identity two branches changed differently is an ordinary
//   changed-on-both-sides disagreement, refused the same way any other value
//   is.
//
// Either way this returns one explanation, or accepts the identity the
// three-way rule allows.
function mergeIdentity(named, versions, states, ancestor, one, other) {
  const merged = mergeValue(ancestor, one, other);
  const at =
    "value" in merged
      ? states.findIndex(
          (held) =>
            held !== null &&
            recordsOwnIdentity(held.identity, held.href) &&
            held.identity !== merged.value,
        )
      : -1;
  if (at !== -1) {
    const held = states[at];
    return {
      conflict:
        `"${held.identity}": ${versions[at].label} records it as the work at ` +
        `"${held.href}", where the versions would otherwise merge to identity ` +
        `"${merged.value}". A recorded identity is kept across a move, so two ` +
        `of them do not become one work item by their links coming to coincide, ` +
        `and these versions do not establish which work this entry is.`,
    };
  }
  if ("value" in merged) {
    return { value: merged.value };
  }
  return {
    conflict: differentValueClash(
      named,
      valueNames.identity,
      versions,
      one,
      other,
    ),
  };
}

// The same rule on one work item. Membership is not a value apart from the
// rest: a branch that removed an item and a branch that changed it have stated
// different intentions about the same work, and no lifecycle order decides
// between them, so that is handed back rather than resolved.
//
// Where work sits in the queue is its priority, so a branch that gave it
// another place there stated an intention about that work just as a branch
// that retitled it did, even though the place itself is merged as the list's
// order rather than as one of the item's own values. `reprioritized` is that
// account, asked of one version and one work item.
export function mergeWork(group, versions, reprioritized) {
  const states = versions.map((version) =>
    stateOf(group.states.get(version.label)),
  );
  const [ancestor, one, other] = states;
  const named = (one ?? other ?? ancestor).identity;
  const [oneReprioritized, otherReprioritized] = versions
    .slice(1)
    .map((version) => reprioritized(version.label, group.key));

  if (sameState(one, ancestor) && !oneReprioritized) {
    return { state: other };
  }
  if (sameState(other, ancestor) && !otherReprioritized) {
    return { state: one };
  }
  if (sameState(one, other)) {
    return { state: one };
  }

  if (one === null || other === null) {
    const removed = one === null ? versions[1] : versions[2];
    const surviving = one === null ? versions[2] : versions[1];
    // What the surviving branch did with the work, so that a human is asked
    // for the decision actually in dispute. Values it left as the ancestor
    // wrote them leave its new place in the queue as the only thing it changed.
    const keptValues = sameState(one ?? other, ancestor);
    const did = keptValues
      ? "gives it a different place in the queue, which is its priority"
      : "changes it";
    const doing = keptValues ? "reprioritizing" : "changing";
    return {
      conflict:
        `"${named}": ${removed.label} removes it while ${surviving.label} ` +
        `${did}. Removing work and ${doing} it are different intentions, ` +
        `and which one applies is not established by these versions.`,
    };
  }

  const state = {};
  const clashes = new Set();

  // Asked only here, where both branches turn out to have changed the work:
  // adopting an identity, recording one on one side alone, and removing the
  // work have each already been decided above. The identity's own rule
  // decides it whole, rather than as one more value in the loop below.
  const identityOutcome = mergeIdentity(
    named,
    versions,
    states,
    ancestor?.identity,
    one.identity,
    other.identity,
  );
  if ("value" in identityOutcome) {
    state.identity = identityOutcome.value;
  } else {
    clashes.add(identityOutcome.conflict);
  }

  for (const name of Object.keys(valueNames)) {
    if (name === "identity") {
      continue;
    }
    const merged = mergeValue(ancestor?.[name], one[name], other[name]);
    if ("value" in merged) {
      state[name] = merged.value;
    } else {
      clashes.add(
        differentValueClash(
          named,
          valueNames[name],
          versions,
          one[name],
          other[name],
        ),
      );
    }
  }

  return clashes.size > 0 ? { conflict: [...clashes].join("\n") } : { state };
}

// The entries both orders list, each in the order that lists them. Reading
// only what two orders share is what keeps an item one branch took, added, or
// removed from reading as the other branch reordering a list it never touched.
function sharedPlaces(one, other) {
  const shared = new Set(one.filter((key) => other.includes(key)));
  return [
    one.filter((key) => shared.has(key)),
    other.filter((key) => shared.has(key)),
  ];
}

// Two orderings agree when the entries both of them hold come in the same
// sequence.
export function sameOrder(one, other) {
  const [held, also] = sharedPlaces(one, other);
  return (
    held.length === also.length && held.every((key, at) => key === also[at])
  );
}

// How much of two orders of the same entries already agrees: the most entries
// both of them list in the same relative order.
function agreedLength(one, other) {
  let row = new Array(other.length + 1).fill(0);
  for (const key of one) {
    const previous = row;
    row = [0];
    for (const [at, also] of other.entries()) {
      row.push(
        key === also ? previous[at] + 1 : Math.max(previous[at + 1], row[at]),
      );
    }
  }
  return row[other.length];
}

// The work whose place among the rest changed, read from one list's order in
// two versions. Only the entries both of them list can have a place here to
// have changed, which is what keeps the entries that shifted up behind work a
// branch took or removed from reading as a reprioritization of themselves.
//
// What changed is the smallest account of it: the entries that have to have
// moved for everything else to still stand in the order it stood in. Where
// more than one such account exists the versions do not establish which entry
// moved, so every entry any of those accounts blames is named — inferring one
// of them is exactly the decision this tool leaves to a human.
export function reorderedWork(ancestral, order) {
  const [held, also] = sharedPlaces(ancestral, order);
  const agreed = agreedLength(held, also);
  const without = (sequence, key) => sequence.filter((at) => at !== key);
  return new Set(
    held.filter(
      (key) => agreedLength(without(held, key), without(also, key)) === agreed,
    ),
  );
}

// The same rule again, on a list's order. This establishes only the relative
// order the versions already agree on; where a newly listed item belongs among
// the others is a priority decision, not an ordering this can infer.
export function mergeOrder(ancestor, one, other) {
  if (sameOrder(one, ancestor)) {
    return { value: other };
  }
  if (sameOrder(other, ancestor)) {
    return { value: one };
  }
  if (sameOrder(one, other)) {
    return { value: one };
  }
  return {};
}
