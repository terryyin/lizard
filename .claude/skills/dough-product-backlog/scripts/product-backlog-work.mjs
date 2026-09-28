// Which written entries, across several versions of one backlog, are the same
// work item.
//
// Inside a single backlog the answer is already settled: the list holds each
// work item once, by identity and by canonical home. Asked across versions,
// that same relation is what lets a branch that has adopted an identity and a
// branch that has not still be talking about one item, and what keeps a
// canonical home that moved on one branch from reading as a removal plus an
// unrelated addition. Two entries anywhere in the versions are the same work
// when they share either name, directly or through a third entry that shares
// the other one.

import { BacklogError } from "./product-backlog-refusal.mjs";

// The two names one written entry answers to.
function namesOf(entry) {
  return [`identity ${entry.identity}`, `home ${entry.href}`];
}

// Names that turn out to mean one work item, gathered as they are seen.
function sameWork() {
  const parent = new Map();

  function find(name) {
    if (!parent.has(name)) {
      parent.set(name, name);
      return name;
    }
    let root = name;
    while (parent.get(root) !== root) {
      root = parent.get(root);
    }
    let walk = name;
    while (parent.get(walk) !== root) {
      const next = parent.get(walk);
      parent.set(walk, root);
      walk = next;
    }
    return root;
  }

  return {
    find,
    join(one, other) {
      const held = find(one);
      const also = find(other);
      if (held !== also) {
        parent.set(held, also);
      }
    },
  };
}

// One group per work item, each holding whichever versions list it, plus the
// key every other part of a merge refers to that item by. Two entries of one
// version landing in the same group means that version says two things about
// one work item, which is a repair only a human can make.
export function groupWork(versions) {
  const relation = sameWork();
  for (const version of versions) {
    for (const entry of version.entries) {
      const [identity, home] = namesOf(entry);
      relation.join(identity, home);
    }
  }

  const found = new Map();
  const keyOf = new Map();
  for (const version of versions) {
    for (const entry of version.entries) {
      const root = relation.find(`identity ${entry.identity}`);
      let group = found.get(root);
      if (!group) {
        group = { key: found.size, states: new Map() };
        found.set(root, group);
      }
      const held = group.states.get(version.label);
      if (held) {
        throw new BacklogError(
          `${version.label} lists "${held.identity}" (${held.href}) and ` +
            `"${entry.identity}" (${entry.href}) separately, but across the ` +
            `supplied versions they name one work item. Repair the versions ` +
            `by hand before merging them.`,
        );
      }
      group.states.set(version.label, entry);
      keyOf.set(entry, group.key);
    }
  }

  return { groups: [...found.values()], keyOf };
}
