---
id: SEED-033
status: active
planted: 2026-11-04
planted_during: Lizard Process PR https://github.com/terryyin/lizard/pull/555
scope: Swift keyword argument labels with local names
---

# SEED-033: Process PR #555

## Stories

<a id="process-pr-555"></a>

### Process PR #555 — Swift keyword argument labels with a local name

**Identity:** SEED-033#process-pr-555
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** Contributors and maintainers receive an evidenced disposition for
[PR #555](https://github.com/terryyin/lizard/pull/555) (Swift: mask keyword
argument labels that have a local name), including merge when the change is
correct, justified, and authorized delivery conditions hold.

**Source:** Dashboard ad-hoc instruction: `Lizard Process PR
https://github.com/terryyin/lizard/pull/555` with landing `auto-land` on the
default checkout.

**Beneficiary:** Swift users whose declarations use keyword labels with a
separate local name (for example `func sample(for value: Int)`), and
maintainers deciding whether to accept this contribution.

**Scope:** Product-backlog intake through evidence-based review, any bounded
correction that preserves contributor history, full local qualification, hosted
CI, and authorized merge or an evidenced non-merge disposition for this PR
only. No unrelated language work.

**Key examples:**

- Expected: `func f(for value: Int, enabled: Bool) -> Int { ... }` with the
  same body as an `_ value` baseline reports the same CCN and cognitive
  complexity; real `for` loops (including typed `for i: Int in`) still count;
  existing call-site labels `run(for: x)` stay masked.
- Actual (reported on the PR): the label normalizer only recognized
  `keyword:`, so `for value:` was treated as control flow (one fixture showed
  CCN 6 / cognitive 1 vs baseline CCN 5 / cognitive 8).
- Frozen head at intake: `a130474b67948284d3f4b7e655f1d377d51e295b` on
  `cpheinrich:codex/fix-swift-declaration-labels`; base
  `6a70fefa989bcd6181e2d429bbf5fe791fd87e30`; evidence packet under
  `.planning/pr-555-evidence/`. Maintainer CHANGELOG note advanced the head
  to `b9c2e6981a007577b05b13f4820b8eda4081b9bf` before merge.

**Authority:** Full process-PR scope — review, reproduce, correct on the
contributor branch when reachable, qualify, and merge when gates hold, or
record a precise non-merge disposition. Publish ordinary lifecycle records.
