---
id: SEED-070
status: active
planted: 2026-11-21
planted_during: lizard process pr / process issues (past two days)
scope: process Hafez open PRs updated 2026-11-19..21
---

# SEED-070: Process Hafez open PRs (2026-11-19..21)

<a id="process-hafez-prs-nov19-21"></a>

### Process Hafez open PRs (2026-11-19..21)

**Identity:** SEED-070#process-hafez-prs-nov19-21
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"7486bf9d1d7cae5346219563cd7f78226ffb95efcb05f36ae0d91cab4817accc"}}
```

**Goal:** Maintainers disposing Hafez's open pull requests from
2026-11-19 through 2026-11-21 receive one complete disposition per named
PR: merge when the contribution is valid on current trunk under the
near-future direction and qualification gates, or leave it unmerged with
an evidence-backed reason (including draft unreadiness or a superseded
closed PR).

**Beneficiary:** Maintainers clearing the PR queue, and users blocked on
wrong Vue/JS/TS/JSX metrics those PRs target.

**Scope:** Named open PRs only:
[#579](https://github.com/terryyin/lizard/pull/579) (Vue script blocks),
[#580](https://github.com/terryyin/lizard/pull/580) (nesting after a
function that starts at `(`),
[#581](https://github.com/terryyin/lizard/pull/581) (nesting after a
same-line parenthesized expression; builds on #580),
[#582](https://github.com/terryyin/lizard/pull/582) (shared implied-semicolon
scan; draft; follows dropped SEED-069),
[#584](https://github.com/terryyin/lizard/pull/584) (keyword members such as
`promise.catch()`),
[#585](https://github.com/terryyin/lizard/pull/585) (namespaced JSX names),
[#587](https://github.com/terryyin/lizard/pull/587) (block after a
keyword-named property; replaces closed #583).

Already completed or closed outside this owner:
#575–#578 (done records SEED-062..065 / SEED-066),
closed draft #583 (replaced by #587),
closed draft #586 (SEED-067 already on trunk).
No GitHub issues were created or updated in the two-day window.

Full lizard-process-pr scope applies: backlog ownership (this story),
evidence packets, direction check, behavior/consumer review, performance
qualification where required, corrections when reachable, and an
evidenced disposition per PR. Issues skill applied: empty issue set for
the window.

**Key examples:**

- #579: a `.vue` file whose template contains `Don't` still reports the
  script function with the expected CCN.
- #580/#581: braceless sibling `if`s after a mapped callback or a
  same-line parenthesized assignment report ND/NS 1, not inflated nesting.
- #584: `promise.catch(() => {})` inside `if` does not inflate cognitive
  complexity or ND.
- #585: `<b on:click={go}>Go? Or stay?</b>` reports CCN 1 for the
  component function.
- #587: a block after `themes.default` / keyword property does not make
  sibling braceless statements count as nested.
- #582: draft performance change meets recorded single-extension bars or
  stays unmerged with the SEED-069 cost evidence.

**Source:** Dashboard instruction to run lizard-process-pr and
lizard-process-issue on PRs and issues from the past two days
(launch `9a581ddf-1b5d-4314-81dd-3e4f657236dc`).
