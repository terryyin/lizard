---
id: SEED-072
status: active
planted: 2026-11-24
planted_during: lizard process pr / process issues (past 30 hours)
scope: process open PRs new or updated since 2026-11-22T21:05Z
---

# SEED-072: Process open PRs (past 30 hours)

<a id="process-open-prs-past-30h"></a>

### Process open PRs (past 30 hours)

**Identity:** SEED-072#process-open-prs-past-30h
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"90a76dacb4dbd62578c3c515dc2fb810d23eca9f487270a2ba285be74a239a8d"}}
```

**Goal:** Maintainers disposing open pull requests that are new or newly
ready in the past 30 hours receive one complete disposition per named PR:
merge when the contribution is valid on current trunk under the near-future
direction and qualification gates, or leave it unmerged with an
evidence-backed reason.

**Beneficiary:** Maintainers clearing the PR queue, and users blocked on wrong
language metrics, extension crashes, optional warning routing, or missing
Svelte/Kotlin/Ruby reporting those PRs target.

**Scope:** Named open PRs created or updated since 2026-11-22T21:05Z that
still need a disposition after SEED-071:

- [#612](https://github.com/terryyin/lizard/pull/612) (new; optionally write
  text warnings to stderr)
- [#611](https://github.com/terryyin/lizard/pull/611) (extension crash on
  tokenless functions; undrafted after SEED-071)
- [#610](https://github.com/terryyin/lizard/pull/610) (skip missing metric
  values in thresholds/sorting; undrafted)
- [#609](https://github.com/terryyin/lizard/pull/609) (Ruby blocks ending a
  function early; undrafted)
- [#608](https://github.com/terryyin/lizard/pull/608) (Kotlin interface
  default methods; undrafted)
- [#599](https://github.com/terryyin/lizard/pull/599) (Svelte component as a
  unit; undrafted)
- [#597](https://github.com/terryyin/lizard/pull/597) (Ruby `%` literals with
  interpolation/nested delimiters; undrafted)
- [#594](https://github.com/terryyin/lizard/pull/594) (Kotlin lambda after
  comma cuts a function short; undrafted)
- [#593](https://github.com/terryyin/lizard/pull/593) (Kotlin functions lost
  after body-less declarations; undrafted)
- [#590](https://github.com/terryyin/lizard/pull/590) (Kotlin extension
  functions; undrafted)
- [#582](https://github.com/terryyin/lizard/pull/582) (cheaper implied-semicolon
  scan for ND/NS; rebased with new measurements after SEED-071)

Already merged under SEED-071 (updated timestamps only): out of scope.
Open issues in the window: none. lizard-process-issue applied with an empty
issue set.

Full lizard-process-pr scope applies: backlog ownership (this story),
evidence packets, direction check, behavior/consumer review, performance
qualification where required (especially #582 and any runtime/per-token
change), corrections when reachable, and an evidenced disposition per PR.

**Key examples:**

- #612: with the new option, warning lines go to stderr while the default
  table stays on stdout; without it, behavior matches master.
- #611/#610: a function with no metric value does not crash extensions or
  break threshold/sort; zero-value metrics still participate when present.
- #590/#593/#594/#608: Kotlin extension functions, body-less declarations,
  lambdas after commas, and interface bodies report the expected functions.
- #597/#609: Ruby `%` literals and blocks keep correct function boundaries.
- #599: a Svelte component reports as one unit without changing CLI
  signatures.
- #582: single-extension ND/NS cost vs current trunk clears the project bar
  or stays unmerged with measured evidence.

**Source:** Dashboard instruction to run lizard-process-pr and
lizard-process-issue on PRs and issues new or updated in the past 30 hours
(launch `334bc899-6b90-4da4-849c-3a92e48e3f74`).
