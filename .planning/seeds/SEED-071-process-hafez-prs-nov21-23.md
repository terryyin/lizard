---
id: SEED-071
status: active
planted: 2026-11-23
planted_during: lizard process pr / process issues (past 48 hours)
scope: process Hafez open PRs updated 2026-11-21..23
---

# SEED-071: Process Hafez open PRs (2026-11-21..23)

<a id="process-hafez-prs-nov21-23"></a>

### Process Hafez open PRs (2026-11-21..23)

**Identity:** SEED-071#process-hafez-prs-nov21-23
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"eb93756a3b7c8e289fd886fd060e081f59729fa94a30f6e273ac21cbdcb13d81"}}
```

**Goal:** Maintainers disposing Hafez's open pull requests updated in the
past 48 hours receive one complete disposition per named PR: merge when the
contribution is valid on current trunk under the near-future direction and
qualification gates, or leave it unmerged with an evidence-backed reason
(including draft unreadiness).

**Beneficiary:** Maintainers clearing the PR queue, and users blocked on wrong
language metrics, extension crashes, or missing language support those PRs
target.

**Scope:** Named open PRs updated since 2026-11-21T00:57Z only:

Non-draft (merge candidates when qualified):
[#582](https://github.com/terryyin/lizard/pull/582) (implied-semicolon scan;
was draft under SEED-070, now ready),
[#588](https://github.com/terryyin/lizard/pull/588) (Kotlin lambda named params),
[#589](https://github.com/terryyin/lizard/pull/589) (Svelte markup functions),
[#591](https://github.com/terryyin/lizard/pull/591) (Ruby keyword method names),
[#592](https://github.com/terryyin/lizard/pull/592) (Groovy sources),
[#595](https://github.com/terryyin/lizard/pull/595) (Swift/Kotlin/PHP keyword members),
[#596](https://github.com/terryyin/lizard/pull/596) (Twig templates),
[#598](https://github.com/terryyin/lizard/pull/598) (Ruby regex after keyword),
[#600](https://github.com/terryyin/lizard/pull/600) (JS/TS `page.goto` cognitive),
[#601](https://github.com/terryyin/lizard/pull/601) (JS/TS keyword method/key names),
[#602](https://github.com/terryyin/lizard/pull/602) (`//` comment end outside C family),
[#603](https://github.com/terryyin/lizard/pull/603) (nested generic `?` not ternary),
[#604](https://github.com/terryyin/lizard/pull/604) (C# verbatim identifiers),
[#605](https://github.com/terryyin/lizard/pull/605) (Scala function body end),
[#606](https://github.com/terryyin/lizard/pull/606) (extension state reset per file),
[#607](https://github.com/terryyin/lizard/pull/607) (JS U+2028/U+2029 in strings).

Draft (review disposition; do not merge while draft):
[#590](https://github.com/terryyin/lizard/pull/590),
[#593](https://github.com/terryyin/lizard/pull/593),
[#594](https://github.com/terryyin/lizard/pull/594),
[#597](https://github.com/terryyin/lizard/pull/597),
[#599](https://github.com/terryyin/lizard/pull/599),
[#608](https://github.com/terryyin/lizard/pull/608),
[#609](https://github.com/terryyin/lizard/pull/609),
[#610](https://github.com/terryyin/lizard/pull/610),
[#611](https://github.com/terryyin/lizard/pull/611).

Open issues updated in the same window: none. lizard-process-issue applied
with an empty issue set.

Full lizard-process-pr scope applies: backlog ownership (this story),
evidence packets, direction check, behavior/consumer review, performance
qualification where required, corrections when reachable, and an evidenced
disposition per PR.

**Key examples:**

- #600: `page.goto(...)` does not inflate cognitive complexity as a goto.
- #601/#595/#591: methods/keys named like keywords (`catch`, `if`, `end`)
  report correct CCN and nesting.
- #606: analyzing two files in one run does not leak extension state.
- #582: implied-semicolon scan meets single-extension ND/NS cost bars or
  stays unmerged with measured evidence.
- #592/#596/#589: new readers report functions for their file types without
  changing public CLI signatures.
- Drafts: leave open with a short review note; merge only after undraft.

**Source:** Dashboard instruction to run lizard-process-pr and
lizard-process-issue on PRs and issues updated in the past 48 hours
(launch `cea9f0e7-5772-4dab-a8ef-0a0e54fd4396`).
