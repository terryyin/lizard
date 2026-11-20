---
id: SEED-065
status: active
planted: 2026-11-20
planted_during: lizard process pr
scope: Svelte component analysis from PR #575
---

# SEED-065: Process PR #575

<a id="process-pr-575"></a>

### Process PR #575 — Analyze Svelte components (`.svelte`)

**Identity:** SEED-065#process-pr-575
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"8ddaf82328d21b7d6bd60b256c19406789c3f4edc3b12919612f101ad3eeb0d5"}}
```

**Goal:** Maintainers disposing
[PR #575](https://github.com/terryyin/lizard/pull/575), and users scanning
Svelte components, receive one complete disposition: `.svelte` files are
discovered and analyzed through a Vue-derived script-block reader with correct
source line numbers and a cognitive profile, when the contribution remains
valid on current trunk, or the PR stays unmerged with an evidence-backed
reason (including draft unreadiness).

**Source.** Dashboard instruction `lizard process pr` for PRs created or
updated in the past 24 hours. Draft at intake. Full process-PR scope applies.

**Beneficiary.** Maintainers deciding whether to accept Svelte support, and
users analyzing Svelte SFCs the way Vue SFCs already work.

**Scope.** This pull request only. Reproduce directory discovery of `.svelte`,
`<script>` / `<script module>` / `lang="ts"`, reactive statements, rune
callbacks, and markup isolation. Apply language-support conventions. Correct
on a reachable contributor branch when needed. Qualify and merge only when
gates hold, including draft readiness.

Deferred: counting template `{#if}` / `{#each}` complexity, and the
apostrophe-before-script shared Vue limit. Rejection constraints as for other
process-PR owners; do not expand beyond slight extension of existing
Vue-style SFC reading.

**Key examples.**

- Given a directory with only `Add.svelte` containing a script function,
  `lizard .` analyzes that file (trunk reports 0 files).
- Given markup with `{#if}` / `{#each}` / inline handlers after the script,
  script function metrics match the script-only case.
- Given `lang="ts"` and module scripts, functions are reported with source
  line numbers.
- Draft unreadiness or an unworthy change receives a recorded reason instead
  of a merge.

**Direction:** Within "slightly extend the ability of the current behavior"
via a Vue-subclass reader and registration; no change to existing external
signatures or fundamental metric behavior for other languages.

Intake metadata: head `92dc75d56f8dcd829b306ccef1c536ffafee4dc1` on
`AbdelrahmanHafez:svelte-support`; base `master` at `e8d18d2e`; draft;
`maintainer_can_modify` true; MERGEABLE; evidence under
`.planning/evidence/pr-575/`.

**Disposition (2026-11-20):** **Merged** as `2c032f4b` (PR #575). Direction within slight extension; correctness accepted; CI green; existing JS workloads treated as unaffected (noisy plain API screen under load). Comments: https://github.com/terryyin/lizard/pull/575#issuecomment-7030459535 https://github.com/terryyin/lizard/pull/575#issuecomment-7030469281
