---
id: SEED-028
planted: 2026-11-02
planted_during: Authorized Open Dough 0.3.82 proof migration
trigger_when: Maintainers deliberately refresh successful website scenario proof
---

# SEED-028: Deliberate successful Cucumber proof

<a id="deliberate-cucumber-proof"></a>

### Deliberately refresh successful website scenario proof

**Identity:** SEED-028#deliberate-cucumber-proof
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/018-deliberate-cucumber-proof/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"5b3d35c1ed031b37b8f23d351690138a1fc14702e89c477f6c71067fec82e3b8","plan":"cfb475d82324c19fcb24db1714bfb5bb8bcf6f89da23bfdeefb550ad72eeb1d9"}}
```

**Goal:** Maintainers deliberately retain successful website scenario verification
without ordinary passing or failing test runs changing that retained evidence.

**Scope:** Migrate the existing native Cucumber producer to the installed 0.3.82
contract. Normal runs, including optimization and CI, leave proof byte-for-byte
untouched and create no proof files, directories or locks. An explicit project-owned
option enables generation using the same tests, selection, output, diagnostics and
exit meaning. Gate before proof reads, writes, cleanup or locks. Declare exactly
one nonempty root-relative generation command or project-owned skill invocation
on website/frontend/features in .planning/open-dough.json, including the recording
option, Nix wrapper and actual selection. Document it without automatic commits.

Maintain compact stable snapshots, adopting readable historical run files by
latest-observation-first reduction, with an unsuccessful outcome winning a timestamp
tie before filtering to passes. Explicit focused refresh updates only executed final
attempts: passing scenarios retain source identity, attempts, duration, finish time,
run-start commit and tested feature version; unsuccessful executed final attempts
clear earlier success from every proof file, preserving newer observations and
unexecuted identities. Intermediate retry failures do not clear proof. Persist no
failure observation or marker. Delete empty consumed files and atomically replace
snapshots with stable identity ordering. Serialize concurrent checkout mutations
without restoring another run's removed success; interrupted runs keep completed
updates. Preserve separate-worktree identity cleanup during Git reconciliation.
Dry runs and fail-fast unexecuted scenarios leave their earlier proof intact.
Recording remains silent and best effort, preserving existing progress/HTML reports.

**Key examples:** Ordinary pass and failure preserve existing proof and leave a
missing proof directory absent. Explicit focused success updates that identity and
preserves other historical identities. A final explicit failure clears its old
success, while a retry that finally passes records cumulative attempts. Legacy
later failures (including ties) invalidate older successes during adoption.
Concurrent pass/cleanup operations preserve both changes; interrupted generation
retains finished updates. Generation remains opt-in in every profile and normal CI.

**Exclusions:** No new runner/framework adapter, website application release or
deployment. Retained proof and CI verdicts remain independent; no local-proof/CI
comparison. Preserve managed guidance and unrelated or concurrent work.

**Execution authority:** Terry's current handoff authorizes implementation,
validation, independent refactoring, landing and final CI, including ownership of
installation revision 210ae3633086b989bea7fa4170268d6db3f4c711's CI. Follow
[Open Dough's failing and flaky test policy]
(/Users/terryyin/git/open-dough/AGENTS.md#failing-and-flaky-tests) as written
together with Lizard's AGENTS.md, carrying this link into every handoff.
