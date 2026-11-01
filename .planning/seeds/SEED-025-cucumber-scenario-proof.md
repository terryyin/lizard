---
id: SEED-025
planted: 2026-11-02
planted_during: Authorized adoption of Open Dough 0.3.81 Cucumber proof
trigger_when: Ordinary website Cucumber runs need published scenario observations
---

# SEED-025: Website Cucumber scenario proof

<a id="cucumber-scenario-proof"></a>

### Record ordinary Cucumber scenario proof

**Identity:** SEED-025#cucumber-scenario-proof
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/017-cucumber-scenario-proof/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"2e641e655f56b982135643cb99ab6948619bc0000d87097356ca965865ddd723","plan":"493eedfe06a4fdc9844b0e4dc909d5f26a94b2797ce97d96afd557c22d72584c"}}
```

**Goal:** Maintainers see the latest observed result and tested version beside
the website's Gherkin scenarios in Intent after ordinary focused or full runs.

**Scope:** Project-owned Cucumber.js plugin, existing frontend configuration,
repository-relative proof declaration, runner-level regression coverage and real
committed proof. Preserve progress/HTML reports, diagnostics, status, retries and
parallel runs. Capture only executed final attempts in declaring locations,
outline source identities, run-start Git commit and actual feature bytes; write
unique atomic per-run files silently and best effort, retaining finished work
on interruption. Dry runs produce no observations. Validate and land to
origin/master under the current authorized implementation-and-landing request.
Managed Open Dough files and unrelated parser work remain outside this change.

**Key examples:** Focused and full ordinary runs write version-1 proof; retries
produce one final observation with cumulative attempts/duration; outlines retain
unexpanded names and parsed cells; dry-run and fail-fast-unexecuted scenarios
write none; concurrent/interrupted runs preserve their own completed scenarios.

**Failure policy:** Carry and follow [Open Dough's failing and flaky test policy]
(/Users/terryyin/git/open-dough/AGENTS.md#failing-and-flaky-tests) as written,
together with this project's AGENTS.md. Failing/flaky local or CI checks remain
owned defects through the applicable final CI, including failures on the base;
an exact repair already owned by another active execution stays with that owner.

**Identity evidence:** At 9c787424, identity-evidence reports highest SEED-024
and unused SEED-025. Its sole gap was inspected directly: SEED-024's readable
home lacks front matter but explicitly records SEED-024#resolve-issue524-label-types,
agreeing with backlog/history/profile. No ambiguous identity or unreadable
record remains; preserve the active owner's file.
