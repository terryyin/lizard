---
id: SEED-036
status: active
planted: 2026-11-05
planted_during: Terry Yin's request to remove local proof across maintained projects
trigger_when: A Lizard developer maintains website test proof or test execution
scope: story
---

# SEED-036: Remove local proof from Lizard

<a id="remove-local-proof"></a>

### Remove local proof from Lizard

**Identity:** SEED-036#remove-local-proof
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/023-remove-local-proof/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"9091df30ee675e42859f82f1a3036850930778b2b6dd9f3b81d2c8e06bf4e144","plan":"804d95021ecb037268341915bd8618cab17acda76699d683cf28545d8c4ef719"}}
```

**Goal.** A Lizard developer who maintains the website and its tests works in a
smaller repository that no longer carries a local Cucumber proof collection or
persistence feature, while ordinary local tests and GitHub Actions CI proof
continue to serve their existing purposes.

**Scope.** Delete the local website Cucumber proof feature outright from this
repository: the retained snapshot tree, the recorder/plugin and refresh
implementation, the deliberate recording path, dedicated proof tests and their
exclusive helpers/dependencies, local `proof` / `proofGeneration` settings, and
project-owned documentation or agent guidance that describes that feature.

Required behavior:

- Remove `website/frontend/proof` and every local-proof-only historical
  snapshot, fixture record or copied artifact. Keep no archive or replacement
  history store.
- Remove `website/frontend/proof-plugin.js`,
  `website/frontend/proof-snapshot.js`, the `RECORD_CUCUMBER_PROOF` collection
  path, Cucumber plugin wiring, recording commands and the dedicated
  `test:proof` script. Remove local `proof` and `proofGeneration` settings from
  `.planning/open-dough.json`, preserving the Intent source location and CI
  proof entry identities (`test` and `website/frontend/features`).
- Remove the dedicated proof-plugin and proof-refresh tests and every helper,
  fixture, mock, dependency or package entry used only by the feature (including
  exclusive support such as `proper-lockfile` when nothing else needs it). Trace
  exclusive dependencies until no orphaned support remains.
- Remove local-proof documentation, comments and project-owned agent guidance.
  Delete the feature with no disabled implementation, compatibility reader,
  opt-in switch, stub, historical feature narrative or migration guide. Spent
  story/plan records for this work are cleaned at closure, not as a substitute
  for deleting the product feature.
- Preserve analyzer and website behavior, ordinary local test execution
  (including `test:e2e` pass/fail semantics, reports and diagnostics), and
  GitHub Actions' native test reports, CI proof fragments, summary artifacts and
  the dashboard contract established by
  [Publish Lizard CI proof for Intent](../done/SEED-035_publish-lizard-ci-proof.json)
  / the CI proof publisher under `scripts/ci-proof/`. Shared code remains only
  where a surviving test or CI responsibility needs it; delete local-proof
  collection and persistence entry points.
- Receive shared Open Dough guidance removal through Open Dough's released
  update rather than editing its installed managed copies. Open Dough's own
  story owns published guidance and payload cleanup.

Deferred promises: richer CI proof than today's contract; any replacement local
verification archive; changing Intent entry identities beyond dropping
local-proof fields. Rejection constraints: do not leave a disabled or stubbed
local-proof path; do not invent a permanent absence checker in place of deleting
dedicated feature tests; do not weaken or replace CI proof publication; do not
edit installed managed Open Dough guidance in this story.

Assumptions: CI proof (report → fragment → summary → dashboard) is a separate
surviving responsibility from the deleted local recorder; ordinary review
evidence and CI run artifacts keep their actual surviving purpose and need no
local fallback.

**Key examples.**

- Given the repository still has `website/frontend/proof`, the recorder modules,
  `RECORD_CUCUMBER_PROOF` wiring and `test:proof`, when this story completes, those
  paths, scripts and settings are gone, ordinary `test:e2e` still runs without a
  proof plugin, and `.planning/open-dough.json` still names the Gherkin Intent
  location without `proof` or `proofGeneration`.
- Given GitHub Actions already publishes CI proof fragments and a summary for the
  existing Intent entries, when local proof is removed, that workflow still
  produces fragments and a summary for those same entry identities; the
  Cucumber proof-recorder job step that only ran `test:proof` is gone with the
  feature.
- Given a developer searches the maintained product tree after deletion, local
  proof collection/persistence references, exclusive fixtures and orphaned
  dependencies are absent, while analyzer tests, website tests and `scripts/ci-proof/`
  remain. Dedicated feature tests are deleted rather than replaced by a permanent
  absence checker; a one-time whole-product reading/search is the deletion audit.
