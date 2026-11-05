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
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

**Goal:** A Lizard developer maintains a smaller repository without a local
proof collection or persistence feature, while ordinary tests and CI proof
continue to serve their existing purposes.

**Requirements:**

- Delete `website/frontend/proof` and every local-proof-only historical
  snapshot, fixture record or copied artifact. Keep no archive or replacement
  history store.
- Delete `website/frontend/proof-plugin.js`,
  `website/frontend/proof-snapshot.js`, the `RECORD_CUCUMBER_PROOF` collection
  path, Cucumber plugin wiring, recording commands and the dedicated
  `test:proof` script. Remove local `proof` and `proofGeneration` settings
  from `.planning/open-dough.json`, preserving the source location and CI
  proof entry identities.
- Delete the dedicated proof-plugin and proof-refresh tests and every helper,
  fixture, mock, dependency or package entry used only by the feature. Trace
  exclusive dependencies until no orphaned support remains.
- Remove local-proof documentation, comments and project-owned agent
  guidance. Delete the feature outright, with no disabled implementation,
  compatibility reader, opt-in switch, stub, historical feature narrative or
  migration guide. Clean up spent story/plan records at closure.
- Preserve the analyzer and website behavior, ordinary local test execution,
  diagnostics and failure semantics. Preserve GitHub Actions' native test
  reports, proof fragments, summary artifacts and the dashboard contract in
  [Publish Lizard CI proof for Intent](SEED-035-publish-lizard-ci-proof.md#publish-lizard-ci-proof).
  Shared code remains only where a surviving test or CI responsibility needs
  it; delete local-proof collection and persistence entry points.
- Receive shared guidance removal through Open Dough's released update rather
  than editing its installed managed copies. Open Dough's own story owns the
  published guidance and payload cleanup.

**Evaluation:** Run the maintained tests for surviving analyzer, website,
ordinary test execution and CI report → fragment → summary behavior. Separately
perform a one-time whole-product reading/search for deleted local-proof
references, artifacts and exclusive dependencies. Delete dedicated feature
tests rather than replacing them with a permanent absence test or checker.

**Boundary:** Complete local-proof deletion. Ordinary review evidence and CI
run artifacts are classified by their actual surviving purpose; this story
creates no local fallback, archive or richer CI proof requirement.
