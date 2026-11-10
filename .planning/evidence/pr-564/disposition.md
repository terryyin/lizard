# PR #564 disposition

- **Decision:** merged
- **Merge:** `f4c7f72883da56538298332647d4c342292ada42`
- **Final head:** `05a44d255bf419db1c549bb05f92d40349502695`
- **Base:** `c99b67a136356fd3b8e8348e88bebfdd902324db`
- **Acknowledgement:** https://github.com/terryyin/lizard/pull/564#issuecomment-6763552346
- **Owner:** SEED-053#process-pr-564
- **Corrections on contributor branch:** merged current `master`; added
  discriminating regression tests for nested Rust block comments and
  TypeScript `f<T>(...)` generic calls.
- **Proof:** local full pytest 3119 passed / 7 skipped; `make` pylint 9.19;
  hosted Python application workflow success on final head
  (https://github.com/terryyin/lizard/actions/runs/42787024828).
- **Performance:** frozen 1200-file JS/TS/Rust corpus; api-parameters passed
  the 3% screening bound; api/cli medians were about +1–2% with wider CIs
  under elevated host load. Not treated as a merge blocker for this story.
