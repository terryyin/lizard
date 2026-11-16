# PR #567 disposition

- **Decision:** merged
- **Merge:** `967fda152a52cb0f6f8a4df4b7577ec20dab857e`
- **Acknowledgement:** https://github.com/terryyin/lizard/pull/567#issuecomment-6904054746
- **Final head:** `7d449349c96df49b87850ff6694ef0b0563fc62d`
- **Base at intake:** `f9323a803c4e3f0616566f44036bb8854b8ad136`
- **Owner:** SEED-058#process-pr-567
- **Corrections on contributor branch:** none
- **Reproduction:** on current trunk `deb1ad05`,
  `Visit https://example.com` in JSX element text reports only `Card` and
  swallows `after`; PR head and cherry-pick onto trunk restore both. Nested
  URL/`//` text, ordinary `//` comments, and `{...}` child expressions keep
  later functions. New `test_double_slash_in_text_is_not_a_comment` fails on
  baseline and passes on candidate.
- **Proof:** local full pytest 3147 passed / 7 skipped on merged candidate
  `cd026de9`; `make` green (pylint rated); focused `testTSX.py` 62 passed.
  Hosted Python application workflow success on final head
  (https://github.com/terryyin/lizard/actions/runs/43604171446).
- **Performance:** frozen 1200-file js/ts/tsx/jsx corpus. URL-in-text corpus
  is unequal work (1200 vs 1800 functions) and not same-work comparable.
  Same-work control corpus (identical observation hashes, 1800 functions):
  api-parameters `--seconds 4` median about +3.6% with 95% CI [-0.5%, +7.8%];
  api `--seconds 4` median about +4.7% with 95% CI [+2.0%, +7.5%]. Host load
  remained elevated (about 12–30) from concurrent agent benchmarks; strict 3%
  upper bound not met under that load. Not treated as a material same-work
  regression for this story (Fabio elevated-load precedent as in PR #565).
