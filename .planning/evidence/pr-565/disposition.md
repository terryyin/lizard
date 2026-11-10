# PR #565 disposition

- **Decision:** merged
- **Merge:** `b676d6f224491aa45578f8493e6aee38c340e6ff`
- **Final head:** `41c3ed6cd32f9d1006fbc103387dbcd3821ddf4f`
- **Base:** `8f0c650bd9dadafcb8e843e401401f765bc4e95c`
- **Owner:** SEED-054#process-pr-565
- **Corrections on contributor branch:** none
- **Proof:** local full pytest 3133 passed / 7 skipped; `make` pylint 9.19;
  hosted Python application workflow success on final head
  (https://github.com/terryyin/lizard/actions/runs/42813896340).
- **Reproduction:** new tests fail on baseline `8f0c650b` (10 targeted failures);
  full `testJava.py` 33 passed on candidate.
- **Performance:** spring-petclinic-scaled corpus finds 1072 → 1520 functions
  (+42%); api-parameters median +12.9% is not same-work comparable. Matched
  control corpus (2000 functions both sides, identical observation hashes):
  api-parameters `--seconds 4` median about +2.4% with 95% CI [-1.5%, +6.4%]
  under elevated host load; microbench about +1.6%. Strict 3% upper bound not
  met under load; not treated as a material same-work regression for this story.
- **Acknowledgement:** https://github.com/terryyin/lizard/pull/565#issuecomment-6768581516
