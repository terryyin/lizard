# PR #446 disposition

- Owner: SEED-049#process-pr-446
- Contributor head (final): `fd22403a9a27be3b766b260196c8e1974c28211b` on `Ollec:feature/document_extentions`
- Original commit retained: `dd2f37a09ef5c96e2f1dc7fbcdf13bfdde99e906`
- Maintainer updates: merged current `master`, then corrected the catalog:
  - Nesting Depth examples load `-END` before `--ND` (bare `--ND` fails)
  - Duplicated parameter-list flag is `-Eduplicated_param_list`
  - Added Cognitive Complexity (`-Ecognitive` / `--CogC`) and Halstead (`-Ehalstead`)
  - README.rst links `lizard_ext/README.md` from the existing Extensions and
    warning controls section (no second competing section)
- Local qualification on candidate: `pytest` 3089 passed, 7 skipped; `make` green
- Performance: unaffected (documentation and images only; no runtime path change)
- Hosted `Python application` / `ci-proof` / `changes` on final head: success
- Packet: `.planning/evidence/pr-446/terryyin--lizard--pr446/ba1dc266bde3b2c5d3e477cf010388f41bf331beb4d6e8ff15fb6941095a4d6c`

- Merged as `d0c8bc12191944547f234091fce14ae6d48c8721` (acknowledgement:
  https://github.com/terryyin/lizard/pull/446#issuecomment-6702464871)
