# PR #506 disposition (SEED-046)

- Head: `5b5608f2b6006043baf39f162251214620ed90ba`
- Base: `0488a260e74e73ceba3149d7c600faac6a3616e6`
- `maintainerCanModify`: true; merge state CONFLICTING/DIRTY
- PR adds only `MAX_CONTENT_LENGTH = 1 MiB` to pre-revival `index.py`
- Trunk already has `MAX_CONTENT_LENGTH = 512 KiB` and `MAX_SOURCE_BYTES = 64 KiB`
  with 413 JSON handling (`f818daf65`, after this PR's base)
- Website limit tests green: source over 64 KiB → 413 and retry; oversized body →
  413 without reflection; source at 64 KiB accepted (4 passed)
- Disposition: **closed unmerged** — already fixed / superseded

Judgment: https://github.com/terryyin/lizard/pull/506#issuecomment-6699790865
