# PR #571 disposition

**Owner:** SEED-059#process-pr-571-jsx-empty-text-token
**Decision:** Merged
**Head:** bf59d607c9f74dd69781e2e29581dc27ba848392
**Merge:** 60b2fb5933112ddab2bd540cd29236cd5ec3951b
**Ack:** https://github.com/terryyin/lizard/pull/571#issuecomment-6904239426

## Proof

- Reproduced `IndexError` on current trunk with adjacent JSX children + `-Eduplicate`.
- Candidate and master+cherry-pick clear the crash; tokenizer emits no empty `''`.
- Focused TSX tests green; full pytest 3085 passed; `make` exit 0.
- Performance (corpus 950 files; `--seconds 4` after noisy short batches):
  - api-seconds4: pass, upper CI 2.43%
  - api-parameters-seconds4-retry: pass, upper CI 1.25%
  - cli-seconds4: pass, upper CI 2.50%
- Intent coverage: Caught (empty-token flush fault fails both discriminating tests).
