# PR #553 — Quoted TypeScript optional member vs ternary

**Disposition:** merged at contributor head
`692a456cfa5448a9435cf4e7c43b31eba8c47701` (no maintainer patch required).
Frozen trunk baseline: `4a05e4056cebb2edd75cb02b7a43e911369f4ce5`.
Incremental target: `eec8c38b226b7a777d0e6d82978d618fde09a65a` (after #552).

## Value

On current target, quoted optional members such as `'retry-after'?: string`
tokenize `?` as a ternary, inflating CCN (`f=4` / `retryAfter=3`). Correct
behavior reports `m=1`, `f=2`, `g=2` so only `||` and a genuine string ternary
add decisions.

## Adaptation

Contributor patch applies as-is: widen `OPTIONAL_NAME` in
`lizard_languages/typescript.py` for quoted string names, plus
`test_optional_quoted_member_is_not_a_ternary`. Independent refactor: none.

## Proof

- Target reproduction: claim red (`f=4`); control ternary green (`g=2`).
- Corrected: all match (`m=1`, `f=2`, `g=2`).
- Focused JS/TS suite: 1385 passed, 5 skipped.
- Full pytest: 3024 passed, 7 skipped.
- make: exit 0.
- Hosted CI: all required builds pass on the exact head.

## Performance

Attempt1 incremental api / api-parameters pass (worst upper CI ~2.59%).
Incremental CLI attempt1 failed under load spike (~10); retained. Attempt2
retries incremental CLI and all cumulative workloads; all pass (worst upper
CI ~2.33% cumulative-api).
