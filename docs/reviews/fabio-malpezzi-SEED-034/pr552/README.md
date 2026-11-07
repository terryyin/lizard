# PR552 TypeScript generic function declarations

**Disposition:** merged at contributor head
`c58534ac54c1f3edbe0fe009322170c4831fcf9f` (no maintainer patch required).
Frozen trunk baseline: `4a05e4056cebb2edd75cb02b7a43e911369f4ce5`.
Incremental target: `b6b6b8c671bab79da1a6c28c61a2076e4735998d` (after #551).

## Value

On current target, `export function first<T>(list: T[])` reports no function
because consuming `<T>` resumes in the global state and loses the open
declaration. Correct behavior reports `first` / `pair<T, U extends object>`
with parameters and end lines, while generic methods and arrows stay intact.

## Adaptation

Contributor patch applies as-is: optional `resume` on
`_consume_generic_type_params`, passed as `self._function` from the declaration
path. Independent refactor: none.

## Proof

- Target reproduction: claim fixtures red; controls green.
- Corrected / raw head: all match.
- Focused (re-established): 1386 passed, 5 skipped.
- Full pytest: 3022 passed, 7 skipped.
- make: exit 0.
- Consumers: `consumer-trace-dispositions.md`.

## Performance

Attempt1 incremental api / api-parameters / cli all pass (worst incremental
upper CI ~2.02%). Cumulative api on attempt1 was noise-contaminated (load ~10);
attempt2 cumulative api / api-parameters / cli all pass; overall worst upper CI
**2.961%**. See `attempt2/summary.json`.
