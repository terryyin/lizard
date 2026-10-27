# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-consumers-history.md` | changed path |
| `slice2-consumers-history.md` | basename of `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-consumers-history.md` |
| `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-consumers.md` | changed path |
| `slice2-consumers.md` | basename of `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-consumers.md` |
| `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-execution-context.md` | changed path |
| `slice2-execution-context.md` | basename of `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-execution-context.md` |
| `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-proof.md` | changed path |
| `slice2-proof.md` | basename of `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-proof.md` |
| `.planning/slice-plans/012-arrow-parameter-bindings/PLAN.md` | supplied |

## Scope

Whole checkout at `33610866db00`, including uncommitted and untracked files: 1372 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-consumers-history.md`, `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-consumers.md`, `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-execution-context.md`, `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-proof.md`.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

none found

### Test

none found

### Test support or stand-in

none found

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.planning/slice-plans/012-arrow-parameter-bindings/PLAN.md` | `docs/reviews/fabio-malpezzi-20261027/pr536/slice2-execution-context.md`, `slice2-execution-context.md` | 39: `rationale are retained in [slice2-execution-context.md](../../../docs/reviews/fabio-malpezzi-20261027/pr536/slice2-execution-context.md).` | run: inspected the active plan's relative context link; structural comparison preserves all five ordered slices/statuses, proof gates and canonical G1-G7 verbatim; moved context links resolve |

## Refactor observations

Only documentation placement changed. The original generated consumer-table rows
and dispositions were compared as a multiset and preserved exactly across
`slice2-consumers.md` and `slice2-consumers-history.md`. The active plan retains
its outcome, executable slices, statuses, proof gates and canonical obligations;
detailed execution/PFE/preparation material is in `slice2-execution-context.md`.
Product/test/input hashes still match `slice2-source-hashes.json`; accepted API,
CLI, formatter, warning, duplicate-list and website proof remains applicable.
Tests skipped: no product or test edits. Required later-slice proof is unchanged.

The first guarded split refused an oversized combined archive before writing;
splitting along the generated Production category satisfied the size bound.
