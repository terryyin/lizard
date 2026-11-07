# PR #525 and issue #524

[PR #525](https://github.com/terryyin/lizard/pull/525) is **merged**, with
current-master corrections and independent refactoring, as
[b97f7958](https://github.com/terryyin/lizard/commit/b97f7958bd63bdafce692cf926c70d927410717b).
The original contributor commit remains in its history.
[One thank-you message](https://github.com/terryyin/lizard/pull/525#issuecomment-6561098652)
acknowledges the contribution, material corrections and validation.

The [issue comment](https://github.com/terryyin/lizard/issues/524#issuecomment-6525221776)
specifically names #525. Last week's FabioMalpezzi cohort covered #527, #528
and #531–#547; #525, by leonrosenshein, was outside that cohort. That work
already resolved several original crash examples, while #525 still supplied
the missing callback/subscript-boundary correction.

| Related PR | Relationship |
| --- | --- |
| [#531](https://github.com/terryyin/lizard/pull/531) | Expression arrows containing object operands. |
| [#533](https://github.com/terryyin/lizard/pull/533) | Arrow endings and source-line ownership. |
| [#534](https://github.com/terryyin/lizard/pull/534) | Ternary versus type-annotation ownership. |
| [#536](https://github.com/terryyin/lizard/pull/536) | Binding and formal-parameter ownership. |
| [#545](https://github.com/terryyin/lizard/pull/545) | Shared template-substitution consumer. |

Issue [#523](https://github.com/terryyin/lizard/issues/523) involved a related
exhausted binding tracker but did not settle #524's subscript boundaries.
This merge does not claim to resolve every independent limitation in #524.

## Resulting behavior

The callback in `it('t', () => {\n e(r[0]);\n if(x) g(x);\n});`
retains lines 1–4 and CCN 2, and following functions retain their own metrics.
Chained/optional subscripts, array-valued object properties and template
substitutions preserve their enclosing function. Argument/object commas end
expression arrows before subsequent arguments or members; global generic-call
continuations retain their existing span.

Replayed tokens reach binding tracking once. Actual/provisional formals retain
the binding owner; proven calls reuse ordinary nested syntax with argument-comma
context. No parallel bracket counter or whole-stream buffer was added.

Public regressions retain all eleven authored cases across JS, TS, JSX, TSX,
MJS, CJS and Vue, plus successor metrics, typed/default/ternary formals, templates,
NS/nonstrict, actual CSV CLI and HTTP JSON. Full pytest and make each passed:
**2,427 tests, seven existing skips**. Configured style checks passed; existing
pylint diagnostics are not represented as zero messages.

## Performance and hosted checks

Eight prospectively frozen comparisons used twenty alternating warmed pairs
each. API/parameter and NS batches calibrated to 1.2 seconds; CLI to four seconds.
Every measured batch exceeded one second, with stable source/input/harness hashes
and output observations. Each paired 95% upper slowdown bound is strictly below
3%; the worst is **+0.526%**. Negative change means faster.

| Comparison | Paired change | Paired 95% interval |
| --- | ---: | ---: |
| Current master, complete API/parameters | -5.965% | [-7.093%, -4.824%] |
| Current master, complete NS | -4.951% | [-6.324%, -3.557%] |
| Current master, isolated call API/parameters | -1.130% | [-1.721%, -0.535%] |
| Current master, isolated call NS | -1.449% | [-1.934%, -0.961%] |
| Current master, broad CLI | -1.176% | [-2.328%, -0.010%] |
| Original parent, safe API/parameters | -13.443% | [-14.010%, -12.874%] |
| Original parent, safe NS | -10.335% | [-11.001%, -9.664%] |
| Original parent, broad CLI | -0.442% | [-1.401%, +0.526%] |

Runtime baseline: `6d288d4cdcb85bc50ab4c8a887196177607eeb7d`;
original parent: `01afe9c228ba2a3dc5253259d8415d2ef47f0b71`.
Later master `c694707` changed process guidance only. Qualified contributor head
`e68c735e11564e3afb7b543f8b3ad7cc1a06b3a0` retained all 107 accepted runtime
and 174 test/fixture hashes after history-preserving integration.

Both exact-head hosted matrices (Ubuntu/Windows × Python 3.8–3.14) and both
change-detection jobs passed; both web jobs were correctly skipped by the path
filter. The merge guarded that exact head. The
[merged-master run](https://github.com/terryyin/lizard/actions/runs/41329592732)
also passed its complete applicable matrix.

Pre-existing labeled-body arrow omission, typed-class-field false positive
and anonymous multiline-generic truncation remain documented limitations.
The bounded corpus does not replay the contributor's private monorepo or prove
absence of crashes on arbitrary input.

## Recoverable evidence

[Before-cleanup f5d10b7c](https://github.com/terryyin/lizard/tree/f5d10b7c371b933947d15805f484b33c1b39b0cb/docs/reviews/pr525-correction-20261101)
retains the completed source/plan, implementation and independent refactor
returns, public red/green observations, hashes, frozen manifests, profiles,
full-check logs, all timing attempts, exact-head guard and merge/thanks receipts.
The unfavorable +5.55% isolated-call attempt is preserved; the final correction
removed its unnecessary tracking and passed the unchanged workload.
The captured GitHub packet is losslessly encoded; `hosting/packet-storage.md`
and its verified round-trip inspection explain recovery.

The [initial review evidence](https://github.com/terryyin/lizard/tree/5f7d58fbad166bf05e1510162831bbc03cbabd96/docs/reviews/pr525-issue524-20261101)
retains the original proposal/parent comparison and relevance investigation.
Raw files can be recovered with `git show <revision>:<repository-relative-path>`.
