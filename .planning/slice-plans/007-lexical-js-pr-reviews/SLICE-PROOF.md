# Proof mapping — lexical/condition PR reviews

Refresh each PR head/target during execution. Target-baseline observations below
were taken on preparation revision `d6be477c37eea80bdf1423ed35e15338db398968`.

### 1. JavaScript line-comment termination — PR #544

Source: https://github.com/terryyin/lizard/pull/544  
Frozen head: `3ac7e4eaf61e898836fded88ab715b37f9fedbbf`

Target baseline: `b` has CCN 3 / NLOC 7; expected CCN 4 / NLOC 8 after a
correct disposition that accepts the fix.

Proof: A backslash does not extend a JS/TS/JSX/TSX line comment into the next
line. Preserve C/C++ continuation when shared comment logic changes. Run the
[JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof)
plus every shared consumer the actual change reaches; then full suite, `make`,
required CI, and the rest of the complete cycle.

### 2. Conditions in template interpolations — PR #545

Source: https://github.com/terryyin/lizard/pull/545  
Frozen head: `baa95a4df8278421bb3d3a6487516030cc03a979`

Target baseline: `label` has CCN 1; expected 3 when interpolation conditions
count.

Proof: Interpolation expressions contribute CCN in JS, TS, JSX and TSX while
balanced scanning preserves literal text, nested templates, strings, comments,
regex literals and function/state-token boundaries. Same focused command plus
reached consumers and complete-cycle gates.

### 3. Keyword member names — PR #546

Source: https://github.com/terryyin/lizard/pull/546  
Frozen head: `a671632141127fca9b2785fdbb0c675d45ae9042`

Target baseline: `load` has CCN 2; expected 1 when `.catch` is not a condition.

Proof: Member names `catch`/`if`/`for`/`while`/`case` (after `.` or `?.`) add
no conditions; corresponding control-flow constructs still do. Inspect
condition-counter consumers. Same focused command plus reached consumers and
complete-cycle gates. Residual keyword-member-inside-interpolation after #545
is deferred unless a ≤15-minute coherent fix is owned in-cycle.

### 4. Ternary and optional markers — PR #547

Source: https://github.com/terryyin/lizard/pull/547  
Frozen head: `2766557096daac3f8e55657e9c48ba685157635d`

Target baseline: `pick` has CCN 1 (expected 2); TSX `m` has CCN 3 (expected 1).

Proof: Unspaced ternaries count in JS/TS; optional fields/parameters do not add
conditions in TS/TSX/JSX. Preserve optional chaining and nullish coalescing.
Include `test/test_languages/testOptionalOperators.py` with the focused set
plus reached consumers and complete-cycle gates.
