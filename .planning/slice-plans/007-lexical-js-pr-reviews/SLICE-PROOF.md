# Provisional proof mapping

Historical observations are claims to refresh during story refinement and execution.

### 1. JavaScript line-comment termination — PR #544

Source: https://github.com/terryyin/lizard/pull/544

Baseline observation: b has CCN 3; expected 4, with NLOC 8.

Proof: A backslash does not extend a JS line comment into the next line. Preserve C/C++ continuation behavior if shared comment logic changes. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 2. Conditions in template interpolations — PR #545

Source: https://github.com/terryyin/lizard/pull/545

Baseline observation: label has CCN 1; expected 3.

Proof: Interpolation expressions contribute CCN in JS, TS, JSX and TSX while balanced scanning preserves literal text, nested templates, regex literals and function boundaries. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 3. Keyword member names — PR #546

Source: https://github.com/terryyin/lizard/pull/546

Baseline observation: load has CCN 2; expected 1.

Proof: Member names catch/if/for/while/case add no conditions; corresponding control-flow constructs still do. Inspect condition-counter consumers and optional member access. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 4. Ternary and optional markers — PR #547

Source: https://github.com/terryyin/lizard/pull/547

Baseline observation: pick has CCN 1; expected 2.

Proof: A ternary without spaces counts in JS/TS; optional fields/parameters do not add conditions in TS/TSX. Preserve optional chaining and nullish coalescing. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.
