# JavaScript labels and TypeScript type boundaries

Runtime labels are statements. Type annotations own their nested brackets,
commas and function-type arrows and return enclosing runtime syntax exactly
once. Type-only class fields and assertions produce no runtime functions;
actual initializers, callbacks and successors retain their names, formals,
spans and metrics.

Contextual `as`/`satisfies` identifiers remain runtime operands at statement and
expression starts. Genuine assertions follow completed operands. Optional
member punctuation uses the established metric protocol; nested template,
generic function, import and query types retain their type ownership across
JS, TS, JSX, TSX, MJS, CJS and Vue readers.

Regression documentation is in
[annotation boundaries](../../../test/test_languages/testTypeScriptAnnotationBoundaries.py)
and [assertion roles](../../../test/test_languages/testTypeScriptAssertionRoles.py).
The two modules cover 490 cases. Full pytest and make pass 2917 tests with 7
existing skips. All 18 measured current/cumulative comparisons have paired 95%
upper slowdown below 3%; the highest is 2.7992%.

[PR #548](https://github.com/terryyin/lizard/pull/548) is merged at `2416c37`.
[Issue #524's resolution summary](https://github.com/terryyin/lizard/issues/524#issuecomment-6569296813)
links the related fixes and confirms merged-source verification.

[Qualification evidence](https://github.com/terryyin/lizard/tree/72d0e8cda9c7789ac7c45a5500e8fec0b4c67bf8/docs/reviews/issue524-label-types-20261102)
retains the frozen inputs, raw paired timings, failed observations, independent
reviews and verification receipts in Git history.
