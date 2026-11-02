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
The two modules cover490 cases. Full pytest and make pass2917 tests with7
existing skips. All18 measured current/cumulative comparisons have paired95%
upper slowdown below3%; the highest is2.7992%.

[PR548](https://github.com/terryyin/lizard/pull/548) is merged at2416c37.
[Issue524's resolution summary](https://github.com/terryyin/lizard/issues/524#issuecomment-6569296813)
links the related fixes and confirms merged-source verification.
