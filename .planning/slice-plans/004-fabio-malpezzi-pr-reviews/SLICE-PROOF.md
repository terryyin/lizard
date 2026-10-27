# Retained first-batch proof

Only original slices 1–8 remain here; other mappings are in
[story-map.md](../../../docs/reviews/fabio-malpezzi-20261027/story-map.md).

### 1. Python nesting boundaries — PR #527

Source: https://github.com/terryyin/lizard/pull/527

Baseline observation: Python NS for a, b, c is 1, 2, 6; expected 1, 1, 0.

Proof: Two successive functions and a comprehension/ternary have independent structural depth; also observe separate files through one analyzer, match/case, async statements and continuation lines. Focused command: `nix develop -c python -m pytest test/testNestedStructures.py test/testNestedStructureScopes.py test/testPythonNestedStructures.py test/test_languages/testPython*.py test/test_languages/testGDScript.py`.

### 2. Regex literal context — PR #528

Source: https://github.com/terryyin/lizard/pull/528

Baseline observation: a and the nested arrow have CCN 1; expected 2 each.

Proof: Regex literals after return and => preserve conditions, function ends and following files; division remains division. Exercise JS, TS, JSX, TSX and Ruby because the decorator is shared. Focused command: `nix develop -c python -m pytest test/test_languages/testJavaScriptRegex.py test/test_languages/testJavaScript.py test/test_languages/testTSX.py test/test_languages/testRuby.py test/testNestedStructure*.py test/testPythonNestedStructures.py`.

### 3. Objects inside arrow expressions — PR #531

Source: https://github.com/terryyin/lizard/pull/531

Baseline observation: box ends at line 4 and layout at 5; expected 2 and 4.

Proof: An object in an expression-bodied arrow does not close or extend surrounding functions; a function-type return annotation still allows its function body. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 4. Multiline arrow boundaries — PR #533

Source: https://github.com/terryyin/lizard/pull/533

Baseline observation: arrows end at 5 and 7, m has CCN 2, pick ends at 10; expected 4, 6, CCN 3, and line 11.

Proof: Check newline termination versus expression continuation, case labels, async methods after fields, NLOC and token ownership. Verify #531 first; test both with and without semicolons. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 5. Arrows in ternary branches — PR #534

Source: https://github.com/terryyin/lizard/pull/534

Baseline observation: choose contains only one reported arrow; expected two, with CCN 1 and 2.

Proof: An enclosing ternary colon ends the first arrow, while a ternary inside an arrow retains its colon. Verify #531 first. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 6. Identifier parameters — PR #532

Source: https://github.com/terryyin/lizard/pull/532

Baseline observation: render and round each have one parameter; expected three and two.

Proof: Names beginning with _ or $, and names matching TS type keywords, count correctly while annotations do not. Explicitly assess the PR's disclosed lone-$ limitation through FunctionInfo, not just parser-local state. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 7. TypeScript this declarations — PR #535

Source: https://github.com/terryyin/lizard/pull/535

Baseline observation: onClick has two parameters; expected one.

Proof: A leading this type declaration is excluded while actual arguments and body uses of this are preserved. Verify #532 first. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 8. Arrow parameter forms — PR #536

Source: https://github.com/terryyin/lizard/pull/536

Baseline observation: four reported arrows each have zero parameters; expected 2, 1, 2, 2.

Proof: Parenthesized, single-name, ternary-branch and typed-return arrows retain parameter counts and initializer CCN; compare with named arrow controls. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.
