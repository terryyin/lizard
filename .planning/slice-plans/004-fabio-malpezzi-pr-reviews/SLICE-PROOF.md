# Slice observations and focused proof

Each slice owns its whole disposition under [review contract](REVIEW-CONTRACT.md).
The current executable statuses remain in [PLAN.md](PLAN.md).

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

Proof: An object in an expression-bodied arrow does not close or extend surrounding functions; a function-type return annotation still allows its function body. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 4. Multiline arrow boundaries — PR #533

Source: https://github.com/terryyin/lizard/pull/533

Baseline observation: arrows end at 5 and 7, m has CCN 2, pick ends at 10; expected 4, 6, CCN 3, and line 11.

Proof: Check newline termination versus expression continuation, case labels, async methods after fields, NLOC and token ownership. Verify #531 first; test both with and without semicolons. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 5. Arrows in ternary branches — PR #534

Source: https://github.com/terryyin/lizard/pull/534

Baseline observation: choose contains only one reported arrow; expected two, with CCN 1 and 2.

Proof: An enclosing ternary colon ends the first arrow, while a ternary inside an arrow retains its colon. Verify #531 first. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 6. Identifier parameters — PR #532

Source: https://github.com/terryyin/lizard/pull/532

Baseline observation: render and round each have one parameter; expected three and two.

Proof: Names beginning with _ or $, and names matching TS type keywords, count correctly while annotations do not. Explicitly assess the PR's disclosed lone-$ limitation through FunctionInfo, not just parser-local state. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 7. TypeScript this declarations — PR #535

Source: https://github.com/terryyin/lizard/pull/535

Baseline observation: onClick has two parameters; expected one.

Proof: A leading this type declaration is excluded while actual arguments and body uses of this are preserved. Verify #532 first. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 8. Arrow parameter forms — PR #536

Source: https://github.com/terryyin/lizard/pull/536

Baseline observation: four reported arrows each have zero parameters; expected 2, 1, 2, 2.

Proof: Parenthesized, single-name, ternary-branch and typed-return arrows retain parameter counts and initializer CCN; compare with named arrow controls. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 9. Arrow parameters after member access — PR #537

Source: https://github.com/terryyin/lizard/pull/537

Baseline observation: module.exports.add and load each have zero parameters; expected two.

Proof: Member access before an arrow does not turn its parameters into call arguments. Recheck against #536 and real member calls. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 10. Immediately invoked function boundaries — PR #538

Source: https://github.com/terryyin/lizard/pull/538

Baseline observation: setup ends at line 4 with CCN 1 and m is absent; expected line 9, CCN 2, and m present.

Proof: Nested IIFEs keep outer function and class boundaries, following conditions and methods. Cover arrow and traditional-function IIFEs. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 11. Parenthesized expression metrics — PR #539

Source: https://github.com/terryyin/lizard/pull/539

Baseline observation: total has CCN 1 and merge CCN 2; expected three each.

Proof: An abandoned provisional arrow or method retains the enclosing expression's tokens and conditions; real arrows still own their defaults and body CCN. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 12. Switch label boundaries — PR #540

Source: https://github.com/terryyin/lizard/pull/540

Baseline observation: only a is reported; expected both callbacks as well.

Proof: case/default colons allow callbacks and nested blocks without consuming them as types; TS annotations and ternaries remain distinct. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 13. Get and set member names — PR #541

Source: https://github.com/terryyin/lizard/pull/541

Baseline observation: get/set methods are absent and create ends at 8; expected methods present and create ending at 7.

Proof: Methods, shorthand properties and arrow-valued properties named get/set coexist with actual accessor prefixes and a following function. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 14. Array values and computed names — PR #542

Source: https://github.com/terryyin/lizard/pull/542

Baseline observation: types ends at line 5; expected 7.

Proof: Nested arrays in property values and bracketed computed keys preserve their distinct boundaries and following functions. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 15. Object spread expressions — PR #543

Source: https://github.com/terryyin/lizard/pull/543

Baseline observation: onClick is absent and a callback is named withDefaults as a method.

Proof: Spreads of parenthesized expressions and calls retain nested callbacks without creating fictitious object methods. Recheck the overlap with #539. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 16. JavaScript line-comment termination — PR #544

Source: https://github.com/terryyin/lizard/pull/544

Baseline observation: b has CCN 3; expected 4, with NLOC 8.

Proof: A backslash does not extend a JS line comment into the next line. Preserve C/C++ continuation behavior if shared comment logic changes. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 17. Conditions in template interpolations — PR #545

Source: https://github.com/terryyin/lizard/pull/545

Baseline observation: label has CCN 1; expected 3.

Proof: Interpolation expressions contribute CCN in JS, TS, JSX and TSX while balanced scanning preserves literal text, nested templates, regex literals and function boundaries. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 18. Keyword member names — PR #546

Source: https://github.com/terryyin/lizard/pull/546

Baseline observation: load has CCN 2; expected 1.

Proof: Member names catch/if/for/while/case add no conditions; corresponding control-flow constructs still do. Inspect condition-counter consumers and optional member access. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 19. Ternary and optional markers — PR #547

Source: https://github.com/terryyin/lizard/pull/547

Baseline observation: pick has CCN 1; expected 2.

Proof: A ternary without spaces counts in JS/TS; optional fields/parameters do not add conditions in TS/TSX. Preserve optional chaining and nullish coalescing. Run the [JS/TS focused command](REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.
