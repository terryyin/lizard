# Provisional proof mapping

Historical observations are claims to refresh during story refinement and execution.

### 1. Arrow parameters after member access — PR #537

Source: https://github.com/terryyin/lizard/pull/537

Baseline observation: module.exports.add and load each have zero parameters; expected two.

Proof: Member access before an arrow does not turn its parameters into call arguments. Recheck against #536 and real member calls. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 2. Immediately invoked function boundaries — PR #538

Source: https://github.com/terryyin/lizard/pull/538

Baseline observation: setup ends at line 4 with CCN 1 and m is absent; expected line 9, CCN 2, and m present.

Proof: Nested IIFEs keep outer function and class boundaries, following conditions and methods. Cover arrow and traditional-function IIFEs. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 3. Parenthesized expression metrics — PR #539

Source: https://github.com/terryyin/lizard/pull/539

Baseline observation: total has CCN 1 and merge CCN 2; expected three each.

Proof: An abandoned provisional arrow or method retains the enclosing expression's tokens and conditions; real arrows still own their defaults and body CCN. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 4. Switch label boundaries — PR #540

Source: https://github.com/terryyin/lizard/pull/540

Baseline observation: only a is reported; expected both callbacks as well.

Proof: case/default colons allow callbacks and nested blocks without consuming them as types; TS annotations and ternaries remain distinct. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 5. Get and set member names — PR #541

Source: https://github.com/terryyin/lizard/pull/541

Baseline observation: get/set methods are absent and create ends at 8; expected methods present and create ending at 7.

Proof: Methods, shorthand properties and arrow-valued properties named get/set coexist with actual accessor prefixes and a following function. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 6. Array values and computed names — PR #542

Source: https://github.com/terryyin/lizard/pull/542

Baseline observation: types ends at line 5; expected 7.

Proof: Nested arrays in property values and bracketed computed keys preserve their distinct boundaries and following functions. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.

### 7. Object spread expressions — PR #543

Source: https://github.com/terryyin/lizard/pull/543

Baseline observation: onClick is absent and a callback is named withDefaults as a method.

Proof: Spreads of parenthesized expressions and calls retain nested callbacks without creating fictitious object methods. Recheck the overlap with #539. Run the [JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof) plus any additional shared consumers reached by the actual change.
