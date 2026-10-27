# Structural JS/TS proof mapping

Refreshed during preparation on workspace revision `b263e6d` (2026-10-28).
Historical PR-body baselines are claims; trunk observations below settle
whether the defect remains. Execution refreshes heads/target again before acting.

### 1. Arrow parameters after member access — PR #537

Source: https://github.com/terryyin/lizard/pull/537
Frozen head: `c89470c442d28195bebba45597910c7a481a5043`

Original baseline: `module.exports.add` and `load` each had zero parameters;
expected two.
Trunk observation (`analyze_file` on the PR fixture): `add` param=0;
`load` param=1 — still fails expected two for both.

Proof: Member access before an arrow does not turn its parameters into call
arguments. Recheck against #536 and real member calls. Run the
[JS/TS focused command](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md#focused-jsts-proof)
plus any additional shared consumers reached by the actual change.

### 2. Immediately invoked function boundaries — PR #538

Source: https://github.com/terryyin/lizard/pull/538
Frozen head: `c869db90cf2ab6ba93b5c368b71f66e3eaaa6014`

Original baseline: `setup` ends at line 4 with CCN 1 and `m` is absent;
expected line 9, CCN 2, and `m` present.
Trunk observation: same failure — `setup` 1–4 CCN 1; `m` absent.

Proof: Nested IIFEs keep outer function and class boundaries, following
conditions and methods. Cover arrow and traditional-function IIFEs. Run the
focused JS/TS command plus reached shared consumers.

### 3. Parenthesized expression metrics — PR #539

Source: https://github.com/terryyin/lizard/pull/539
Frozen head: `daa7d2068ad1bbaaa201b9685aa5d21582ec0015`

Original baseline: `total` CCN 1 and `merge` CCN 2; expected three each.
Trunk observation: same — `total` CCN 1, `merge` CCN 2.

Proof: An abandoned provisional arrow or method retains the enclosing
expression's tokens and conditions; real arrows still own their defaults and
body CCN. Run the focused JS/TS command plus reached shared consumers.

### 4. Switch label boundaries — PR #540

Source: https://github.com/terryyin/lizard/pull/540
Frozen head: `d225a6d60af856ac155b882d1a3770b7d66457ef`

Original baseline: only `a` reported; expected both callbacks as well.
Trunk observation: both `(anonymous)` callbacks present and `a` 1–6 CCN 2 —
matches the PR's fixed expectation. Treat as already corrected on trunk unless
execution finds a remaining gap (for example block/template cases in the PR
tests).

Proof: `case`/`default` colons allow callbacks and nested blocks without
consuming them as types; TS annotations and ternaries remain distinct. Run the
focused JS/TS command plus reached shared consumers; record already-fixed
evidence if the whole PR claim set is already satisfied.

### 5. Get and set member names — PR #541

Source: https://github.com/terryyin/lizard/pull/541
Frozen head: `644480ce9f46892fae258696583a6c18649c05dc`

Original baseline: get/set methods absent and `create` ends at 8; expected
methods present and `create` ending at 7.
Trunk observation: same failure — only `after` and `create` (5–8).

Proof: Methods, shorthand properties and arrow-valued properties named get/set
coexist with actual accessor prefixes and a following function. Run the focused
JS/TS command plus reached shared consumers.

### 6. Array values and computed names — PR #542

Source: https://github.com/terryyin/lizard/pull/542
Frozen head: `ddf075a0cdee4079e584919d818c28b6468a84b0`

Original baseline: `types` ends at line 5; expected 7.
Trunk observation: same — `types` ends at 5.

Proof: Nested arrays in property values and bracketed computed keys preserve
their distinct boundaries and following functions. The PR notes `[m[0]]` early
end remains with the bracket-expression reader / #525 (still open, out of this
story). Run the focused JS/TS command plus reached shared consumers.

### 7. Object spread expressions — PR #543

Source: https://github.com/terryyin/lizard/pull/543
Frozen head: `f6a329b488bb1a6e5aa9af43f0cf3e5e761d8353`

Original baseline: `onClick` absent and a callback named `withDefaults` as a
method.
Trunk observation: same — only `withDefaults` as a method; `onClick` absent.

Proof: Spreads of parenthesized expressions and calls retain nested callbacks
without creating fictitious object methods. Recheck overlap with #539. Run the
focused JS/TS command plus reached shared consumers.
