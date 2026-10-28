# PR538 immediately invoked function boundaries

Disposition: **merged** under SEED-010#review-structural-js-prs slice 2 as
[`a2f0a5385b19d7cb6255a8736d09ba9792403390`](https://github.com/terryyin/lizard/commit/a2f0a5385b19d7cb6255a8736d09ba9792403390), with
[acknowledgement](https://github.com/terryyin/lizard/pull/538#issuecomment-6456461311).

Frozen contributor head: `c869db90cf2ab6ba93b5c368b71f66e3eaaa6014`.
Qualified published head recorded in `hosted-publication.json`.

## Claim

A function body that starts with an IIFE then continues must keep the outer
function's end line/CCN and following methods. On current target before
correction, class method `m` after a field IIFE was absent.

## Correction

Count abandoned optimistic parameter `(` / `function` groups via `_open_groups`
so the closing `)` of `(() => …)` / `(function () {})` does not end the
enclosing block. Group lifecycle helpers live in `JavaScriptFunctionsMixin`;
discriminating tests in `testJavaScriptGroupExpressions.py`. Product-edit time
≪15 minutes; post-change refactor consolidated ownership under the 250-line
limit. ADR-0000 / ADR-0001.

## Proof

Focused JS/TS 614 passed / 5 skipped; full pytest 2132 passed / 7 skipped;
`make` ok; required CI on exact head; six performance workloads pass
(cumulative-cli at 4.0s).
