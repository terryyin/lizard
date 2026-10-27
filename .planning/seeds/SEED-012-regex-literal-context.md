---
id: SEED-012
status: active
planted: 2026-10-27
planted_during: FabioMalpezzi PR review, PR528 disposition
trigger_when: Correct regex literal handling across the shared readers
scope: medium
---

# SEED-012: Regex literal context

<a id="regex-literal-context"></a>

### Recognize regex literals without consuming division expressions

**Identity:** SEED-012#regex-literal-context

**Beneficiary:** Users measuring JavaScript, TypeScript, JSX/TSX and Ruby whose
regex literals or division expressions currently hide conditions or boundaries.

**Goal:** Recognize regex literals after genuine expression-start contexts while
preserving valid division, correct public metrics and function/file boundaries.

**Source:** [PR528](https://github.com/terryyin/lizard/pull/528), reviewed at
`b66ec3a7f3ca1b3f59d8a76891ce556406231566` against target
`e871ae448d16893301856b41124aba34496e9536`. The report is real and worthwhile;
the proposal is deferred and remains unmerged. Correction and coherent regression
cleanup are estimated at30–45minutes, above the review mission's15minute cutoff.
No correction was attempted under that mission.
Target later advanced to7ac449f through CI/support-claim maintenance; inspection
confirmed no analyzer/reader/extension changes, so these observations still apply.
Future required CI uses Ubuntu/Windows with Python3.8–3.14 (14matrix jobs).

**Reproducers and expected results:**

```js
function a(s) { return /#/.test(s) || s === 1; }
function b(xs) { return xs.filter(s => /"/.test(s) || s === "a"); }
function divide(obj,a,b,c) { return obj.return / (a || b) / c; }
```

The first function and nested arrow require CCN2. The current target reports1;
the proposed patch produces2. But divide requires CCN2/token24 and regresses to
CCN1/token18 under the patch in all four JS-family extensions. Spaced,
comment-separated and optional member accesses, valid dollar-prefixed names and
contextual of/await identifiers have the same regression. Ordinary division
without a keyword-like name remains unchanged and serves as a control.

```ruby
def a(s)
 return /#/ =~ s ? 1 : 0
end
def divide(obj,a,b,c)
 obj.return / (a || b) / c
end
```

Ruby a requires CCN2 (target1, proposal2); divide requires CCN2/token22
(proposal1/token16). Existing split/gsub regex behavior must be preserved.

Run the [public API reproducer](../../docs/reviews/fabio-malpezzi-20261027/pr528/reproduce.py)
with `nix develop -c python <reproducer> --checkout <absolute-checkout>`.
It calls analyze_file on real source and checks explicit CCN expectations.
Both target and proposed revision fail for their different defects; their
terminal observations are retained beside the reproducer.

**Scope and remaining work:**

- Distinguish actual expression-start keywords from member names, dollar names
  and contextual identifiers, including intervening whitespace/comments.
- Reuse js_style_regex_expression as the owner and keep its lexical decision
  coherent with javascript_templates' balanced scanning. Trace TS/JS, JSX/TSX,
  inherited Vue behavior and Ruby; do not add a parallel regex recognizer.
- Preserve literal character classes, escapes, quotes/backticks, braces, flags,
  division, template boundaries, following functions and NS lifetime across files.
  Interpolation condition counting itself belongs to PR545's separate review.
- Add discriminating regression tests through public analysis. Review the full
  affected concept and independently refactor into a minimal coherent solution.
- Remove the measured affected-input slowdown, then remeasure corrected code
  against its current target and the review's frozen starting baseline as relevant.
  Preserve representative controls and unrelated language behavior.
- Require full pytest, make and final hosted CI before a guarded exact-head merge;
  publish necessary corrections to the contributor head and acknowledge them.

**Retained performance and diagnosis:** The raw proposal fails the affected-input
gate:20warmed alternating pairs,49related cases, median+19.4428%, paired95%CI
[+18.9053%,+20.3307%]. Broad fixed-source CLI is-1.0137%, interval[-1.7114%,+.2591%].
An unchanged-output5000analysis profile has identical public metric tuples and
180000token-span calls on both sides, with tokens_in_span selftime increasing
.065343s to.106154s. This supports extra lexical matching overhead independent
of newly visible tokens; it is diagnostic evidence, not a second timing verdict.

Literal commands, source/head/base,149-file manifest,49fixed cases, bounded
profiles and raw timings are retained in the [review evidence](../../docs/reviews/fabio-malpezzi-20261027/pr528/review-findings.md).
Manifest SHA256:2b4d99de48defbca46ef3b55568e45edd3f19d7f8366ded49b51e136cbf5d7cc.
Future corrected code must have a95%upper bound on slowdown strictly below3%
for affected and broader workloads, including cumulative effects. Vue-specific
and ENS performance were not separately established by the raw-proposal review.

**Completion:** The report and all division/lexical boundaries above produce
correct observable metrics, shared consumers remain coherent, and the corrected
revision passes correctness, quality, strict performance, full tests and hosted CI.
No implementation is authorized merely by this queued story.
