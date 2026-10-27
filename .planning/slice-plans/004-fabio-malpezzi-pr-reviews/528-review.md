# PR528 disposition

Disposition: deferred, remains open/unmerged at
b66ec3a7f3ca1b3f59d8a76891ce556406231566. Refreshed after review.
Canonical receiving story: [Recognize regex literals without consuming division expressions](../../seeds/SEED-012-regex-literal-context.md#regex-literal-context),
SEED-012#regex-literal-context, added first in Backlog list through the installed
product-backlog command. This is the review source's authorized >15minute branch,
not acceptance of a partial fix or a discarded real problem.

## Source comparison and proof acceptance

Source-only reviewer source_review_2 read only the selected seed and returned
individual commitments, exact qualifications and journey alternatives in the
conversation. Its report was independent of the plan, implementation and proof.
Against those commitments:

- Original report → CCN2 in return/arrow regex contexts, correct boundaries.
  Current target gives1; raw proposal gives2. Worthwhile user defect reproduced
  through actual analyze_file; no fixtures supply the metric and no parsing mocks.
- Valid member/contextual-name division → preserve CCN2 and tokens. Raw proposal
  regresses2→1 and24→18tokens in JS-family readers,22→16 in Ruby. These explicit
  counterexamples contradict full correctness despite authored CI success.
- Whole affected design → shared REGEX_TOKEN reaches TS/JS, JSX/TSX, Ruby,
  balanced templates and inherited Vue. Simple word boundaries cannot distinguish
  member names/comments or contextual identifiers. A shared correction with
  regression/refactor work is estimated30–45minutes; no implementation started.
- Before/after and cumulative performance →49related API cases,149fixed inputs,
  same interpreter/options,20warmed alternating pairs. API slowdown+19.4428%,95%CI
  [+18.9053%,+20.3307%], failing strict<3%. Broad CLI passes, and does not mask API.
  Unchanged-output profile localizes the additional sampled cost to token matching.
- >15minute outcome → a separate canonical story records beneficiary, original and
  counterexample reproducers, expected results, remaining work and source PR link.
  The performance and correctness failures remain explicit receiving obligations.
- Final corrected tests/CI, exact-head merge and thank-you → conditional on a
  satisfactory candidate; none is accepted or merged here. Future implementation
  and proof belong to the linked story, which retains the original merge gates.
- Other17remaining PRs → their ordered slices own their respective dispositions.
  No missing promise was silently passed to an unspecified future owner.

Full patch, metadata, discussion, literal commands, explicit observing expectations,
terminal reproduction logs, raw timings, manifest and bounded profile are retained
in [durable evidence](../../../docs/reviews/fabio-malpezzi-20261027/pr528/review-findings.md).
The original reported cases plus all listed division variants are asserted by
reproduce.py; baseline and candidate exit1 for their different actual defects.
No product/test change, selective formatting, hook lint, hosted write or merge
was performed. Vue-specific and ENS performance were not separately measured;
the receiving story explicitly owns corrected-candidate proof for those consumers.

## Current target and identity evidence

Measurements/reproduction target was e871ae4, cumulative baseline2e21e40. The
target advanced during review to7ac449f59b3f155fd159ce9aad6794a422045632 through the
separate Python-support story. Inspected diff changes CI/support claims, records
and one metadata test; analyzer/readers/extensions and benchmark inputs are
unchanged. Retained behavioral/default-path measurements therefore still apply.
Future merge validation must use refreshed Ubuntu/Windows×Python3.8–3.14 matrix
(14jobs), with pip install . replacing setup.py install. No old four-job success
is claimed as current-base acceptance.

Identity evidence ran against both execution HEADaf27c910 and clean read-only
trunk checkouts at e871ae4 and then7ac449f, before publication. Every source read,
no gaps; highestSEED-011/nextSEED-012, candidate regex-literal-context unlisted with
no history seed/record. The new identity is recorded once in its canonical seed.

Final independent refactor_528 review accepted149hashes,49input mappings, shared
harness identity, recomputed intervals/profile metrics and canonical links; no
edits or uncovered consumers, ## REFACTOR COMPLETE. Current product's full
`nix develop -c python -m pytest` remains green:1687passed,7skipped in3.06s
(/tmp/lizard-slice2-pytest.log). Prior make proof is reused because no production
source or product tests changed. Story-obligations check slice2 passed,2entries.
Guarded fresh delivery accepted16eed2248cf873d4b7bec42792198378a4e73ad8 on the
execution branch, suffix base af27c910; existing observer reused, no rebase.
