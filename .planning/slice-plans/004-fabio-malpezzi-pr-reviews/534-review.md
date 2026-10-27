# PR #534 — ternary arrow boundaries

**Disposition:** Defer to
[SEED-015#ternary-arrow-boundaries](../../seeds/SEED-015-ternary-arrow-boundaries.md#ternary-arrow-boundaries).
The real reported benefit is retained, but a new internal-colon regression,
unresolved typed branches and unproved performance gates prevent acceptance.
Coherent correction and cleanup are estimated **30–45 minutes**, excluding
review/measurement. No product correction, commit or hosted write was attempted.

Source: https://github.com/terryyin/lizard/pull/534.
Frozen and refreshed open head: `f4074ae860c7999cc37dd081a6f7ebfa777adc83`.
Base: `01afe9c228ba2a3dc5253259d8415d2ef47f0b71`.
Execution snapshot: `7186fdbe826bf32b399165cc3d36897a128d4d0d`.
Fetched trunk: `7ac449f59b3f155fd159ce9aad6794a422045632`; analyzer sources match.
Earlier raw #528/#531/#533 production changes remain absent from this checkout.

**Observable results:**

- The complete original `choose` example reports one arrow on target, CCN 2.
  Raw and independent child report two arrows at 2–2, CCN 1 and 2, followed
  by choose spanning 1–4 with CCN 2, across JS/TS/JSX/TSX/Vue.
- Internal simple ternaries and nested branch ternaries work on raw/child.
  A curried internal ternary `x => d ? y => y : z => z || x` regresses:
  target reports the established outer arrow f CCN 3; raw/child report f CCN 2
  plus an extra anonymous CCN-2 arrow. Preserve the existing outer-only curried
  convention (`testES6.py::Test_ES6_multiple_arrow_patterns::test_nested_arrows`).
- `c ? (x: number): number => x : (y: number): number => y || d` still reports
  one arrow CCN 2 instead of two CCN 1/2. Genuine return-type colons consume
  pending runtime ternary depth before the arrow starts. TS/TSX/Vue observations
  retain this unresolved failure and following functions.
- Object-property/computed-key sibling loss is pre-existing. Tests retain these
  limits without claiming a new regression, parameter fix or newline fix.

**Dependency:** Raw includes #531's object heuristic and five known literal
function-type return failures. Child removes only that heuristic, restores those
five cases, and retains the #534 authored test/report improvement. Its focused
suite retains one expected inherited object-test failure, separate from its
passing #534 test. Independent work can start; no #531/SEED-013 blocker is created.
The child still has the new internal-colon regression and typed-branch loss.

**Proof commands:**

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/freeze.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/run-proof.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/measure.py
```

The [durable README](../../../docs/reviews/fabio-malpezzi-20261027/pr534/README.md)
also gives the literal three supplemental commands, snapshot/corpus copy recipe,
setup and assertion locations. `reproduce.py` asserts all relevant import paths
and analyze_file/FileAnalyzer metric equality. Explicit expected rows check names,
spans and CCN; actual rows retain NLOC/token count. Target authored test fails;
raw/child authored test passes. Focused isolated suites: target 271 passed/5 skipped;
raw 274 passed/5 skipped; child 273 passed/5 skipped/1 inherited object failure.
The supplemental four-reader internal-colon control passes on target and fails
on raw/child. CLI observations retain all three decisive examples and typed bodies.

**Performance:** One immutable 92-case affected corpus and 100 original source
files; unchanged central paired harness, warmed 1.5-second batches, 20 alternating
pairs. All actual batches exceed one second. API medians 1.488780/1.519058 s,
SD .018110/.034054, +2.0337%, paired 95% [1.5167%, 3.2720%]. CLI medians
1.701121/1.722747 s, SD .152478/.112059, +1.2713%, interval [−4.7317%, 4.3104%].
Both strict below-3% gates are **unproved**. Invalid raw code was not repeatedly
timed to obtain acceptance. The queued correction owns a correct candidate and
sufficiently precise performance proof. The supplemental isolation case is
outside the timed corpus and explicitly unmeasured.

Current and initial default paths differ only in unused NS methods; all 92 API
actuals and full broader CLI bytes match. This supports one comparison covering
incremental/cumulative default cost. Prior accepted #527 proof remains separate;
ENS was not measured. One unchanged-output 5000-iteration profile per snapshot
finds extra object-helper calls/setup assignments but does not allocate the full
API cost or explain CLI variance. Raw data, hashes and bounded profile are retained.

**Evidence:**
[findings and consumer dispositions](../../../docs/reviews/fabio-malpezzi-20261027/pr534/review-findings.md),
complete patch/metadata, original and corrected harness diagnostics, explicit
inputs, paired JSON/terminal logs and isolated child patch. Original external
logs remain unchanged; checked-in terminal copies normalize trailing whitespace.

**Backlog:** Installed identity-evidence on current branch and canonical trunk
reports unlisted SEED-015, no history seed/record and no gaps (highest 014/011).
Installed add queued the canonical story after SEED-014, preserving existing
maintenance priority and unrelated order. No Take, plan or dependency agreement.

**Remaining ownership:** The follow-up owns correction, independent refactor,
installed consumer trace, discriminating final red/green proof, performance and
current-matrix CI. Coordinator owns source acceptance, obligations, PLAN status,
full suite/make, delivery and any later exact-head merge/thanks. The historical
four-job CI pass does not satisfy the current 14-job gate. Observer/stash remain
untouched. All owned commands are terminal; no owned watcher was created.

Independent source_review_5 read only the selected seed and returned its separate
commitments, qualifications and alternative journeys. Source comparison: the
original useful result is established, but explicit valid typed branches and the
existing curried grouping require the retained results, which raw/child fail.
The first missing-inner-function hypothesis was incorrect; the existing test
and `_push_arrow_function` refusal define the outer-only convention. Diagnostic
expectations were corrected without changing any product assertion or behavior.
G5 owns only the confirmed typed/internal-colon gaps and unproved performance.
The queued story supplies beneficiary, reproducers, expected results, remaining
work and PR link under the authorized >15-minute branch. It explicitly owns
corrected-code performance including the untimed supplemental controls, consumer
tracing and final merge/thank-you gates. No raw-candidate acceptance is claimed.
Independent refactor_534 validated all 192 hashes, 92 timed inputs, four untimed
controls, intervals, profiles, links and backlog order. It returned
## REFACTOR COMPLETE without edits or uncovered consumers. All 70 changed files
are below 250 lines. Current unchanged product's full pytest passed 1,687 tests,
7 skipped in 2.50s (/tmp/lizard-slice5-pytest.log); prior make proof applies.
