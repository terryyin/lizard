# PR #525 review and relevance to issue #524

Review completed on 2026-11-01. **Still relevant; requires integration and
performance qualification before merge.** The PR remains open and unmerged;
no GitHub comments or reviews were posted, and no product code was changed.

The [linked issue comment](https://github.com/terryyin/lizard/issues/524#issuecomment-6525221776)
specifically requests attention to [PR #525](https://github.com/terryyin/lizard/pull/525).
The October 27–28 FabioMalpezzi cohort covered PRs #527, #528 and #531–#547;
#525, by leonrosenshein, was outside that cohort.

## Relevant completed work

| PR | Relationship |
| --- | --- |
| [#531](https://github.com/terryyin/lizard/pull/531) | Direct overlap: expression-bodied arrows must read object operands as objects. Merged with a corrected implementation, `ac2b454a`. |
| [#533](https://github.com/terryyin/lizard/pull/533) | Related arrow-end and line-ownership work. Merged with a corrected implementation, `a2d2ab77`. |
| [#534](https://github.com/terryyin/lizard/pull/534) | Related ternary versus type-annotation ownership; current signature handling must survive integration. Merged `48de7f29`. |
| [#536](https://github.com/terryyin/lizard/pull/536) | Related binding/formal-parameter ownership in the same reader concept. Merged `1bc7cbd6`. |
| [#545](https://github.com/terryyin/lizard/pull/545) | Shared template-substitution consumer; #525 also changes `${` bracket tracking. Merged `d0ecc33b`. |

Issue [#523](https://github.com/terryyin/lizard/issues/523) was another exhausted
binding-tracker crash, caused by a ternary colon being read as a type annotation.
Its repair `a062c4d` is related but does not settle #524's subscript-boundary
problem. The other reviewed JS/TS PRs share parser areas but are not substitutes
for #525's remaining behavior.

## Revision-bound correctness

The frozen reported target / PR parent is
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`; PR head is
`63798156566ea4ab100048dc1eb21a7366cde380`. Current target at review is
`6863a63513325ba40e440bc19cf608b95f35bbf5`, whose product files are unchanged
from `4e0b2c22c5845639c8686730da1c90868132e5c8`; its additional commit admits
this review. The PR head was rechecked after analysis and had not moved.

On current master, all of issue #524's original `h(() => f(), {id: 1})`,
nested-array and Map crash examples now complete without an exception and
retain the expected callback spans. Thus last week's work fixed a substantial
part of the report, without incorporating #525.

The PR still supplies useful behavior absent from current master:

```javascript
it('t', () => {
  e(r[0]);
  g(x);
});
```

Current master reports the callback at lines **1–2**; #525 reports the correct
**1–4**. Adding `if (x)` on line 3 also demonstrates lost callback CCN on master.
The chained-subscript example ends at line 2 instead of line 3. In
`function f() {\n return { w: [m[n - 1]] };\n}\nfunction g() {}`, master ends
`f` on line 2 instead of line 3; #525 preserves both functions correctly.

The complete eleven-case authored matrix runs through `analyze_file` as
JS, TS, JSX, TSX, MJS, CJS and Vue script input: the proposal matches **77/77**
expectations, current master **49/77**. Of the 28 exact mismatches, 21 concern
the three boundary examples above; seven concern the ordering of the two
functions in the object-arrow example. The latter are reported as a changed
completion order, not as another missing-function defect. For just the four
suffixes named in the PR, the corresponding results are **44/44** and **28/44**.

There are 128 total discriminating observations per source snapshot, including
following-function/CCN, adjacent-member, type-signature, labeled-statement and
template controls. The `ns` extension reproduces the same base metric results.
The original target has 28 crashes, while proposal and current master have
none in this bounded corpus. The original target aborts CSV, `-o` and `-Ens`
batch CLI runs; both proposal and current master complete all three modes,
including a following Python file. Current CSV output retains the incorrect
subscript callback span seen through the API.

This does not establish absence of crashes on arbitrary inputs or replay the
contributor's private 28k-file corpus. The label and deeper nested-object-array
limitations remain on both proposal and master. The old proposal also lacks
newer type-signature improvements already present on master. Those inherited
limitations are not treated as newly introduced PR defects.

## Integration, quality and checks

The complete four-file diff, PR discussion, submitted reviews, inline-thread
states and hosted run were read. Discussion, reviews and threads were empty.
The patch's defensive exhausted-frame guard is sensible; avoiding duplicate
tracker input and consulting bracket ownership address the actual subscript
failure. The fix belongs with the existing binding tracker and shared state
dispatch. It must preserve the current parameter, signature, ternary and
template consumers, rather than replacing current files with their older
versions. ADR-0001's established metric meanings apply; no architectural
exception is proposed.

`git merge-tree --write-tree` reports content conflicts in both
`lizard_languages/javascript_bindings.py` and
`lizard_languages/typescript_states.py`. The original green branch therefore
does not establish a correct current-master candidate. No contributor branch
was modified and no write-access claim is made.

Original branch full pytest: **1,667 passed, 7 skipped**. Current master full
pytest: **2,210 passed, 7 skipped**; current `nix develop -c make` also exits 0,
including the full suite and configured style checks. Existing pylint and
dependency warnings are retained in the logs, not represented as zero messages.
Original [hosted PR CI](https://github.com/terryyin/lizard/actions/runs/40242018917)
succeeded on four historical jobs. Current CI requires fourteen
Ubuntu/Windows × Python 3.8–3.14 builds and the changes job; web checks are
conditional. No integrated proposal or current-matrix acceptance is claimed.

## Measured cost

The installed paired benchmark runner measured the original PR against its
exact parent using Nix Python 3.9.11, once per workload, with twenty alternating
warmed pairs calibrated to at least 1.2 seconds. Inputs were frozen before
timing: 24 affected JS/TS-family API files that both revisions can analyze and
100 original Python source files for the broad CLI. Runtime source and corpus
hashes, every pair, load observations and output stability are retained.
Source snapshots are Git archives, so runner revision fields are null; the
workload manifest records their exact Git revisions and the runner records
their production hashes.

| Workload | Paired change | 95% interval |
| --- | ---: | ---: |
| Affected API including parameter consumers | +3.60% | +2.75% to +4.44% |
| Broad CLI | +0.22% | -1.06% to +1.51% |

The affected result does not establish compliance with the earlier Fabio
cohort's strict sub-3% upper-bound policy. That historical policy is a useful
reference, not a newly invented universal limit for #525. The broad pass does
not cancel the affected cost. These measurements qualify neither a rebased
candidate nor its performance against current master. No favorable retry or
causal profiling claim is substituted for the recorded comparison.

## Disposition

**Review completed within the requested scope.** Keep #525 open and unmerged
pending a current-master correction. Reconcile its remaining binding lifecycle
and replay changes with the current owners, preserve the authored public tests
and newer signature/template controls, profile the affected overhead, and
qualify the resulting candidate against current master and applicable hosted
CI before a merge decision. The issue should not be closed merely because its
first crash reproducer now passes.

The review owner is SEED-022#review-pr525-issue524. Product correction and public
GitHub actions have not been performed as part of this review.

## Replay evidence

At [before-cleanup 5f7d58fb](https://github.com/terryyin/lizard/tree/5f7d58fbad166bf05e1510162831bbc03cbabd96/docs/reviews/pr525-issue524-20261101),
this directory contains `github-packet.json`,
`final-state.json`, `reproduce.py`, six default/NS result files, `cli.json`,
`workload-manifest.json`, both performance reports and full pytest/make logs.
Run `reproduce.py --checkout <snapshot> --out <new-result.json>` in the prepared
Nix checkout, optionally adding `--extension ns`. Snapshot roots are the exact
`git archive` exports of the reported parent and proposal above; current uses
the owned execution checkout. Recover any raw file with
`git show 5f7d58fbad166bf05e1510162831bbc03cbabd96:docs/reviews/pr525-issue524-20261101/<file>`.
The current snapshot retains this disposition; the full review inputs and
diagnostics remain recoverable at that revision.
