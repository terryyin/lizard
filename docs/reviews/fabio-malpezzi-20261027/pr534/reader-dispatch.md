# Deliver TypeScript reader tokens through the configured state

Base: `ec2ec81565e037883066f220ab794f912b8df2b8`. Actual verified product is
uncommitted and not performance-qualified. Only typescript.py product bytes
differ from that base; reader-dispatch-proof.json retains every product hash
and the exact runtime patch. No grammar, annotation receipt or counter changed.

## Existing protocol and bounded implementation

The built-in JS/TS/JSX/TSX/Vue readers have a sole TypeScriptStates instance and
use CodeReader's no-op token hook. Following GoReader's existing local delivery
pattern, bind that state's class call once, deliver each original token once,
then finalize the state and call eof. Counters/extensions remain in the original
pipeline and their order relative to state processing/yield is unchanged.

Direct delivery requires an exact built-in list/tuple, one exact TypeScriptStates
object, and both unchanged class and resolved bound hook identities. Other
containers/state types and subclass/inherited/property/instance hooks delegate
to unchanged CodeReader. This avoids early len/index/call assumptions for
ordinary alternative configurations. It adds no generic hook framework or
speculative midstream mutation support. Context assignment, finalization, eof
and errors retain the existing protocol. ADR-0000/0001 and local reader ownership
remain applicable; no other production subsystem changes.

## Real protocol consumers and retained fixture correction

testTypeScriptReaderDispatch.py drives the real TypeScriptStates grammar and
normal token processors with FileInfoBuilder. Reader subclasses/instance hooks
and a decision-word extension add a condition using the actual context API;
they do not replace parsing or supply analyzed results. Assertions observe
public function CCN/parameters/spans and original emitted tokens, then confirm
state finalization completed before eof. Inherited/property and bound/unbound
instance hook cases exercise the ordinary extension forms separately. The
additional state observes the finalized real function; alternate iterable and
no-state configurations keep their existing delivery contract. Hook failure
propagates without an eof callback.

The first nine-case run ended 1: eight passed, and the alternate container
fixture failed at unchanged preprocess's parallel_states[0]. It supplied only
iteration, although preprocessing already requires indexing. The fixture now
provides that existing index contract while still omitting len; no product
logic or expected analysis was weakened. Its failed source/hash, numeric receipt
and exact cause remain in initial-dispatch-test-receipt.json and the execution
conversation (tool chunk 3a29b5). The corrected nine-case run terminated 0.

A real two-file JS/TS analyze_files(..., threads=2) observation additionally
checks exact function results and that no workers remain. Actual temporary
source files provide the filesystem setup. No parsing mocks are used.

## Commands and assertions

Before proof, Territory.local had 16 CPUs/48 GiB, load 5.64/6.04/6.52. The
retained host-before.json includes VM/process data. No competing owned heavy
proof was active; other native VM/worktree activity is unknown. Focused tests,
replays and a two-worker/two-small-file protocol check are modest here.
All Nix commands remain serial; reconsider material contention or failure.

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testArrowObjectBoundaries.py test/test_analyzer.py test/testOutput*.py test/testApplication.py test/test_options.py test/testBasicFunctionInfo.py test/test_extensions test/test_languages/testOptionalOperators.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-reader-dispatch/focused.log 2>&1
PIP_QUIET=1 nix develop -c python -m pytest test/test_website.py test/test_website_smoke.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-reader-dispatch/website.log 2>&1
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testTypeScriptReaderDispatch.py::test_public_parallel_analysis_preserves_function_results test/test_parallel_analysis.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-reader-dispatch/parallel.log 2>&1
PIP_QUIET=1 nix develop -c python -m pytest 'test/test_languages/testTypeScriptReaderDispatch.py::test_reader_decision_hook_changes_public_ccn[PropertyDecisionReader]' > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-reader-dispatch/property-hook.log 2>&1
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reader-dispatch-verify.py
```

Every command terminated 0. Focused result 987 passed/five existing skips;
website 41 passed; parallel 16 passed; later property-hook case one passed.
The two later cases add protocol observations after the focused run; they do
not change runtime bytes or invalidate accepted earlier selections. All eleven
protocol cases have actual observations across these commands. No unchanged
full focused command or profile was repeated solely for test-only additions.

Source strings, real readers, context and normal token processors provide the
preconditions. Public tuples in the existing ternary/formal-slot modules retain
all original expectations; the new protocol module observes extension effects,
token delivery and lifecycle. The focused selection includes all extension
suites, output/CLI/core and JavaScript nested-structure consumers. Website
checks exercise real HTTP results. The broader worker suite retains cancellation,
failure and cleanup observations alongside the new real JS/TS result check.

reader-dispatch-verify.py invokes the unchanged qualification verifier in
isolated real processes for base and working product, requiring all 166 expected
rows and independent FileAnalyzer agreement. Actual facade/registry/reader/state/
core/helper imports are asserted. All seven public fields are identical, not
only names/CCN. Immutable base source hashes hold and only typescript.py differs.

Both versions run the unchanged 100-file CLI with the SAME setup2 absolute
corpus arguments. Warning exit 1 and complete output bytes match exactly,
SHA256 `3adeb4d133e695385615a788f09fa35b31e28ae888e6badfae10f129eb545e8a`.
No corpus, old profile, expected row or statistical result was changed.

## One after-change diagnostic

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reader-dispatch-profile.py
```

The driver and both workers terminated 0. A fresh load/VM observation and
bounded decision are retained in profile-decision.json. One serial working
profile per original128/augmented166 corpus uses five warm passes and 100
analysis-only passes; metric extraction is outside the profile loop. Seven
fields match the exact ec2 profile before/after, with unchanged working hashes.
The immutable ec2 profiles are reused, not rerun or overwritten.

| Diagnostic | Original128 | Augmented166 |
| --- | ---: | ---: |
| Old ec2 total seconds | 5.0998 | 6.8203 |
| Working total seconds | 4.9569 | 6.6335 |
| Old generic reader own seconds | 0.2199 | 0.2920 |
| Working reader own seconds | 0.1365 | 0.1793 |
| Old no-op reader process_token calls | 561,900 | 735,800 |
| Working no-op reader process_token calls | 0 | 0 |

Tokenizers' process_token methods remain; only the unused READER hook disappears
on the recognized built-in path. The state machine still consumes every token.
Single profiles under changing host conditions support reduced dispatch work,
not a causal allocation of the whole time change or a below-3% throughput bound.
All six exact committed-revision gates remain with planned qualification 8.

## Consumer record and remaining duties

The installed trace uses base ec2 with __call__, process_token, parallel_states,
statemachine_before_return, eof and TypeScriptReader. Its complete raw/final
records, row count/hash and proof correspondence are retained in the cache and
reader-dispatch-proof.json. Shared JS/TS/JSX/TSX/Vue/CJS/MJS, core/FileAnalyzer,
output/extensions/CLI/HTTP and real parallel consumers have the observations
above. Other languages still use unchanged generic dispatch; historical/lifecycle
or lexical matches do not inherit TypeScriptReader. Token/metric contracts for
unchanged extensions remain observed by current extension/shared-reader tests
and equal public values. No reached consumer is left without a disposition.

Cache: `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-reader-dispatch`.
Independent refactoring/delivery, all six timing gates, final full pytest/make,
current hosted CI and contributor duties remain with their planned owners.
No throughput qualification, commit, push or status change is claimed here.

## Source comparison and semantic acceptance

Fresh isolated `/root/continuation_coordinator/source_reassessment` derived
only the original seed/contract plus explicit selected-correction amendment;
its actual launch and full inventory remain in the execution conversation.
Compared individual journeys with actual diff, real reader/counter setup and
observing assertions: all 166 rows retain seven fields and CLI bytes; original
typed/internal/curried/property/body/following-function promises are unchanged.
Inspected hook/state-container guards, native call binding, finalization/eof,
all eleven real protocol cases, public parallel results and verifier imports.
No parser mock, weaker assertion, grammar or source-promise change was found.
The 464 consumer dispositions are populated; G10's fixture loss is resolved.
Timing remains with 8 and full/hosted/contributor delivery with 9. Independent
structure7_refactor returned `## REFACTOR COMPLETE`: no edits or tests, 39
incoming files and all hashes/dispositions checked. Coordinator then ran
`PIP_QUIET=1 nix develop -c make pep8`, terminal 0, without repairs. Existing
semantic proof remains unchanged; profiles grant no timing pass.
