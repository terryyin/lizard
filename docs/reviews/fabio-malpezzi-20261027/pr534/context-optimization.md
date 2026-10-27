# Annotation context attribution and simplification

Base: `8dac7b80c2478d65b54d8ec96d6aa36bf4530b91`; current comparison:
`9979f106a955aef2460ed6fe7ebb5782b755bcad`. The earlier two statistical
experiments remain unchanged. This diagnostic grants no throughput verdict.

## Decision and observation

Territory.local had 16 CPUs, 48 GiB RAM, load 2.818/3.280/3.444, 22,604 free,
53,529 speculative and 1,046,320 inactive 16-KiB pages. Historical swap totals
do not establish current pressure. No benchmark, pytest or profiler was found;
the coordinator confirmed no other owned heavy work. One serial profile of
100 × 128 retained inputs per current/8dac snapshot is a modest attribution
experiment. Keep its output unchanged within each snapshot, confirm imports,
retain all profiles, and reconsider before another experiment if contention
or unexplained variance changes this basis. The slice agent owns terminal
commands; final exact-revision four-gate qualification belongs to slice 6.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/context-profile.py
```

Terminal exit: 0. Raw JSON and cProfile statistics are retained in
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-context`.
Imports resolve to the exact immutable archives; each snapshot's seven public
fields are identical before, during and after profiling. Current and corrected
metrics differ as expected; profiling's tuple extraction is additional work
that is absent from the immutable paired API worker.

| Signal, 100 corpus passes | Current | 8dac |
| --- | ---: | ---: |
| Total profiled seconds | 4.9099 | 5.2498 |
| Reader preprocess own seconds | 0.1740 | 0.2143 |
| Runtime global-state own seconds | 0.2989 | 0.2976 |
| Annotation-context cumulative seconds | absent | 0.0306 |
| Return-annotation probe cumulative seconds | absent | 0.1398 |
| Included branch-separator scan cumulative seconds | absent | 0.0573 |

One observational `sys.setprofile` pass records 315 context queries, 80 after
a canonical root `)` and 63 returned contexts. Every returned context has that
same canonical root token; zero counterexamples. Root CodeStateMachine writes
`last_token` after child dispatch; comments/newlines are removed before state
processing. The shortcut can avoid 235 traversals, but its small attributed
cost cannot alone close the material performance gap. No statistical retry
or runtime edit followed this finding.

Most first typed branch groups are existing bare nested states, without a
binding tracker; simply adding a binding-root type flag would identify only
three contexts, plus six empty groups. Do not introduce a parallel tracker
to compensate. The implementation decision below uses canonical type consumption;
these diagnostics establish no throughput acceptance.

## Existing-owner implementation

The runtime global state's existing non-object colon→type-consumption boundary
records type evidence on that group. Runtime ternary separators return before
this boundary; nested object, function-return and type states own their own
colons. When the group already has a binding tracker, root/non-initializer
constraints remain. No bare-group creation or binding behavior was changed.
The completed group is an unambiguous signature when empty or already typed;
its following colon delegates directly to the existing type-annotation owner.
Untyped, nonempty groups retain the original type/body confirmation.

The reader queries context only after its canonical root `)` and creates a
tee pair only at an actually ambiguous colon. It discards the probe iterator,
yields that colon once and restarts the native `for` with the preserved main
iterator. There is no recursive probe stack, per-token wrapper, counter replay
or raw-source filter. Newline handling precedes the decision safely because
`:` cannot start a statement. Ordinary inputs and unambiguous groups incur no
whole-stream tee setup. The removed empty-parameter probe argument is internal;
the same group now delegates directly rather than parsing the annotation twice.

One diagnostic after the group/query change (`working`) and one after demand
buffering (`working-demand`) are retained separately; neither overwrites the
earlier profiles. The same modest serial decision remained applicable, with
no other owned heavy work. Context queries fell 315→80, annotation probes
63→49 and separator scans 44→39 per corpus pass. Context cumulative seconds
fell 0.0306→0.0143 and probe cumulative seconds 0.1398→0.1132. Preprocessing
own seconds were 0.2143/0.2090/0.1937 for 8dac/group-only/final; corresponding
totals were 5.2498/5.3091/5.1669. These are single profiles with different
legitimate function counts versus current, not paired throughput observations.
They support less work in the implicated path, not a below-3% claim.

## Public proof

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testArrowObjectBoundaries.py test/test_analyzer.py test/testOutput.py test/testOutputCSV.py test/testApplication.py test/test_options.py test/test_extensions/testCognitiveComplexity.py test/test_languages/testOptionalOperators.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/qualification-verify.py /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-optimized-8dac7b80/inputs.json --require-expected > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-context/final-metrics.jsonl
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/context-profile.py working-demand
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/context-verify.py
```

Every command terminated with numeric exit 0. Final focused result: 753 passed,
five unchanged skips. The retained input verifier observes 128/128 expected
public rows, independent FileAnalyzer agreement and actual imports from this
checkout. `context-verify.py` asserts all seven fields match exact 8dac and
both post-change profiles, checks immutable archive source hashes and runs
the unchanged 100-file broad CLI through both versions. CLI exits are the
expected warning exit 1, with byte-identical output SHA256
`49e9a7922e48f71ac1702f2da38939d93c24b4339d0d3618dfdfdebe1287150b`.
`context-proof.json` retains the assertions, raw evidence hashes, selected
profile rows and exact uncommitted patch hash; it explicitly says not qualified.

Setup supplies only source strings, filenames and retained input rows.
Assertions in `testJavaScriptTernaryArrows.py` observe public names/spans/CCN;
the verifier adds NLOC, tokens and parameters. Existing expectations remain.
The expanded runtime-group test supplies assignments containing a ternary,
object, call or nested arrow; each keeps the outer separator, alternate and
following functions. The trivia test supplies typed/empty first parameters
and untyped alternate parameters separated from return annotations by comments
and newlines, and observes both arrows, enclosing and following functions.
No parser logic is mocked. The test module is 237 lines; unrelated large test
files remain byte-identical. TypeScriptStates remains 250 lines.

## Consumer dispositions and remaining owners

```sh
PIP_QUIET=1 nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ --base 8dac7b80c2478d65b54d8ec96d6aa36bf4530b91 --term return_annotation_context --term return_type_colon --term _typed_parameters --term _empty_parenthesis --term preprocess --term TypeScriptReader --term TypeScriptStates --term JavaScriptFunctionsMixin --term tee > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-context/consumers-final.md
```

The retained trace has 285 populated dispositions and zero blanks, including
the supplied terms and every located consumer. JS/TS/JSX/TSX/CJS/MJS/Vue,
registry, core preprocessing, public facade/FileAnalyzer, CLI/CSV and cognitive
consumers are run through the focused command/replays and CLI comparison.
Existing widget/fixture observations retain their five skips explicitly.
Independent other-language and C-preprocessor implementations do not consume
this annotation context; output formats receive unchanged seven-field values
and have no changed formatting contract. Frozen review records are immutable
history, while unchanged guidance/source descriptions reuse the same contracts.
Broad `tee` substrings in guarantees, color names and unrelated scripts have
no reader dependency. No semantic consumer lacks a disposition. Four strict
timing gates remain with 6; independent refactoring/delivery and final full
pytest/make/hosted CI/contributor actions remain with their planned owners.
All owned commands are terminal; no extra verification watch was created.

## Independent refactor

The independent pass named the context tuple's ternary depth and unambiguous
signature values, clarified that helper's return contract, and explained the
intentional native-iterator restart. No recognition rule, state lifecycle,
input, expected value or production subsystem changed.

The profiles and `context-proof.json` remain frozen at pre-refactor runtime
patch `90658c62329f1e7b8720c2c051db7d1218b8da57cebe26b187d29ee20b3ecf21`.
The equivalent refactored runtime patch is
`a19519923b33a5eb0ba44667e9214df95f4be17404c4c79337c230114bdc171c`.
These hashes distinguish the variants without claiming a new timing result.

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScriptTernaryArrows.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/qualification-verify.py /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-optimized-8dac7b80/inputs.json --require-expected > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-context/refactor-metrics.jsonl
```

Both commands terminated 0: 162 public regression cases pass; all 128 expected
rows, actual imports and FileAnalyzer agreement pass, and all seven fields
equal the prior final replay. Source fixtures and explicit assertions are
unchanged. Other accepted shared-consumer and byte-identical CLI proof is
reused by inspected equivalence. The installed trace, with the prior supplied
terms plus `unambiguous_signature`, is `seed015-context/refactor-consumers.md`:
300 populated dispositions, zero blanks. Additional matches are unchanged
guidance/history contracts. `seed015-context/refactor-receipt.json` retains
the hash, replay comparison and terminal receipts. No profiler, statistical
measurement or full suite was rerun.
