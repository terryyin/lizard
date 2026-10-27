# Dispatch the canonical root without unused binding work

Base: `13c633b8ed1ab5af4a30decfd87ceaeaf7044093`. The actual verified product
is uncommitted and not performance-qualified. Only typescript.py product bytes
differ; root-dispatch-proof.json retains all 104 source hashes and exact patch.

## Existing owner and minimal change

TypeScriptReader creates an exact TypeScriptStates root without a binding
candidate. Existing binding allocation creates distinct children; their
TypeScriptStates wrappers own binding/default/formal updates. The root wrapper
only performs an unused candidate check before canonical CodeStateMachine
dispatch. The prior read-only whole-owner PFE is retained in root-dispatch-pfe.md.

The recognized sole-state/default-hook path now additionally requires an actual
root binding candidate of None, then binds CodeStateMachine.__call__ once.
Missing or configured binding owners retain generic dispatch, as do existing
custom hook, state and container configurations. No base state-machine code,
child wrapper, BindingSummary, type grammar, metadata receipt or metric changed.

The canonical dispatcher still calls the active state. A binding child invokes
its own unchanged TypeScriptStates wrapper; saved-state restoration, callbacks,
last_token and exit propagation remain in their existing owner. Parser-only
next/replay calls remain normal self(token). Root finalization and eof keep
their order. This does not support speculative private midstream mutation.
Accepted ADR-0000/0001 and local reader responsibility remain applicable.

## Observable proof

The existing reader protocol module still drives actual TypeScriptStates,
FileInfoBuilder and normal extensions. The added configured-root case installs
a real JavaScriptBindings owner and observes defaults/previous-token tracking,
public function CCN/parameter count and the finalized result. It would lose the
binding observations if the configured root incorrectly bypassed its wrapper.
No parser or analyzed result is mocked or supplied by the fixture.

Existing tests cover inherited/property/bound/unbound reader hooks, extra states,
indexable no-len containers, no-state delivery, hook failures, original tokens,
finalization/eof and real parallel JS/TS results. Shared tests additionally
exercise nested/default/formal/catch/object/type callbacks and following
functions through public analysis. Original frozen/curried assertions remain.

Territory.local had 16 CPUs/48 GiB, load 7.35/6.94/6.71, about 15.5 GiB
inactive memory. Host/VM/process and bounded decision records are retained.
No other owned heavy proof was active; native VM/worktree activity is unknown.
Serial focused/replay/HTTP checks and temporary two-worker small-file tests are
modest here; reconsider changed contention or failure before repeats. The one
after-change diagnostic separately observes headroom before its serial workers.

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testTypeScriptReaderDispatch.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-dispatch/protocol.log 2>&1
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testArrowObjectBoundaries.py test/test_analyzer.py test/testOutput*.py test/testApplication.py test/test_options.py test/testBasicFunctionInfo.py test/test_extensions test/test_languages/testOptionalOperators.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-dispatch/focused.log 2>&1
PIP_QUIET=1 nix develop -c python -m pytest test/test_website.py test/test_website_smoke.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-dispatch/website.log 2>&1
PIP_QUIET=1 nix develop -c python -m pytest test/test_parallel_analysis.py > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-dispatch/parallel.log 2>&1
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/root-dispatch-verify.py
```

Every command terminated 0. Protocol 12 passed; expanded focused 990 passed,
five existing skips; HTTP 41 passed; worker lifecycle 15 passed. The complete
focused selection includes the new parallel JS/TS result test. Literal test
setups/assertions are in testTypeScriptReaderDispatch.py, the existing ternary,
formal-slot, function-type, output/extension and HTTP/worker modules. Source
strings, actual context/reader extensions and real temporary files provide
only preconditions. No expected result or unrelated large test was changed.

root-dispatch-verify.py invokes the unchanged public verifier in isolated
processes for exact 13c and working product. All 166 expected rows pass with
independent FileAnalyzer agreement and actual facade/registry/reader/state/core/
helper imports checked. All seven public fields match, not merely names/CCN.
The exact archive hashes hold; only the intended Reader product file differs.

The unchanged 100-file CLI runs against the SAME setup8 absolute corpus paths.
Both versions return warning exit 1, no stderr, and byte-identical full output
SHA256 `88f28b6a85933dd2e1674ca75210fcb891f6511715bcb6a70f3ebbeb39e3e494`.
No input, expected row, original manifest or old statistical result was changed.

## One labelled after-change diagnostic

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/root-dispatch-profile.py
```

Driver and both workers terminated 0. Each uses five warm passes and 100
analysis-only corpus passes; metric extraction stays outside the profile loop.
The previous working profile hashes correspond exactly to committed 13c's
104 product hashes. The diagnostic reuses those profiles without rerunning or
overwriting them. Its source-path corpus is the same as that prior diagnostic;
every source byte/name is asserted equal to the newer qualification corpus.
This is correspondence checking, not normalized replay data. All fields and
working source hashes match before/after; imports resolve to this checkout.

| Diagnostic | Original128 | Augmented166 |
| --- | ---: | ---: |
| Prior 13c total seconds | 4.9569 | 6.6335 |
| Working total seconds | 4.9234 | 6.5093 |
| Prior TypeScriptStates wrapper total calls | 1,109,400 | 1,492,000 |
| Working wrapper total calls | 547,500 | 756,200 |
| Removed root entries | 561,900 | 735,800 |
| Prior wrapper own seconds | 0.1996 | 0.2767 |
| Working wrapper own seconds | 0.1110 | 0.1591 |

Nested binding wrappers remain active; only root entries disappear. Single
profiles under changing host conditions support less dispatch work, not a
causal allocation of the whole time difference or a strict throughput bound.
All six exact committed-revision performance gates remain with slice 9.

## Consumer record and handoff

The installed trace uses base 13c and CodeStateMachine, __call__,
_binding_candidate, TypeScriptReader, _replay_token, callback,
statemachine_before_return and eof. Complete raw/final traces and their hash
are retained in the cache/proof. Actual shared JS/TS/JSX/TSX/Vue/CJS/MJS,
FileAnalyzer/core/output/extensions/CLI/HTTP and worker consumers have the
observations above. Other language implementations still use their unchanged
dispatch/owners. Historical, lifecycle and lexical matches do not become new
TypeScriptReader callers. Every located consumer has a concrete disposition.

```sh
PIP_QUIET=1 nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout /Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ --base 13c633b8ed1ab5af4a30decfd87ceaeaf7044093 --term CodeStateMachine --term __call__ --term _binding_candidate --term TypeScriptReader --term _replay_token --term callback --term statemachine_before_return --term eof > /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-dispatch/consumers-raw-final.md
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/root-dispatch-integrity.py
```

Both commands terminated 0; 492 located rows have populated dispositions and
zero blanks. Seven owned files pass AST/JSON, whitespace, EOF and 250-line
checks. The tracked-test diff contains only this cohesive protocol module.

Cache: `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-dispatch`.
All owned commands are terminal, with no extra watches. Independent refactoring,
publication, six timing gates and final full pytest/make/hosted/contributor
duties remain with their planned owners. No commit, push, plan status change,
formatter, hook lint or throughput acceptance is claimed here.

## Source comparison and semantic acceptance

Fresh isolated `/root/continuation_coordinator/source_inventory2` received only
the original seed/contract and selected-correction amendment. Its actual launch
and complete individual inventory remain in the execution conversation.
Compared the source journeys with actual code, setup and observing assertions:
all166 public rows retain every field and CLI bytes; original branch/internal/
typed/curried/property/genuine-body/following-function promises remain observed.
Inspected rootNone guard, canonical method binding, child/callback/replay code,
the real configured-root binding test and all existing protocol/worker cases.
The 492 consumer dispositions are populated. No source promise, grammar,
metric or accepted parameter behavior changed. Independent structure8_refactor
returned `## REFACTOR COMPLETE`: no edits/tests, 35 files, 104 source hashes,
proof-log/metric/CLI digests and 492 consumer dispositions checked. Coordinator
`PIP_QUIET=1 nix develop -c make pep8` then terminated0 without repairs.
Six exact-revision bounds remain with9, final full/hosted/contributor duties10.
