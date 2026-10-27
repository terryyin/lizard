# Anonymous arrow formals — slice 2 proof

Base: `33610866db00d28ba9e2dafe5a3fd1942347fb2f`, in the established
SEED-016 execution checkout. Candidate source hashes are retained in
`slice2-source-hashes.json`. This is semantic proof, not performance or final
delivery qualification.

## Ownership and observations

`JavaScriptBindings._read_parameter` now owns the former `_dec` extraction
rule. Named signatures give it their actual `FunctionInfo`; provisional
parenthesized candidates give it a provisional `FunctionInfo`, and arrow
confirmation transfers the same `full_parameters` representation. There is
no independent `_read_name` collector. Bare identifier arrows retain their
single formal in that same representation. Return annotations preserve the
pending signature, and ambiguous call/ternary parentheses retain candidates.
ADR-0001's parameter count / language reader terminology and ADR-0000's
human-owned direction are unchanged; no architectural exception is proposed.

New tests are in `test/test_languages/testJavaScriptArrowParameters.py`:

| Test / starting input | Observable result |
| --- | --- |
| `test_anonymous_and_named_bindings_agree`: alias pair | Both expose count2 and `renamed,tail`, across JS/TS/JSX/TSX/Vue/cjs/mjs |
| `test_anonymous_patterns_preserve_named_convention`: compound, nested, quoted, computed and sigil formals | Public names and counts equal the established named convention |
| `test_reported_arrow_forms`: four reported TS/TSX arrows | Four anonymous functions, counts2/1/2/2 and exact names |
| `test_bare_named_and_anonymous_arrows`: named/async/sigil arrows | Each has its actual one-identifier formal |
| `test_defaults_and_wrapper_call`: defaults and `api.wrap` | Defaults preserve names/count3/CCN5; wrapper callback has count2 |
| `test_arrow_parameter_metric_consumers`: real three-parameter arrow | CSV3, checkstyle `3 PARAM`, warning above2, highlighted HTML3 |
| `test_arrow_duplicate_lists_use_established_alias_names`: named/anonymous alias signatures | Duplicate counts2/2 and key `renamed,a,b,c,d` |
| `test_cli_reports_arrow_parameters_and_threshold`: real temporary JS file | Actual CLI PARAM3 rows, threshold2 warning and exit1 |
| `test_website_returns_actual_arrow_parameter_count`: HTTP JS/TS alias source | `/analyze` returns one anonymous function with `parameter_count`2 |

The tests supply only source or a real source file and threshold settings;
the analyzer produces every asserted parameter value. No parsing mocks or
precomputed output fixtures are used. Unrelated large test files are unchanged.

## Literal commands and terminal results

Red, before product edits:

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScriptArrowParameters.py -q
```

The initial18 tests all failed for the actual count/name loss. Named aliases
reported2 / `renamed,tail`; anonymous twins reported0 / no names. Defaults
already retained CCN5 while reporting no parameters. The later CLI/website
consumer tests were added after this initial red observation.

Green, final reader and consumer selection:

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testOptionalOperators.py test/test_analyzer.py test/test_options.py test/testApplication.py test/testOutput.py test/test_extensions/testDuplicatedParamList.py test/test_website.py test/test_website_smoke.py test/testCommentOptions.py -q
```

Terminal result:572 passed,5 skipped in2.28s. This includes the new public
assertions above, earlier identifier/receiver tests, output/threshold and
duplicate-list suites, actual website API and real-HTTP smoke boundaries.

The first baseline reproducer invocation used the checked-in input location:

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/reproduce.py "$PWD" docs/reviews/fabio-malpezzi-20261027/pr536/all-inputs.json > /tmp/seed016-slice2-baseline.jsonl
```

It observed all API rows, including reported counts0/0/0/0, named/anonymous
alias asymmetry and wrapper count0, then stopped at `assert reported.is_file()`:
the checked-in record does not include `proof/corpus/affected/arrow-reported.ts`.
`slice2-baseline-api.jsonl` preserves those observations; it is not a completed
CLI baseline run. The corrected setup uses the frozen cache with the same
input bytes and its actual corpus file:

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/reproduce.py "$PWD" /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/all-inputs.json > /tmp/seed016-slice2-candidate.jsonl
```

This completed65 API cases and the actual CLI checks, verifying facade,
reader and core import paths before and after. Terminal exit1 is the expected
partial-story result:60 cases pass, including all34 accepted PR532/535
controls; exactly the five rows below fail. `slice2-candidate.jsonl` retains
every case's names, counts, CCN and other observed metrics and CLI output.

## Remaining story obligations

- Empty bindings: anonymous and named `({}, [], tail)` report count1 with
  names `tail`, expected count3. Source: “empty binding patterns still
  occupy formal slots without inventing public names”. Receiving slice3 owns
  the shared slot representation and proof.
- Typed function return: `arrow-return-function.ts` returns only the inner
  `n` arrow; expected also the outer anonymous arrow with `x,y` and count2.
  Source: “Generic parameters, typed returns, rest/defaults, nested default
  CCN, ternary-default CCN ownership and following functions stay correct.”
  Receiving slice3 owns annotation continuation and its explicit regression.
- Empty-slot output consistency: CSV and checkstyle currently derive counts
  from `len(parameters)`; HTML uses `len(full_parameters)`. Source promises
  “across CLI, formatters, warnings, duplicate-parameter lists and the current
  website JSON/`parameter_count` consumer path.” Slice2 proves nonempty
  signatures; receiving slice3 must align count consumers with formal slots
  while duplicate lists keep only established names. The typed callback,
  generic, nested-default and ternary-default frozen rows now pass; this does
  not replace slice3's explicit regressions and full acceptance.
- `arrow-member.js` still reports member-assignment count0 as explicitly
  deferred to #537; its promised wrapper-call half now reports2.
- `arrow-abandoned.js` outer CCN remains2 rather than3 as deferred to #539.
- Performance, full pytest/make, final hosted CI, actual contributor push,
  guarded merge and acknowledgement remain slices4–5. No claim is made here.

## Consumer trace

```sh
PIP_QUIET=1 nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout "$PWD" --base 33610866db00d28ba9e2dafe5a3fd1942347fb2f --term parameter_count --term full_parameters --term parameters --term BindingSummary --term javascript_bindings --term javascript_functions --term typescript_states > /tmp/seed016-slice2-consumers-full.md
```

[slice2-consumers.md](slice2-consumers.md) and its linked
[slice2-consumers-history.md](slice2-consumers-history.md) retain the complete
generated trace with dispositions for every located file. Runtime consumers are connected to the assertions
above. Historical evidence/planning text and other-language readers are
excluded with reasons; `FunctionInfo` itself has no product edit in this slice.
The website path is proved through the actual HTTP response; inspected
`Playground.jsx` renders its numeric `parameter_count` field directly through
`METRICS` and `fn[field]`. No browser-rendering observation is claimed.

Focused proof was inexpensive on the local host (seconds), so no heavy
workload or performance experiment was launched. No tests or subprocesses
remain pending. Nix's existing platformdirs/virtualenv resolver warning did
not prevent setup or the passing test command.
