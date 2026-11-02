# Independent aggregate refactor return

Fresh independent review of SEED-024#resolve-issue524-label-types, slice 1,
after the final 200-case implementation return. The implementer and earlier
refactor evidence were read as supplied proof and were not edited. Root owns
the plan, performance, full checks, reconciliation, hosted CI and delivery.

The aggregate is coherent after two import-only edits. Runtime analysis and
return-annotation lookahead now import `TypeScriptTypeAnnotationStates`
directly from its extracted owner, `typescript_type_annotation`, instead of
obtaining it through the declaration mixin's module. All historic aliases,
including the declaration and reader module aliases, remain available and
resolve to the same class. No parser behavior or test assertion changed.

## Exact change and scope

| Path | Refactor edit | Current SHA256 |
| --- | --- | --- |
| `lizard_languages/typescript.py:11` | Direct annotation-owner import | `8a2b408b471810da54b2d999ce652866e6b7ec3489a61f8bed73cfdc9857d7f7` |
| `lizard_languages/typescript_signatures.py:5` | Direct annotation-owner import | `daec9731184e1b4e9be3ab7d741d70586dca7b94f1f9c7a53bae595c78d93702` |

The accepted earlier removal of the unreachable signature fallback remains
intact. The new signature hash differs from accepted
`a36101fb19d18ab86c0877972a387d1d829372e0cc156b3440b18fece67908a9`
only because its import path changed. No file was renamed, extracted, split or
deleted by this pass. At this agent's Nix release, the other eight supplied
runtime/test paths retained their accepted hashes; `manifest.json` records all
ten hashes and line counts for that focused-proof boundary.
`before.json` records the ten matching accepted hashes before these edits.

After that release, root reported a make/pycodestyle E128 at
`typescript_declarations.py:24` and mechanically aligned the continuation by
adding one space, with no AST change. This is root's subsequent formatting
edit. Root owns its final aggregate hash and the post-format make proof; this
agent's earlier manifest and focused receipts remain an immutable record of
the pre-format import bytes. The source, setup and assertion correspondence
above is unchanged by that whitespace adjustment.

Both edits stay within the existing JS/TS language-reader subsystem. The
inspection included untouched binding, function and state-machine owners,
reader registration, analyzer preprocessing, optional-aware extensions and
test construction. ADR-0001, **Ubiquitous language for Lizard domain concepts**,
`docs/adrs/0001-ubiquitous-language-accepted.md`, constrains retained function,
CCN, NLOC, token and parameter meanings. There is no architectural conflict or
exception, new runtime bracket counter, whole-stream buffer or issue switch.

## Ordered refactor decision

This read-only decision pass preceded all edits and tests.

- Duplication: the annotation reader owns its type bracket stack; binding
  frames own formal/default recognition; runtime nested state machines own
  runtime scopes. These are distinct responsibilities. Lookahead reuses the
  annotation reader with `context=None`; its grammar is not duplicated.
  Exactly-once replay remains in `_replay_token` and does not feed bindings
  twice. Optional punctuation uses the established `is_optional` protocol.
- Domain naming: annotation, declaration, signature, binding and member roles
  describe parser capabilities. The JS/TS/JSX/Vue dialect selections express
  language/container differences. No story-specific product names remain.
- Shotgun surgery: class-member role is set at the actual class body and
  carried by the existing member owner; initializer `=` transfers that role
  to runtime syntax. Optional-aware extensions consume one existing protocol.
  No additional recognition layer or unrelated shared abstraction is needed.
- Dead/redundant code: the earlier unreachable fallback was already removed.
  Extraction had left two imports routed through the declaration module.
  Direct imports remove that stale ownership indirection while retaining
  historic module aliases. No other supported dead-code candidate was found.
- Behavioral documentation: the existing helper analyzes actual source through
  `analyze_file`/`FileAnalyzer`, including the actual Vue script wrapper. Exact
  function lists, metrics, formals, spans, CLI CSV and HTTP results distinguish
  the promises. Parameterized variants retain distinct ownership cases; no
  assertion was narrowed or redundant test added.
- File size: current runtime/test paths range from 16 to 250 lines; the new
  regression file is 241. This authored report is below 250. Generated JSON and
  consumer-trace artifacts are excluded from the authored-code size gate.

## Inspected observable proof

`test/test_languages/testTypeScriptAnnotationBoundaries.py` retains these
actual setups and observations, all rerun on the direct-import bytes:

| Setup | Observing assertions |
| --- | --- |
| Minimal and continued labels, seven suffixes | Lines 18–32: actual anonymous and successor names, CCN, span, formals, NLOC and token count |
| Type-only fields and actual initializer | Lines 41–57: no phantom functions; constructor/method/after survive; initializer keeps `d`, `n`, CCN2 |
| Assertions at array, comma and method brace | Lines 67–74: exact actual callback/method lists and successor |
| Nested annotation brackets, function types, tuple, conditional and template types | Lines 83–99: `x`/`y` formals, defaults, runtime arrows, names and CCN |
| Extension, CLI and HTTP consumers | Lines 104–144: NS nesting, nonstrict CCN, CSV/`-o` rows and HTTP JSON actual function list |
| Dialect and generic-header ownership | Lines 148–174: legacy JS members versus typed fields, actual Vue tags and preserved constructors/methods |
| Negative/template return, declarator newline, conditional handoff | Lines 180–205: retained anonymous formal, actual `run`, both runtime branches and successor |
| Optional header versus initializer decisions | Lines 218–241: actual initializer formals, enclosing CCN3, exactly two runtime question tags |

The harness at `testJavaScriptBindingBoundaries.py:38` invokes real public
analysis and wraps Vue source in `<script lang="ts">`. Its summary at line46
also checks parameter count against actual named parameters. Existing authored
#525 cases remain at lines14–35 and are executed by the complete language and
consumer suites. No fixture supplies the parsed function result.

## Proof correspondence and terminal checks

The incoming implementation boundary, source fixtures, parser logic and
assertion locations remain unchanged. The two changed imports invalidate only
the prior module-loading-byte correspondence; `import-provenance.json` freshly
observes class identity at all four import paths and records actual checkout
module origins. Complete affected language, consumer and output suites were
rerun, rather than substituting helper-only tests.

| Receipt | Terminal result | Correspondence |
| --- | --- | --- |
| `languages-after-refactor.json` | Exit0; 1097 passed, 5 existing skips, 1.54s | Replaces accepted complete-language receipt for current import bytes; all 200 new cases included |
| `consumers-after-refactor.json` | Exit0; 699 passed, 2 existing skips, 1.25s | Replaces accepted application/API/options/extensions/HTTP receipt for current imports |
| `outputs-after-refactor.json` | Exit0; 64 passed, 3.36s | Replaces accepted output/CSV/`-o`/website-smoke receipt for current imports |

Each receipt retains exact executed child `argv`, resolved test paths, stdout,
stderr, terminal status, ten current hashes and imported-module provenance.
The actual launcher was an inline sequential Python runner entered with
`PIP_QUIET=3 nix develop -c python -`; each child used
`subprocess.run(argv, capture_output=True, text=True)`. Receipts also label the
equivalent standalone Nix command explicitly. No nonexistent wrapper is claimed
as executed. No full suite or timing ran in this pass.

Accepted optional-extension (44 pass), lexical (24 pass), additional consumer
(1022 pass) and actual-fixture-capture (200 pass) observations are reused for
their unchanged setup, parsing and assertion boundaries. Their behavior is
also exercised where applicable by the fresh complete suites. The historical
fixture sources/results remain immutable; no recapture is needed for an
import-only change to the same class object. The accepted receipts' explicit
per-file `hashes` maps and implementation `manifest.json` establish the incoming
bytes; legacy top-level candidate/test hash fields are not used for that claim.

`verification-decision.json` records the host, load, known competing work and
choice to run these inexpensive suites serially using retained duration
evidence. Root held all foreground Nix/heavy work during the pass. Required
full pytest/make, applicable style/hook checks, frozen paired performance and
exact-head hosted CI remain with root, now that fixed production bytes and
focused proof are available. This report makes no performance verdict.

## Consumer trace

The installed `consumer-trace.mjs` produced `aggregate-consumer-trace.md`
(`--base HEAD` plus annotation/member/optional effect terms) and
`import-consumer-trace.md` (both refactored paths plus class/import/lookahead
terms). Both commands exited0; all Nix entries were serial. Each located
consumer has an explicit disposition in the generated table and in
`consumer-dispositions.json`.

The current trace union contains446 paths. Its union with the accepted650-path
record contains719:59 freshly run,113 reused and547 excluded with reasons.
The69 additions are five explicitly inspected reader/type owners plus planning,
review evidence and website-release lexical matches. Historic evidence and
independent deployment machinery are not analyzer-import consumers. Their
source/import distinctions and unchanged contracts are recorded per path.
The imported reader/type owners receive current import-identity and complete
language/consumer proof. Existing source-edge obligations are all retained in
`source-edge-correspondence.json`, including optional metrics, runtime
initializer signatures, split template types, context-None lookahead and
exactly-once delimiter replay. No listed consumer is left undispositioned.

Consequential finding: extracting the type reader made direct annotation-owner
dependencies possible without changing compatibility import paths or duplicating
lookahead grammar. The aggregate needs no further structural change on the
observed paths. No new required behavior discrepancy or unresolved semantic
gap was found in those paths; this is not a claim of complete TypeScript grammar
support. Root retains every unexecuted delivery/performance obligation.

Approximate active elapsed time: 25 minutes, including source/assertion and
consumer inspection. Final whitespace check: `git diff --check`, exit0.
No commit, push, CI observer mutation, merge or issue action occurred.

Foreground Nix ownership was explicitly released to root after the focused
receipts and manifest were written. All owned commands are terminal; no owned
process remains running. Subsequent report work used apply_patch and Git only.

## REFACTOR COMPLETE
