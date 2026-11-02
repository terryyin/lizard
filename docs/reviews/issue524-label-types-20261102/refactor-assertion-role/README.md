# Independent assertion ownership refactor review

Fresh independent `dough-post-change-refactor` of the same slice 1 in
`SEED-024#resolve-issue524-label-types`, after the final assertion ownership and
export-default correction. Outcome: **none — already clean**. No product, test,
fixture, corpus or harness implementation changed in this review. Only this
directory's audit and review records were added. No file was renamed, extracted,
split or deleted.

This conclusion covers the actual promised paths and their retained consumers.
It is structural completion, not full-check, performance or delivery acceptance.
The coordinator still owns current full checks, all qualification gates and
external publication/merge/closure.

## Scope and decision before verification

Read AGENTS.md, agent-map, the post-change-refactor skill and ordered checks,
language-reader guidance, the supplied seed/plan, executable-proof consumer and
reuse rules, accepted aggregate/cost refactor records, current aggregate Git
diff, the isolated `correction-after-export.patch`, and actual runtime/test
sources. `decision.json` was written before the correspondence audit. No tests
were run before or after that decision.

| Ordered check | Decision |
| --- | --- |
| Duplication | Assertion prefixes derive from existing operand and regex expression-start grammar without changing those owners. Local `default` and contextual-word differences belong to assertion eligibility. No parallel recognizer or ordinary-call wrapper is justified. |
| Domain naming | Operand, postfix operand and closed/qualified/reference type facts describe syntax responsibilities. No story-shaped product concept or metric reinterpretation was introduced. |
| Shotgun surgery | Declarations own assertion eligibility, runtime statement continuation owns replay context, and the annotation owner owns type brackets/completion and returned delimiters. Signature lookahead calls the same owner. |
| Dead/redundant code | Each new capability is reached by current dispatch and actual public regressions. Direct annotation imports, historic aliases and the accepted unreachable signature-fallback removal remain intact. |
| Behavioral documentation | Actual source/wrappers observe function lists, names, formals, spans, CCN, successors and phantom absence. Runtime identifier calls, genuine assertions, runtime operators and genuine generic references have distinct purposes; consolidation would obscure them. |
| File size | Literal `wc -l` confirms all11 changed product/test files are at most250 lines. Planning records and generated evidence retain the caller's accepted exclusion. |

There is one existing production subsystem: JS/TS language readers. No boundary
or callable protocol was moved. ADR-0001, **Ubiquitous language for Lizard domain
concepts**, `docs/adrs/0001-ubiquitous-language-accepted.md`, preserves the existing
function, CCN, NLOC, token and parameter meanings. The ADR index and Accepted
record agree; no conflict, supersession or exception was found. No disputed
plan restriction or confirmed behavior discrepancy was found on these paths.

## Inspected owners and untouched callers

| Owner or edge | Inspected relationship |
| --- | --- |
| `typescript_declarations.py:3–16,20–31,129–138` | Reuses read-only `OPERAND_WORDS`, `ends_expression` and `_EXPRESSION_KEYWORDS`; local prefix difference includes `default`. Assertion consumption requires an actual previous operand and rejects newline, declaration/class pending and statement-start context. |
| `typescript_states.py:14,74–114,197–214,237–246` | Existing rare tail dispatch captures the postfix `!` fact; the existing statement continuation clears `_prev_token` before replay. Declaration/marker order and ordinary-call dispatch retain their accepted cost shape. |
| `typescript_type_annotation.py:8–15,35–97` | Root runtime terminators return through the saved token. Existing bracket ownership protects nested syntax. Positive closed types can return `<`; qualified references, opener-derived reference parentheses and atomic generic tokens retain legitimate type arguments. |
| `typescript_declarations.py:155–162` and `typescript_states.py:49–57` | Annotation callback uses `_replay_token`, bypassing duplicate binding input and retaining exactly-once delimiter dispatch. |
| `code_reader.py:17–21,43–56` | The dispatch calls the state before updating `last_token`, so assertion/postfix decisions inspect the actual consumed predecessor. Nested-state callback and return order remain unchanged. |
| `javascript_functions.py:7–29,38–58,75–82,183–208` | ASI operand/statement predicates, parenthesis/signature ownership, return context and property handling are unchanged. The assertion difference stays local. |
| `js_style_regex_context.py:20–32` | Shared expression-start keywords and their regex consumers remain read-only. No regex or ASI rule changed in the correction/review. |
| `typescript_signatures.py:5–23` | Ambiguous returns use the same annotation owner with `context=None`; the new type capabilities preserve that intentional caller. |
| `javascript_objects.py`, `typescript.py`, `javascript.py`, `tsx.py`, `vue.py` | Accepted member/value, binding, optional-token, dialect, template, tokenizer and preprocessing owners remain unchanged by the correction. Their aggregate diff and current source relationships were inspected. |

All six changed source edges are retained in
`../implementation-assertion-role/source-edges-after-export.json`: contextual
operand recognition, statement replay, prefix/postfix `!`, root runtime
operators, closed types versus references, and public/cost consumers. The
accepted whole-concept eight-edge record remains applicable to its unchanged
optional, template, dialect and replay boundaries.

## Actual setup and observing assertions

Read the complete 77-line `testTypeScriptAssertionRoles.py`, the complete
241-line `testTypeScriptAnnotationBoundaries.py`, and the actual public helpers
at `testJavaScriptBindingBoundaries.py:38–50`. Helpers call real
`analyze_file`/`FileAnalyzer`, insert the actual Vue TS script wrapper, and verify
parameter count agrees with parsed formals. They do not supply parsed functions.

| Promise | Actual setup and observation |
| --- | --- |
| Contextual identifier calls retain callbacks | Assertion-role lines 8–39:13 contexts ×2 words ×7 suffixes =182 actual cases, including export-default. Exact callback `arg`, CCN2, actual span, enclosing function where present and successor `tail` are observed. |
| Genuine assertions exclude type functions | Lines 42–51:36 completed operand/word/suffix cases retain only runtime `run` and `after`, including postfix `value!` and contextual identifier `as`. |
| Assertions return runtime operators | Lines 54–64:54 type/operator/suffix cases observe nested callback `arg`/CCN2, `run`/CCN1 and successor `tail`/CCN2, including actual split shift tokens. |
| Genuine generic function/reference types keep openers | Lines 67–77:18 Box/import/query cases exercise both assertion and anonymous return paths; the latter observes actual formal `arg`. |
| Prior label, field, annotation, conditional and optional promises | Annotation-boundary lines 15–99,147–241 retain minimal/continued labels, phantom absence, real initializers, nested types/templates, declaration newline, conditional branches, dialect, and optional versus runtime decisions. |
| Public consumers | Annotation-boundary lines 102–144 retain NS/nonstrict observations, CLI CSV/`-o` and HTTP JSON function records. The retained consumer record maps the enclosing application, extension and output journeys. |

The captured490 sources, wrappers and public records contain the previous476
records unchanged. The current receipt separately preserves the original200
record correspondence. Frozen40 call sources,80 default/NS records and dispatch
counts are unchanged. The audit validates these retained receipts and actual
476-record inclusion; it does not recapture fixtures or run the parser.

## Proof correspondence and terminal commands

No refactor edit invalidates accepted proof; verification is
**skipped — no refactor edits**. `audit.py` independently matched all108 current
runtime hashes,157 current test hashes and11 story-file hashes against incoming
manifest SHA256
`c5640bfc84aed773154e0a74b1e8ed4d7e94b1a9bb47721bc6c34d3da55c3ae3`.
Its complete retained command/result mappings are in `correspondence.json`.

| Reused receipt | Terminal result and current correspondence |
| --- | --- |
| `languages-after-export.json` | Exit0;1387 passed,5 existing skips in2.06s. Same current owner/test hashes, public setup and actual assertions. |
| `consumers-after-export.json` | Exit0;989 passed,2 existing skips in1.72s. Same current runtime bytes and application/API/options/extensions/HTTP boundaries. |
| `outputs-after-export.json` | Exit0;64 passed in2.34s. Same current runtime bytes and output/CSV/`-o`/website-smoke observations. |
| `fixture-capture-after-export.json` | Exit0;490 passed in2.22s. Current sources/wrappers/public records; prior476 unchanged, original200 correspondence retained. |
| `calls-correspondence-after-export.json` | Exit0; frozen40 source hashes,80 default/NS records and dispatch counts equal. This observation is not timing. |

Literal newly executed audit command:

```text
PIP_QUIET=3 nix develop -c python docs/reviews/issue524-label-types-20261102/refactor-assertion-role/audit.py
```

Terminal exit0:108 runtime,157 test and11 story files match;490 fixtures;
prior476 actual records identical;719 dispositions;6 directed edges; zero
product/test edits, test commands or timing commands.

The literal `wc -l` command and terminal values are in `commands.json`. All11
product/test files satisfy the250-line limit; states250, declarations173,
annotation owner97 and assertion regression77. Final `git diff --check` exit0.
Source reads and Git discovery were terminal; the one targeted `rg` command
returned2 because `pyproject.toml` and `.pre-commit-config.yaml` are absent.
Present Makefile/setup.cfg were then read: make owns check-only pycodestyle and
exit-zero pylint; no hook or selective formatter was introduced or run.

## Consumer trace and deferred proof

Reused installed `consumer-trace.mjs` output at
`../refactor-final/aggregate-consumer-trace.md` and
`../refactor-final/import-consumer-trace.md`, with every one of719 located paths
dispositioned in `../refactor-final/consumer-dispositions.json`:59 run,113 reused,
547 excluded with retained path-specific purpose/reason. This pass reuses that
generated record and the six current directed edges with matching current proof.
The newly added assertion regression's four actual observing test functions are
accounted for above and in the current language/consumer receipts. No necessary
caller contract changed and no broad rediscovery is justified. No uncovered
consumer gap was found within these retained boundaries.

No expensive proof was launched. Previous candidate2 timing is historical proof
for changed bytes and cannot qualify this candidate. Root owns current full
pytest/make and applicable style/hook checks, expanded490-fixture freeze and all
18 strict below3% comparisons, plus current-trunk/exact-head hosted checks and
authorized publication, merge and issue closure. Root's next condition is
matching-byte verification and a fresh host-condition check before serial
qualification. No host-load assumption or focused pass predicts that verdict.

Foreground Nix ownership was explicitly released to root after the audit and
line counts were terminal. No owned process remains running; report completion
used apply_patch and Git only. No commit, push, CI mutation, merge or issue
action occurred. Approximate active elapsed time:20 minutes.

## REFACTOR COMPLETE
