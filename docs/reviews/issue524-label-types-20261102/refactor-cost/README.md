# Independent cost-refactor review

Fresh independent `dough-post-change-refactor` of the same slice 1 G12 cost
correction in `SEED-024#resolve-issue524-label-types`, using the supplied plan
and accepted whole-concept review. Outcome: **none — already clean**. This pass
changes no product, test, corpus or harness bytes. Only this directory's review
records were added. No file was renamed, extracted, split or deleted.

The cost correction keeps existing semantic owners and their execution order.
No additional abstraction, consolidation or naming change is justified by the
three-file cost diff. This is a structural review result, not performance
acceptance: G12 remains unresolved until root qualifies every required frozen
comparison below its strict paired 95% upper slowdown bound of 3%.

## Scope and ordered decision

Read AGENTS.md, agent-map, post-change-refactor and its checks, language-reader
guidance, consumer/proof and planning constraints, the seed and current plan,
the actual aggregate Git diff and the isolated `implementation-cost/cost.patch`.
The read-only decision preceded all verification; `decision.json` retains it.

| Check | Finding |
| --- | --- |
| Duplication | Cheap class/dialect preconditions avoid irrelevant helper/string work. The complete member rule remains `_typed_class_member`; declaration, annotation and return-signature owners retain their existing rules. These guards do not duplicate type grammar. |
| Domain naming | Declaration/marker, trailing-token and annotation-punctuation sets describe dispatch groups. They introduce no story-shaped product owner or altered metric meaning. |
| Shotgun surgery | Class role, field initializer handoff, binding ownership, annotation brackets and delimiter replay retain their existing authoritative homes. No parallel bracket counter, buffer or recognizer is added. |
| Dead/redundant code | Gates have current callers and measured hot-path purpose. Direct type-owner imports, historic aliases and accepted signature-fallback removal remain intact. No supported deletion candidate found. |
| Behavioral documentation | Actual public analysis/wrappers and observations distinguish function lists, names, formals, spans, CCN and external output. No assertion needs narrowing or extra test. |
| File size | Literal `wc -l` reports product files 16–250 lines, regression file 241 and active plan 246. Generated JSON and installed trace output retain their existing exclusion. |

The cost diff stays inside the existing JS/TS language-reader subsystem. ADR-0001,
**Ubiquitous language for Lizard domain concepts**,
`docs/adrs/0001-ubiquitous-language-accepted.md`, preserves function, CCN, NLOC,
token and parameter meanings. The ADR index and Accepted record agree; there is
no conflict, supersession or exception. Core analysis, tokenizer rules, binding
and replay contracts are unchanged.

## Current source and inspected edges

| Owner | Inspected current locations and preserved relationship |
| --- | --- |
| `javascript_objects.py` | Lines 5–19 retain full typed/optional role and CCN undo; 23–28 and 53–56 add only necessary class guards. Runtime initializer handling 23–24/79–95 and class-role handoff 112–124 are unchanged by this correction. |
| `typescript_states.py` | Lines 11–14 derive the union from the existing sets. Lines 74–114 still resolve bindings, handle pending declarations, then dispatch markers; return-colon, optional-field and ternary handling retain their order. Lines 187–214 retain statement termination before mutually exclusive backtick/colon/comma tail work and final token updates. |
| `typescript.py` | Lines 82–83/194/202–220 group only `?` and `:`. Optional marking still uses `is_optional`; return context, `tee`, lookahead, yield and buffered replay are ordered as before. Direct annotation import at line 11 remains intact. |
| Required unchanged callers | `typescript_declarations.py:8–10,14–42,139–157`; `javascript_functions.py:38–58`; `javascript_bindings.py:142–179`; `typescript_signatures.py:5,12–23` preserve declaration sets, class/binding context, context-None lookahead and exactly-once replay. |
| Dialect/container and observers | `javascript.py:13`, `tsx.py:23–26`, `vue.py:26–40` own the shared context dialect flag; `file_analyzer.py:13–16,65–70,94–102` drives real preprocessing/CCN; `lizardcomplextags.py:15–25` and `cognitive_brace.py:25–27` observe the established optional token protocol. |

All six direct edges in `../implementation-cost/source-edges.json` were
inspected and retained: declaration/marker order, tail punctuation, class
member role, preprocessing annotation/optional role, API/parameter/NS records,
and CLI/CSV/`-o`/HTTP/extensions. The accepted whole-concept eight-edge record
remains applicable. No new contract, import path, consumer schema or semantic
owner is introduced by this cost correction or by this review.

## Actual setup and observing assertions

The full 241-line `testTypeScriptAnnotationBoundaries.py` was read unchanged.
Its imported helper at `testJavaScriptBindingBoundaries.py:38–43` invokes real
`analyze_file`/`FileAnalyzer` and adds the actual Vue TS script wrapper. Its
summary at lines 46–50 checks parameter count against actual parsed formals.

| Promise reached by the cost gates | Actual setup and observation locations |
| --- | --- |
| Label continuation and minimal callback | Annotation-boundary lines 15–32: actual callback/successor lists, CCN, formals, spans, NLOC and token count. |
| Type-only fields versus runtime initializer | Lines 35–57: phantom absence with actual constructors/methods/successor, named initializer `d` and formal `n`. |
| Type delimiters, nested brackets, return lookahead and templates | Lines 60–99, 177–205: exact callback/method/successor lists, formals, declaration newline ownership and both actual runtime ternary branches. |
| Dialect/class role | Lines 147–174: JS legacy members, actual Vue script tags and generic constraint controls. |
| Optional marker versus real initializer decisions | Lines 208–241: compact/spaced/$ fields and initializers, enclosing CCN3, exactly two runtime complex-tags questions. |
| Public consumers | Lines 102–144: NS/nonstrict, CLI CSV/`-o` and HTTP JSON function records. |
| Existing declaration and custom dispatch contracts | `testTypeScriptDeclarations.py:6–31,34–87` uses actual TS analysis; `testTypeScriptReaderDispatch.py:57–75,78–122,136–181` observes hooks, emitted tokens, CCN, parameters, EOF, errors and parallel cleanup. |
| Existing optional punctuation | `testOptionalOperators.py:64–111,114–126` observes optional/nullish versus runtime ternaries through public analysis. |

These tests construct source and inspect parser results; their fixtures do not
supply parsed functions. The accepted 200 actual captured cases are exactly
equal to the cost correction's recapture, including source, suffix, wrapper,
entry, extension and public observed records.

## Reused proof and current correspondence

No refactor edit invalidates accepted proof. Verification was
**skipped — no refactor edits**; no test, benchmark, formatter or hook lint ran.
The final read/hash audit exited 0 and wrote `correspondence.json`; its literal
command is retained in `correspondence-command.txt`.

| Reused cost receipt | Actual retained command/result and correspondence |
| --- | --- |
| `languages.json` | Its literal full `PIP_QUIET=3 nix develop -c python -m pytest … -q` command is in the receipt and correspondence JSON: exit0, 1097 passed/5 existing skips. Complete affected JS/TS/JSX/TSX/Vue, optional, template and custom-dispatch coverage includes all 200 cases. |
| `consumers.json` | Its literal full Nix pytest command is in both records: exit0, 699 passed/2 existing skips. Application/API/options/extensions/HTTP observations retain unchanged setup and assertions. |
| `outputs.json` | `PIP_QUIET=3 nix develop -c python -m pytest test/testOutput.py test/testCommentOptions.py test/test_website_smoke.py test/testOutputCSV.py test/testOutputFile.py -q`: exit0, 64 passed. Output/CSV/`-o` observations remain unchanged. |
| `calls-before.json`, `calls-after.json` | Exact frozen 40 current sources and 80 default/NS public observations are equal, including full/filtered formals, counts, long names, spans, CCN/NLOC/tokens and NS nesting. This is correspondence, not timing. |
| `fixture-capture.json`, `cases.json` | Existing command exit0, 200 cases. All captured cases equal accepted implementation cases; no new capture was needed in this pass. |

The audit verifies all 108 current runtime hashes against the cost manifest;
exactly three differ from its recorded failed candidate and the other 105 are
unchanged. Stored before-sources match their hashes. Current regression/test
and focused receipt hashes also match. The three current cost-owner hashes are:

| Path | SHA256 |
| --- | --- |
| `lizard_languages/javascript_objects.py` | `d9f484af96da6405d625429704a1d968f2949936b2cccf093e10b49f672b6c25` |
| `lizard_languages/typescript_states.py` | `aba2b65df01d7c173c2649c9954f53b5e95a422947937947cd0dc1612fdc904a` |
| `lizard_languages/typescript.py` | `c856cb2dbb693c4fa6c90a187d07aa9e7b0c9314add4798d5defa614ba528a05` |

The regression hash remains
`4b1d1ed0543ab1b4b75181fcfc78718506d2dacfb5d7b350cd34f8863fe358a6`.
Current annotation-owner and signature hashes remain in `correspondence.json`.

## Consumer trace and retained obligations

Reused installed `consumer-trace.mjs` output at
`../refactor-final/aggregate-consumer-trace.md` and
`../refactor-final/import-consumer-trace.md`, with all 719 located paths
dispositioned in `../refactor-final/consumer-dispositions.json`: 59 run,
113 reused, 547 excluded with per-path reasons. This review reuses those
dispositions and the changed-edge current proof above. No broad rediscovery
or new trace is justified because no refactor edit or import/caller contract
changed. No uncovered consumer was found in these retained boundaries.

No semantic discrepancy or disputed product restriction was found. Reusing
passing semantic proof does not erase the failed performance verdict. The
failed current-calls gate remains +4.7751675467% mean, +6.0373328721% upper;
profiling/call-count reductions make no claim that G12 now passes.

No expensive proof was launched. Root owns full pytest/make, all 18 unchanged
frozen qualification gates and applicable hosted exact-head CI. Root's next
condition is matching-byte verification after this independent review and fresh
host-condition inspection before serial timing. No machine load assumption is
used to predict timing acceptance.

Literal read/check commands included `git status --short`, `git diff --stat`,
`git diff --cached --stat`, the aggregate `git diff --` for the eight tracked
reader paths, isolated cost-patch reads, serial Nix source/assertion reads and
the recorded `wc -l` list. `git diff --check` exited 0. All source/proof commands
were terminal before foreground Nix ownership was explicitly released to root.
No owned process remains running. Later report work used apply_patch/Git only.
No commit, push, CI observer mutation, merge or issue action occurred.

Approximate active elapsed time: 20 minutes. Consequential conclusion: a
necessary precondition can guard a hot path while the complete role rule stays
in its existing owner; these cost gates need no further structural change.

## REFACTOR COMPLETE
