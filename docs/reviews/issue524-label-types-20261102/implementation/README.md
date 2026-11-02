# Label and type-annotation implementation return

Uncommitted semantic implementation for SEED-024#resolve-issue524-label-types, slice 1, in the recorded Story Branch checkout. No commit, publication, refactor-completion declaration, timing, full pytest, make, merge, issue action or observer mutation was performed by this implementer. Root retains those gates and the branch observer.

All ten current product/test paths and SHA256 hashes, exact checkout/head/branch, line counts, interpreter and imported module provenance are in `manifest.json`. The independently accepted signature cleanup is included at unchanged SHA256 a36101fb19d18ab86c0877972a387d1d829372e0cc156b3440b18fece67908a9; this implementer did not edit that file or refactor-owned evidence. All changed product/test files are at most 250 lines. The installed 68 slice-1 promises are copied verbatim below and in `slice1-commitments.json`; the plan/source-review remain root-owned.

## Result and ownership

Runtime colons are labels unless a declaration, formal signature or typed class member owns them. A class body receives its member role only after its generic header. Type-owned nesting remains in the annotation reader, which returns enclosing runtime delimiters through the existing saved-token replay path without feeding bindings twice. Function-type fields/assertions produce no runtime phantoms. Negative literal types retain signature lookahead with context=None.

JavaScript/MJS/CJS and JSX preserve existing colon-valued class members. TS/TSX use annotated fields. Vue selects the class-member mode at each actual script tag, including script setup and JS/default script controls. Aborted JSX candidates return code braces through the existing JSTokenizer brace owner; ordinary runtime dispatch has no added per-token wrapper. Optional punctuation belongs to a typed member header; `=` transfers ownership to its initializer. Compact optional member names normalize at the same owner. The reader marks standalone optional punctuation through the existing `is_optional` protocol before metric consumers and undoes its core CCN only in that member role; genuine runtime ternaries remain ordinary tokens. Type-owned `${` uses the existing type bracket stack, preserving split TS/Vue template types and atomic TSX controls. No parallel runtime bracket counter, whole-stream buffer or issue-specific switch was added.

ADR-0001, `docs/adrs/0001-ubiquitous-language-accepted.md`, governs retained function/CCN/NLOC/token/parameter meanings. The repair stays within the existing JS/TS reader subsystem; no architectural exception or new public export is introduced.

## Discriminating observations

| Behavior and actual setup | Observing assertions / retained raw proof |
| --- | --- |
| Minimal `[() => { a: 1 }]` plus successor, seven suffixes | Anonymous line1/CCN1/formals[], NLOC1/tokens5; after line2/CCN2/formal tail, NLOC1/tokens12. Pre-edit `red.json`: original mismatch loses anon. |
| Continued label, nested brace, while/break and later if, seven suffixes | Anonymous spans lines1-4 with expected CCN2/3; after line5 has tail/CCN2. Initially green nested-label/control cases retained. |
| Function-type fields, initializer, constructor/method, TS/TSX/Vue | Only actual constructor/s/after; actual typed initializer d retained with formal n/CCN2. Initial red phantom d is preserved in red.json. |
| Assertions in array-end, comma-next-callback and method-closing brace | Simple object type is an existing green control. Function member/generic function types had phantoms. All now report exact actual names/spans/formals/CCN, with after retained. |
| Number[], function type, Map<string,number[]>, tuple and conditional formals/returns | x/y/defaults and exact arrow/successor boundaries asserted. Existing array/function/generic controls stay green; tuple and conditional type phantoms/spans corrected. Conditional CCN5 compatibility is retained, not represented as new type-condition metric accuracy. |
| Semicolonless typed fields before constructor | semicolonless-red.json loses constructor in six cases; semicolonless-green.json passes after newline return. |
| JSX/default Vue/JS Vue versus TS setup script modes | dialect-red.json records three introduced compatibility failures; dialect-green.json and final suite prove runtime versus type-only members through actual readers. |
| Generic class constraint object and negative literal return type | owner-probes.json and owner-regressions-red.json record phantom d, TSX lost after, and lost formal x; owner-regressions-green.json and final suites pass. |
| Multi-declarator typed binding across newline | declaration-probe.json and provenance-verified declaration-baseline.json confirm introduced anonymous-vs-run mismatch only across newline. declaration-red.json records 3 failures; declaration-green.json and final suites retain actual run formal n/CCN2 and after, same-line and multiline. |
| Conditional type followed by runtime ternary | Official TypeScript5.7.3 TS/TSX parser/transpiler accepts exact unparenthesized and parenthesized forms, emits runtime yes/no branches; conditional-handoff-typescript-parser.json retains version/hash/AST/source/emitted JS. Verified da29 baseline returns yes/no/after across TS/TSX/Vue. conditional-handoff-red.json: 3 failed,146 passed; one-line role reset then200 passed. Exact branch names/formals/spans/CCN1 asserted. |
| Optional member spelling and actual initializers | Official TS5.7.3 accepts compact/spaced/$ optional fields and initializer forms. optional-fields-red.json15fail/158pass captures type phantoms and lost/wrong initializer names; member/value ownership fixes them.24 retained before/after function observations match including NLOC/tokens; original9 root compact/ternary controls rerun9pass without changing refactor files. |
| Optional-aware extension metrics | optional-field-tags-red.json3fail/182pass shows extra type-marker tags after the first field repair. Existing is_optional protocol fixes it; outer CCN3 still counts both genuine runtime decisions.18 public ND/cognitive/complextags/ignoreassert/modified/McCabe extension/suffix comparisons agree across compact/spaced/$ forms. |
| Template-type interpolation ownership | template-type-probes.json confirms TS/Vue split `${` while TSX is atomic. Official parser accepts assertion/formal/named/anonymous return setups. template-types-red.json10fail/190pass loses successor/formal in TS/Vue; adding `${` to the existing type bracket map gives template-types-green.json200pass. Existing parameter sets cover array-end/comma/runtime brace, valid formals/defaults and context-None anonymous return. |
| Extensions and real consumers | NS/nonstrict label CCN and nesting assertions; real per-suffix CLI --csv -o rows and HTTP actual-function filtering. Existing PR525 authored cases, other binding controls, output/parameter/extension suites retained. |

Initial pre-product run: **44 failed, 71 passed**. That log is immutable `red.json`. Subsequent failed candidates/expectations remain in attempt1/attempt2, languages-attempt1/languages-attempt2, semicolonless-red, dialect-red, owner-regressions-red declaration-red, conditional-handoff-red, optional-fields-red, optional-field-tags-red and template-types-red JSON. Two originally guessed new token counts (7/13) were corrected to independently observed 5/12: unlabeled callback has 3 tokens plus the 2 label tokens, and the standalone successor has 12. No function-retention, name, formal, span or CCN assertion was removed.

`conditional-baseline.json` retains the first subprocess observation. `conditional-baseline-imports.json` supplements it using an actual probe.py inside the exact da29 archive, with lizard.__file__, TypeScript module paths and hashes. It confirms retained f CCN5 alongside the old phantom v/span defect. `baseline-import-probe.py` is the exact probe source.

Actual final parameterized sources are retained in `cases.json` (200 cases), grouped labels/annotations with suffix, entry point and extension. `capture-fixtures.py` observes actual fixture calls and returns through sys.setprofile without replacing analyzer logic. Helper cases retain raw source plus the actual post-wrapper analyzed_source, filename/helper provenance and returned public metrics. CLI/HTTP/Vue-mode cases retain their actual analyzed source; fixture-capture.json records the imported module paths/hashes and200-pass terminal output. Existing Vue wrappers are preserved.

## Final focused proof

All proof commands below terminated with exit 0 under PIP_QUIET=3 nix develop -c. Complete literal commands, resolved test files and raw JSON-escaped stdout/stderr are in each receipt. They describe this manifest's product/test bytes.

| Receipt | Terminal result |
| --- | --- |
| languages-complete-owner-final.json | 1097 passed, 5 skipped; complete JavaScript*/TypeScript*/ArrowObjectBoundaries/OptionalOperators/JSX/TSX/ES6/Vue* selection. |
| consumers-complete-owner-final.json | 699 passed, 2 skipped; required binding/application/options/language/analyzer/extension/nesting/cognitive/fan-in-out/Halstead/duplicated-parameter/HTTP plus new regressions. |
| outputs-complete-owner-final.json | 64 passed; output/comment/smoke/CSV/output-file consumers. |
| extra-consumers-green.json | 1022 passed; additional traced metric/output/analyzer/language consumers. This selection ran before the last JS/TS-only lexical fix; its unchanged non-JS setup/import boundary remains applicable. |
| lexical-consumers-final.json | 24 passed; additional actual tokenizer consumer, unchanged generic CodeReader contract retained. |
| optional-fields-extra-consumers.json | 44 passed; newly reached classifier/complex-tag/assertion/McCabe/ND test consumers. |
| fixture-capture.json | 200 passed; actual final fixture capture and targeted module. |

Existing skips remain visible in the logs; they are not represented as passes. Existing Ruby invalid-escape warnings remain in the additional-consumer receipt. Nix dirty-tree and earlier pip virtualenv/platformdirs warnings are environment context, not proof failures. The host remains Territory.local with 16 CPUs; checks were serial, cheap focused commands, with no owned heavy work or paired timing.

## Consumer disposition and remaining gates

Current changed member/reader/type paths and the test module are represented by five installed traces in `consumer-traces-complete-owner-final.json`, using frozen da29 and actual owner/protocol/template terms. All terminal exits are0. Three member/preprocess path receipts are reused from the matching optional-owner bytes; the type/template and test paths were freshly traced after the template repair. Earlier nine-path traces and accepted independent signature traces remain retained; unchanged owners keep their prior applicable source inspection.

`consumer-dispositions.md` and JSON disposition all650 unique retained/new consumers. `consumer-inspection-final.json` retains actual source matches/imports/symbols/assertions. `source-edges.json` explicitly dispositions eight changed-to-changed source edges otherwise excluded by aggregate trace discovery: nested member-role lookup, CCN undo, initializer/signature preservation, is_optional extension protocol, type-template bracket replay, context-None lookahead, exactly-once binding replay and container dialects. No test consumer lacks a run receipt. Lexical/EOF/line/discovery and actual output/application consumers remain covered; historical workflow/evidence references are excluded with boundary-specific reasons.

All ten current product/test hashes are in manifest.json and all files are<=250 lines (runtime state250, test241). Accepted independent signature SHA a36101fb19d18ab86c0877972a387d1d829372e0cc156b3440b18fece67908a9 remains unchanged. `../refactor/README.md` and its correspondence receipts own that earlier cleanup; a fresh independent aggregate pass is still required after this return. The implementer makes no refactor-completion declaration.

No confirmed semantic gap remains in the requested observed paths. Root owns fresh independent refactoring; frozen complete/label/annotation/call-group API/parameter/NS and broad CLI strict paired95% upper slowdown<3% against da29 and cumulative6d288d4; full pytest/make; exact-head applicable hosted checks; publication/merge/issue summary and closure. No timing result is claimed. The final manifest and these semantic receipts preserve all earlier failed observations. The first-return and149-case returns are recoverable through retained `*-before-conditional-handoff` and `*-before-optional-fields` manifest/cases/README snapshots and raw receipts. The latest capture has200 cases. Root reports newer trunk changes limited to website/workflow/docs/backlog; reconciliation and hosted-workflow applicability remain root-owned.

Official [TypeScript conditional-type documentation](https://www.typescriptlang.org/docs/handbook/2/conditional-types.html) supports the type grammar. Actual official compiler5.7.3 AST/transpilation receipts establish exact conditional, optional-member and template-type syntax: conditional-handoff-typescript-parser.json, optional-field-typescript-parser.json and template-type-typescript-parser.json. Each records version/path/hash, literal source, diagnostics and emitted runtime JavaScript.

Consequential learnings: successor survival alone is weak minimal-label proof. Member headers, initializer values and type brackets must carry their own punctuation roles. Optional punctuation must communicate that role to established metric consumers, not only core CCN. TS/Vue split type-template `${` belongs to type nesting; TSX atomic templates remain a control. Context-None return lookahead shares the same negative/template/type delimiter behavior. A declarator comma transfers type ownership across line breaks while statement cleanup resets names/modifiers.

All owned commands are terminal. Foreground Nix ownership is explicitly released in this implementation return; no owned subprocess remains running. `git diff --check` is the final whitespace gate; no independent formatter/hook lint, product generator, full suite, make, timing, observer mutation, commit/push or external delivery action was performed by this implementer.

## Verbatim slice-1 commitments

- **C01** Function retention: Retain every runtime function affected by labeled blocks sharing enclosing brackets.
- **C02** Metric retention: Retain those functions’ metrics.
- **C03** Annotation function retention: Retain every runtime function affected by type annotations sharing enclosing brackets.
- **C04** Annotation metric retention: Retain those functions’ metrics.
- **C05** Ownership: Correct the distinction between runtime-label ownership and type-annotation ownership.
- **C06** Termination: Correct annotation termination at enclosing runtime delimiters.
- **C07** Replay: Correct replay of enclosing runtime delimiters to their runtime reader.
- **C08** Field investigation: Investigate related function-type class-field cases.
- **C09** Assertion investigation: Investigate related object-type assertion cases.
- **C10** Related corrections: Correct confirmed failures within the same ownership concept.
- **C11** Label interpretation: Treat a runtime labeled statement as runtime syntax.
- **C12** Label continuation: Preserve later decisions within the labeled runtime block.
- **C13** Statement continuation: Preserve later statements within that block.
- **C14** Runtime closure: Close the block at its actual runtime brace.
- **C15** Brace boundary: Prevent annotations from swallowing an unmatched enclosing-runtime `}`.
- **C16** Bracket boundary: Prevent annotations from swallowing an unmatched enclosing-runtime `]`.
- **C17** Comma boundary: Prevent annotations from swallowing an unmatched enclosing-runtime `,`.
- **C18** Type brackets: Preserve legitimate nested type brackets.
- **C19** Type commas: Preserve legitimate nested type commas.
- **C20** Type arrows: Preserve legitimate function-type arrows.
- **C21** Signatures: Preserve valid signatures.
- **C22** Defaults: Preserve valid defaults.
- **C23** Ternaries: Preserve valid ternaries.
- **C24** Templates: Preserve valid templates.
- **C25** Extension metrics: Preserve established extension metrics.
- **C26** CLI output: Preserve established CLI output.
- **C27** Earlier fixes: Preserve all previously fixed #524 examples.
- **C28** Original examples: Preserve established behavior for the six original issue examples.
- **C29** Authored PR examples: Preserve established behavior for all eleven authored PR #525 examples.
- **C30** Suffixes: Preserve established behavior across the seven JS-family suffixes.
- **C31** NS: Preserve established NS behavior.
- **C32** Nonstrict: Preserve established nonstrict behavior.
- **C33** CSV: Preserve established CSV-consumer behavior.
- **C34** Output option: Preserve established `-o`-consumer behavior.
- **C35** HTTP: Preserve established HTTP-consumer behavior.
- **C36** Regression proof: Add discriminating regression tests.
- **C37** Refactoring: Independently refactor the aggregate change.
- **C38** API performance: Qualify affected API consumers.
- **C39** Parameter performance: Qualify affected parameter consumers.
- **C40** Extension performance: Qualify affected extension consumers.
- **C41** CLI performance: Qualify broad CLI performance.
- **C42** Pytest: Run the full pytest suite successfully.
- **C43** Make: Run the full make checks successfully.
- **C49** Observation retention: Retain failed observations.
- **C50** Timing retention: Retain immutable timing inputs.
- **C51** Public meanings: Preserve public metric meanings.
- **C52** Working tree: Preserve unrelated working-tree changes.
- **Q01** Performance threshold: Each required performance qualification must satisfy a strict paired 95% upper slowdown bound below 3%; equality does not satisfy “below.”
- **Q02** Related-case scope: Field/assertion corrections are conditional on confirming failures that exercise the same ownership responsibility.
- **Q03** Ownership uncertainty: Determine the smallest existing reader/signature owners that distinguish runtime labels and own nested type syntax.
- **Q04** Investigation order: Confirm the exact related field/assertion failures before choosing their implementation.
- **Q07** Authority: The human explicitly authorizes implementation.
- **Q08** Publication authority: The human explicitly authorizes publication.
- **T01** Minimal labeled-block example: `const x = [() => { a: 1 }];` must report one anonymous function.
- **T02** Minimal-example location: That function must be reported at line 1.
- **T03** Reference behavior: The minimal example must match the stated 1.24.1 behavior; the source reports zero functions on master `da29d5d`.
- **T04** Successor name: A following function retains its own name.
- **T05** Successor formals: A following function retains its own formals.
- **T06** Successor span: A following function retains its own span.
- **T07** Successor complexity: A following function retains its own CCN.
- **T08** Field-to-constructor journey: A function-type class field preserves a following constructor when it exercises the shared ownership responsibility.
- **T09** Field-to-method journey: A function-type class field preserves a following method under that qualification.
- **T10** Assertion-to-arrow journey: An object-type assertion preserves expression-arrow boundaries under that qualification.
- **T11** Assertion-to-successor journey: An object-type assertion preserves successor boundaries under that qualification.
- **T12** Example reach: These examples demonstrate required behavior; they do not restrict the promise to only those spellings, delimiters, or successor forms.
- **X01** Counter exclusion: Do not introduce a parallel runtime bracket counter.
- **X02** Buffer exclusion: Do not introduce a whole-stream buffer.
- **X03** Switch exclusion: Do not introduce an issue-specific switch.
