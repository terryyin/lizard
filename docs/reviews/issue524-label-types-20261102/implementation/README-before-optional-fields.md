# Label and type-annotation implementation return

Uncommitted semantic implementation for SEED-024#resolve-issue524-label-types, slice 1, in the recorded Story Branch checkout. No commit, publication, refactor-completion declaration, timing, full pytest, make, merge, issue action or observer mutation was performed by this implementer. Root retains those gates and the branch observer.

Production and test bytes, exact checkout/head/branch, line counts, interpreter and imported module provenance are in `manifest.json`. All changed product/test files are at most 250 lines. The installed 68 slice-1 promises are copied verbatim below and in `slice1-commitments.json`; the plan/source-review remain root-owned.

## Result and ownership

Runtime colons are labels unless a declaration, formal signature or typed class member owns them. A class body receives its member role only after its generic header. Type-owned nesting remains in the annotation reader, which returns enclosing runtime delimiters through the existing saved-token replay path without feeding bindings twice. Function-type fields/assertions produce no runtime phantoms. Negative literal types retain signature lookahead with context=None.

JavaScript/MJS/CJS and JSX preserve existing colon-valued class members. TS/TSX use annotated fields. Vue selects the class-member mode at each actual script tag, including script setup and JS/default script controls. Aborted JSX candidates return code braces through the existing JSTokenizer brace owner; ordinary runtime dispatch has no added per-token wrapper. No parallel runtime bracket counter, whole-stream buffer or issue-specific switch was added.

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
| Conditional type followed by runtime ternary | Official TypeScript5.7.3 TS/TSX parser/transpiler accepts exact unparenthesized and parenthesized forms, emits runtime yes/no branches; conditional-handoff-typescript-parser.json retains version/hash/AST/source/emitted JS. Verified da29 baseline returns yes/no/after across TS/TSX/Vue. conditional-handoff-red.json: 3 failed,146 passed; one-line role reset then149 passed. Exact branch names/formals/spans/CCN1 asserted. |
| Extensions and real consumers | NS/nonstrict label CCN and nesting assertions; real per-suffix CLI --csv -o rows and HTTP actual-function filtering. Existing PR525 authored cases, other binding controls, output/parameter/extension suites retained. |

Initial pre-product run: **44 failed, 71 passed**. That log is immutable `red.json`. Subsequent failed candidates/expectations remain in attempt1/attempt2, languages-attempt1/languages-attempt2, semicolonless-red, dialect-red, owner-regressions-red declaration-red and conditional-handoff-red JSON. Two originally guessed new token counts (7/13) were corrected to independently observed 5/12: unlabeled callback has 3 tokens plus the 2 label tokens, and the standalone successor has 12. No function-retention, name, formal, span or CCN assertion was removed.

`conditional-baseline.json` retains the first subprocess observation. `conditional-baseline-imports.json` supplements it using an actual probe.py inside the exact da29 archive, with lizard.__file__, TypeScript module paths and hashes. It confirms retained f CCN5 alongside the old phantom v/span defect. `baseline-import-probe.py` is the exact probe source.

Actual final parameterized sources are retained in `cases.json` (149 cases), grouped labels/annotations with suffix, entry point and extension. `capture-fixtures.py` observes actual fixture calls through sys.setprofile and does not replace analyzer logic.

## Final focused proof

All proof commands below terminated with exit 0 under PIP_QUIET=3 nix develop -c. Complete literal commands, resolved test files and raw JSON-escaped stdout/stderr are in each receipt. They describe this manifest's product/test bytes.

| Receipt | Terminal result |
| --- | --- |
| languages-conditional-handoff-final.json | 1046 passed, 5 skipped; complete JavaScript*/TypeScript*/ArrowObjectBoundaries/OptionalOperators/JSX/TSX/ES6/Vue* selection. |
| consumers-conditional-handoff-final.json | 648 passed, 2 skipped; required binding/application/options/language/analyzer/extension/nesting/cognitive/fan-in-out/Halstead/duplicated-parameter/HTTP plus new regressions. |
| outputs-conditional-handoff-final.json | 64 passed; output/comment/smoke/CSV/output-file consumers. |
| extra-consumers-green.json | 1022 passed; additional traced metric/output/analyzer/language consumers. This selection ran before the last JS/TS-only lexical fix; its unchanged non-JS setup/import boundary remains applicable. |
| lexical-consumers-final.json | 24 passed; additional actual tokenizer consumer. |
| fixture-capture.json | 149 passed; actual final fixture capture and targeted module. |

Existing skips remain visible in the logs; they are not represented as passes. Existing Ruby invalid-escape warnings remain in the additional-consumer receipt. Nix dirty-tree and earlier pip virtualenv/platformdirs warnings are environment context, not proof failures. The host remains Territory.local with 16 CPUs; checks were serial, cheap focused commands, with no owned heavy work or paired timing.

## Consumer disposition and remaining gates

`consumer-traces-return-final.json` retains nine installed consumer-trace commands using frozen base da29 and each changed product/test path with actual semantic/lexical effect terms. After the confirmed conditional-type handoff repair, the two changed paths were refreshed in `consumer-traces-conditional-handoff.json`; the other seven paths/reached contracts are unchanged and their trace inspection is reused. All terminal exits are 0. `consumer-dispositions.md` and JSON disposition every one of the 558 unique listed consumers. `consumer-inspection-final.json` retains their source matches, imports, symbols and actual test assertions. Lexical traces include generate_tokens, JSTokenizer, aborted candidates, EOF callbacks, discovery and original line controls; they are not limited to annotation state references.

No confirmed semantic regression remains in the requested exercised paths. Open acceptance work is owned by root: independent aggregate refactoring; frozen complete/label/annotation/call-group plus API/parameter/NS and broad CLI strict paired95% upper slowdown <3% against da29 and cumulative6d288d4; full pytest/make; exact-head hosted checks; delivery/merge/issue summary and closure. No measured slowdown result is claimed here. Source qualification families/corpora must not drop these failed observations. The first return's manifest/cases/README are preserved as `*-before-conditional-handoff` alongside all original raw receipts; new fixture capture has149 cases. Official [TypeScript conditional-type documentation](https://www.typescriptlang.org/docs/handbook/2/conditional-types.html) supports the type grammar; the retained actual compiler AST establishes the runtime handoff for this exact source.

Consequential learnings: a successor surviving alone is weak minimal-label proof; already-green nested labels and simple assertions stay controls. Type-only member discovery needs the reader/container dialect. Generic constraint braces belong to header types, and aborted JSX fallback must return their brace accounting to the existing tokenizer owner. Return lookahead needs the same negative-prefix/type delimiter behavior with context=None. A declarator comma transfers the active binding/type role across a line break even when statement cleanup resets function-name/modifier state.

Foreground Nix ownership is released after the final report command terminates; no owned subprocess is running. Root may begin independent refactoring and its qualified proof. `git diff --check` terminated 0; no independent lint/format hook or lifecycle generator ran.

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
