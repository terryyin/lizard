# Assertion ownership and runtime delimiter return

Same active slice 1, G14–G16. The change restores contextual-identifier callbacks, retains genuine assertions after completed operands, and returns confirmed runtime operators from the type reader. All directed known losses, including the six observed left shifts, now have passing regressions. No numeric performance claim applies to these changed bytes.

## Preserved candidate and initial observations

`before-hashes.json` preserves all 108 candidate2 runtime hashes and existing test hashes before edits. The actual three pre-edit owner files are under `before/lizard_languages/`. `candidate2-runtime/` reconstructs all 108 original runtime files, verifying every byte against those hashes before use. Its isolated module paths/hashes are recorded in the pre-control receipts. `correction.patch` is the complete before/after owner and new-test patch; SHA256 `5b17f0b916379a68b066e065c67af83dde5adebc9b7692c618167c37e59fa6e5`.

Root's candidate2 all-18 performance results and full-check receipts remain unchanged under `../evidence/`. They qualify that earlier candidate, not this return. No timing, full pytest, make, CI, publication or issue action was performed by this implementer.

Root's retained 31-row owner probe first demonstrated 14 regressions: operand-position calls named `as` or `satisfies` lost their callbacks. Property/member controls were already correct through the existing field owner, and genuine assertion controls were retained. The old assertion prefix blacklist mistook expression starts for completed operands.

The bounded before-edit observations are retained in `candidate-before.json`, `baseline.json`, `official-parser.json`, and the exact sources in `fixtures.json`. They cover contextual calls at conditional/logical/unary/new operands, return/throw/case/else/if/await statement contexts, semicolon and no-semicolon line starts, genuine assertions after values/groups/calls/new/non-null expressions, and the directed relational handoff. The frozen da29 baseline retains the runtime callbacks; its old type-only assertion phantoms remain visible rather than relabeled as correct. Official TypeScript 5.7.3 accepts all 84 TS/TSX source/parser observations without diagnostics.

## Owner changes

`typescript_declarations.py` decides assertion ownership only when the contextual word is reached. It uses consumed `last_token`, the existing expression-prefix grammar from `js_style_regex_context`, existing operand-word semantics, newline state and declaration/class/member roles. The ASI-oriented helpers remain unchanged. A completed group or `new` expression uses actual consumed syntax rather than sticky `_prev_token` call routing. Runtime identifier roles remain available for contextual type words.

`typescript_states.py` marks an actual single-statement continuation before replay, so a control condition's closing parenthesis does not masquerade as the operand before a new statement. This stays in the existing control-statement owner; ordinary call groups gain no callback wrapper or allocation. The existing rare tail gate records a non-null postfix operand fact for `!`, preserving genuine `value!` assertions while allowing callbacks in `!as(...)`. No counter, token wrapper or stream buffer was added.

`typescript_type_annotation.py` returns root `>`, `>=`, `<=`, `instanceof`, `in` and `^` through the existing saved-delimiter replay. Its type bracket stack still owns nested operators and generic closers. Right shifts are emitted as separate `>` tokens and remain runtime after the first is returned.

For the confirmed `<<` losses, the type owner records positively non-generic or completed type tails. A following `<` returns immediately to runtime without consuming the next token. Primitive/literal types, completed generic/bracketed types and Vue's atomic generic-group representation participate in this capability. Live and qualified type references retain generic openers. Parenthesis capability derives from its opener's existing expectation/reference context, preserving import/query type arguments instead of treating every `)` as non-generic.

The existing rare-token cost gates, direct annotation imports/compatibility aliases, optional `is_optional` protocol, `${` type nesting, ASI, regex scanning, field owner and ordinary-call allocation/replay are preserved. This remains the same JS/TS subsystem and follows ADR-0001's public function/CCN/NLOC/token/parameter meanings.

## Red and green proof

All failures and intermediate candidates remain retained. No assertion was weakened and all 200 prior cases remain unchanged.

| Receipt | Actual terminal observation |
| --- | --- |
| red.json | 170 failed, 252 passed before owner edits. |
| attempt1.json | 422 passed after contextual-operand and first relational repair. |
| operator-red.json | 18 failed, 434 passed for the additional unambiguous operator family. |
| green.json | 452 passed after `instanceof`/`in`/XOR return, with right-shift controls. |
| left-shift-red.json | 6 failed, 455 passed before closed-tail correction. |
| left-shift-green.json | Despite the preliminary filename, this is a retained failed attempt: 1 failed, 460 passed. Vue's atomic generic group still swallowed the shift. |
| green-complete.json | 467 passed after the atomic-group/reference-parenthesis correction. |
| languages-final.json | 1,373 passed, 5 existing skipped; includes all final 476 cases and complete affected language/custom-dispatch/template/optional/Vue suites. |
| consumers-final.json | 975 passed, 2 existing skipped; binding, application/options/analyzer/parameter/extension/HTTP consumers and both regression modules. |
| outputs-final.json | 64 passed; output/comment/smoke/CSV/output-file consumers. |
| fixture-capture.json | 476 passed and captured with actual wrappers, observed public records and imported module provenance. |

Each receipt retains its literal command, resolved test files, terminal status and raw JSON-escaped stdout/stderr. The final capture comprises the original 200 plus 168 contextual-call cases, 36 genuine-assertion cases, 54 runtime-operator/type cases and 18 Box/import/query assertion/anonymous-return controls. The original 200 captured sources, wrappers and public records compare exactly equal to `before-cases.json`.

The new cohesive `testTypeScriptAssertionRoles.py` asserts actual callback names, formal `arg`, CCN2, spans and successors, with enclosing statement metrics where relevant. Genuine assertions retain only `run` and `after`. Anonymous return controls assert formal `arg`, exercising context-None lookahead rather than merely observing an empty callback.

`keyword-owner-after.json` reuses root's exact 31 analyzed sources and wrappers. All 14 repaired operand rows equal their isolated baseline records, including token/NLOC metrics; the 14 property/member and three genuine assertion controls remain identical to the accepted candidate records.

`operators-before.json` and `operators-baseline.json` retain actual token shapes and all 36 directed family observations. The baseline passes all 36. The pre-correction candidate loses `instanceof`/`in`/XOR callbacks and the six `<<` callbacks; `>>`/`>>>` already pass after relational return. Both `number` and `Map<string, number[]>` left-shift cases now pass across TS/TSX/Vue. `operator-official-parser.json` retains parser validity.

`nested-generic-before.json`, `reference-candidate2.json`, and `reference-after.json` preserve genuine `Box<<T>(x:T)=>T>`, `import("m")<(n:number)=>number>` and `typeof import("m").factory<(n:number)=>number>` behavior. All 18 exact isolated candidate2 pre/current post observations match, in assertion and anonymous-return forms. `reference-official-parser.json` records empty TS/TSX parse diagnostics for those exact forms. Import capability is supported by these actual public observations, not only a code claim.

`calls-correspondence.json` checks the existing frozen 40-call corpus once through default and NS public analysis. All 80 records and source hashes remain identical to the accepted cost return, including parameters, spans, metrics and NS nesting. It records no timing verdict and mutates no workload.

## Bytes, consumers and handoff

`manifest.json` contains all before/after runtime/test hashes and all 11 current story product/test paths. Existing tests are byte-identical; only the focused assertion-role test module is added. Current changed hashes are:

| File | SHA256 | Lines |
| --- | --- | ---: |
| typescript_declarations.py | c7aee49c5865a5e9f9f2111053b131cfbe51c9097b7ab975cffa747b706384c6 | 172 |
| typescript_states.py | 2ed69bc5b4f48696960979e6776f8e019ee1ae74496068f673491e7224fd7630 | 250 |
| typescript_type_annotation.py | 2b9d4475babe82c63653fe8d542300e173a8536317b641ed3cc9f23811f60ac2 | 97 |
| testTypeScriptAssertionRoles.py | 154509d554eb3961134dae287e35693d678d067861abf86de6bfee29ed95f4a8 | 76 |

All story product/test files remain at most 250 lines. `source-edges.json` dispositions six changed ownership boundaries with actual setup/assertion receipts. `consumer-correspondence.json` reuses the accepted 719-consumer record and maps current language/parameter/extension/application/output proof to the reached contracts. No broad consumer rediscovery occurred; no consumer was excluded merely because its file was unchanged.

All directed observed G14–G16 gaps in this return are repaired; no confirmed left-shift failure is deferred. Root owns fresh independent aggregate refactoring, full pytest/make, the newly frozen expanded input matrix, all affected performance qualification and eventual external delivery. Earlier candidate2 numeric results must not be claimed on these new bytes.

`git diff --check` terminated 0. No owned command remains running. Foreground Nix ownership was explicitly released after the final short provenance/manifest command; this narrative alone was finished afterward through `apply_patch`. No refactor-completion declaration, timing, formatter/hook lint, CI/observer action, commit, push, GitHub, PR or issue action was performed by this implementer.
