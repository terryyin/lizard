# Independent aggregate refactor return

SEED-024#resolve-issue524-label-types, slice 1. The fresh read-only whole-concept pass found one justified cleanup in the existing JS/TS reader subsystem. No architectural boundary changed. ADR-0001 (`docs/adrs/0001-ubiquitous-language-accepted.md`) retains function/CCN/NLOC/token/parameter vocabulary and meanings; no exception or status mismatch was found.

## Result and scope

Only product edit: `lizard_languages/typescript_signatures.py:19` now uses the annotation owner's saved delimiter directly. Removed the unreachable `saved_token is None` fallback and its obsolete own-bracket termination comment (net eight lines removed; module now 57 lines). Both actual returns from `TypeScriptTypeAnnotationStates` save the runtime delimiter first (`typescript_type_annotation.py:48-49,67-68`). Context-None lookahead instantiates that concrete owner; EOF returns False instead of reporting completion. `CodeStateMachine` has no other completion route. The fallback could therefore never consume another token.

The decision preceded editing. Duplication, domain names, orchestration, speculative/dead code, observable tests and file-size checks were assessed once across declarations, runtime dispatch, binding/replay, signature lookahead, class/member modes, Vue script containers and TSX aborted-candidate tokenization. Type-owned nesting differs from runtime binding frames and remains with the new type owner. Per-reader/container mode selection has one runtime consumer; its genuine dialect differences do not warrant another abstraction. No other candidate justified an edit.

Preserved existing PR525 call-group/binding/replay solution, imported `TypeScriptTypeAnnotationStates` compatibility names, custom-reader hooks/EOF, line conventions and public metrics. No rename, extraction, split or deletion was performed. The original nine manifest product/test hashes still match exactly; the tenth product hash is in `languages-after-refactor.json` and `correspondence.json`. All ten product/test files are <=250 lines (largest 248); lifecycle plans and raw captured artifacts retain their owners.

## Observed proof and consumer correspondence

The exact final implementation language command was rerun after the edit: **1046 passed, 5 existing skipped in 1.63s**, exit0. `languages-after-refactor.json` retains its literal command, identical 35 resolved test files, terminal output, runtime paths and current hashes. It includes all 149 new regressions, all existing binding controls, signature/type/ternary cases, real NS/nonstrict, CSV/-o and HTTP assertions. Setups and actual observed fields are mapped in `correspondence.json`; no test/assertion was weakened or edited.

Installed consumer tracing is retained in `consumer-trace.json` (aggregate uncommitted trace, 322 consumers) and `consumer-trace-signatures.json` (42-consumer targeted follow-up). The follow-up omits aggregate-base discovery to expose the actual TypeScriptReader caller, which the aggregate trace classifies as a changed path. Every unique listed consumer has a run/reused/excluded disposition in `consumer-dispositions.json`, with prior 558-consumer inspection/disposition references retained where applicable. New planning/evidence matches are excluded as records rather than runtime consumers. No uncovered consumer remains for this refactor.

The coordinator-requested optional-class-member check also passed all 9 TS/TSX/Vue public observations. Exact `d?: (()=>void)[]` preserves only constructor/s/after; optional and required typed-field runtime ternary initializers preserve yes/no plus constructor/s/after with exact formals, spans and CCN. `optional-member-probe.py` and `.json` retain literal executed source/command, expected/actual observations and token ownership: `OPTIONAL_NAME` emits `d?` atomically, so the colon reaches typed-member dispatch without a standalone runtime question marker. No behavior edit was needed.

The prior language receipt is refreshed because lookahead implementation bytes changed. Other accepted application/options/analyzer/extension/output/tokenizer/extra-consumer/fixture-capture boundaries retain the same implementation contracts, setups and observations. Their JS/TS syntax portions, binding cases and real output/extension journeys are also exercised by the replacement language command. `correspondence.json` records each accepted receipt's precise reuse basis; it does not relabel historical receipts as new executions.

Whitespace check `git diff --check -- . ':!docs/reviews/**' ':!.planning/**'` exited0. No product generator was affected. No selective formatter, hook lint or full suite was run by this refactor owner.

## Remaining gates and ownership

No newly confirmed semantic gap was found during this bounded refactor. The caller-recorded older anonymous-generic limitation was not newly investigated or represented as fixed. Root owns aggregate metadata refresh for the additional signature path, full pytest/make, all 18 frozen qualification cells with strict paired 95% upper slowdown <3%, exact-head hosted checks, merge and issue summary/closure. Those promises remain open; no performance verdict follows from this refactor.

Retained expensive-proof decision: Territory.local/16 CPUs, exclusive serial foreground Nix ownership; only cheap focused proof ran. Root must reconsider actual host conditions before its expensive proof. Unrelated host work, root-owned plan/source-review/qualification helpers/raw evidence and the branch observer were preserved. No commit, push, stash, reset, cleanup or branch switch occurred.

Approximate active elapsed:7 minutes, including source/proof inspection and evidence writing. Consequential learning: the new annotation completion invariant removes the former need for a second signature-lookahead termination convention.

Foreground Nix ownership is explicitly released after the final evidence command terminates. No owned command or subprocess is running.

## REFACTOR COMPLETE
