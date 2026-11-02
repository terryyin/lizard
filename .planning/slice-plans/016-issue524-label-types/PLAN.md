# Resolve issue #524 label and annotation boundaries

**Identity:** SEED-024#resolve-issue524-label-types
**Source:** [Resolve label and annotation boundaries](../../seeds/SEED-024-issue524-label-types.md#resolve-issue524-label-types)
**Source review:** [Source-only commitments](source-review.md)
**Repair PR:** https://github.com/terryyin/lizard/pull/548 — delivery artifact of this existing canonical owner, not a second work item.

## Goal and scope

Restore labeled runtime blocks and correct type-annotation ownership, delimiter
termination and replay without losing existing metrics or signatures. Qualify
the correction below 3%, merge its exact qualified head and close issue #524
with a related-fix summary after all required checks pass. This is the human's
authorized planned repair, qualification and delivery; no quick-repair time
limit or historic Fabio correction cutoff was selected. Replanning within the
same outcome is authorized; no implementation expansion to unrelated readers.

## Execution identity and authority

- Mode: Story Branch. Work created this owned worktree:
  `/Users/terryyin/git/lizard/.worktrees/resolve-issue524-label-types`, branch
  `codex/resolve-issue524-label-types`. Integration/default checkout:
  `/Users/terryyin/git/lizard`; remote `origin`, trunk `master`.
- Admission `277ed48a03c3bcc4533d2e25a06403aa744a77d2` is published on trunk;
  agent Hitomi-chan, publisher `issue524-label-types-20261102`, created:true.
  Frozen starting master/runtime baseline:
  `da29d5dfd904ba7333199a79ffbf4c0da24c5641`.
- User authorizes code/tests/refactor, publication, creation and merge of the
  repair PR, and one issue summary plus completed closure. No force-push.
- Fresh checkout Nix setup and the existing binding-boundary suite succeeded:
  217 passed. Preserve the existing pip environment warning as environment
  context; it did not prevent the applicable command. No hook or selective
  formatter is configured; make supplies check-only style checks.

## Decisive observations and existing owners

- Minimal label `const x = [() => { a: 1 }];` yields no function on the frozen
  current product, while a verified 1.24.1 export yields an anonymous line-1
  function. A following function survives, so its presence alone is weak proof.
- A labeled nested brace with decisions already preserves callback CCN/span;
  retain it as a discriminating control, not evidence that all labels work.
- `class A { private d: (() => void)[] = []; constructor() { x(); } s() {} }`
  currently reports a phantom runtime function `d`; its constructor/method
  already survive. The untyped-field control reports only the actual methods.
- `const t = () => x as { b: string };` followed by `g` already retains both
  functions and spans. Array, function-type and generic-type formals/returns
  also currently pass. Preserve these before/after controls and extend them
  where the actual changed annotation boundary reaches their purpose.
- Root inspected these observations through analyze_source_code in the owned
  checkout before planning, across TS/TSX/Vue and JS labels. Raw planning probe:
  `/tmp/issue524-planning-0ej8qhct/baseline.json`; implementation must retain
  current red/green public regression proof in its own return.
- PFE: TypeScriptStates owns runtime dispatch; JavaScriptObjectsMixin owns
  member syntax; TypeScriptDeclarationsMixin/TypeScriptTypeAnnotationStates
  own excluded type syntax. Return-type lookahead already reuses that type
  reader with context=None. CodeStateMachine owns nested-state/replay and
  bracket helpers. Change these semantic owners coherently; preserve replay
  without double-feeding binding tracking. No new parallel runtime counter,
  whole-stream buffering, per-token wrapper or issue-specific switch.
- The member reader currently shares as_object for object/class syntax, and
  the runtime fallback treats otherwise-unclaimed colons as annotations. Do
  not choose label/type roles merely from the next example token. Inspect
  declaration, binding, object, block and signature callers together.
- This is one JS/TS-reader subsystem, with no architectural boundary change.
  ADR-0001, `docs/adrs/0001-ubiquitous-language-accepted.md`, preserves public
  function/CCN/NLOC/token/parameter meanings; no exception is proposed.

## Verification and performance contract

Use public analyze_file/FileAnalyzer, CLI and HTTP observations, without mocked
parsing. Extend cohesive existing coverage before adding another equivalent
test. Include label continuation/successors, nested type brackets/commas/arrows,
real versus type-only fields, return annotations, assertions, defaults, ternary,
templates, JS/TS/JSX/TSX/MJS/CJS/Vue and NS/nonstrict. Trace all actual changed
consumers with the installed consumer-trace tool and disposition every match.
The complete affected JS/TS/Vue suites and discovered output/extension/application
consumers are local gates. AGENTS.md requires full pytest and make before final
acceptance. Discover actual hosted matrix and conditional-job applicability at
publication; local green does not substitute for hosted checks.

Freeze complete prior #524 cases plus new affected syntax and nearby controls
before timing. Preserve the old 171-case and 40-call corpora unchanged when
reused. Build separate label, annotation and call-argument consumer families so
an aggregate pass cannot conceal an affected failure. Qualify each family and
the complete set through API+public parameter and NS consumption, plus the
frozen broad 100-source CLI. Compare against frozen current master da29d5d;
also preserve cumulative issue-fix qualification against pre-#525 product
`6d288d4cdcb85bc50ab4c8a887196177607eeb7d`. The new maintainer PR has no additional
contributor-parent correction baseline; 1.24.1 is semantic reference only.

Use installed paired_benchmark.py: twenty alternating warmed pairs, API/NS
calibration1.2s, broad CLI4s, every batch >=1s, strict paired95% upper slowdown
<3% for every applicable comparison. Record all runtime/corpus/harness hashes,
actual interpreter/options/imports, observation stability, durations and host
conditions. Never mutate a failed workload or discard an unfavorable run.
Profile a valid failing candidate before correcting it; profiling is not a
timing verdict. Requalify only proofs whose bytes/inputs/consumers changed.

Prospective precision extension after performance2: current calls/NS mean
+2.60696% has upper+3.29432%. Exact NS profiles show unchanged extension call
counts and no new extension hotspot; the mean is below the limit but that
observation does not clear the bound. Add one fixed block of80 alternating
four-second pairs, retaining the original20 in a pooled100 evaluation using
the installed conservative t(19) method. Both the new80 and pooled100 bounds
must be strictly<3%; either failure returns to correction, with no further
unchanged-candidate precision retry. Keep all four accepted default gates on
matching candidate2 bytes, then finish every remaining20-pair gate. No corpus,
consumer, threshold or product change is authorized by this measurement choice.

Coordinator owns paired timing and full local checks on Territory.local. All
Nix entries (including reads) and heavy proof are serial; the starter owns each
command through terminal status. Inspect current load/memory before timing and
leave unrelated host work alone. Delegates run focused checks only and transfer
foreground Nix ownership explicitly; no concurrent worktree timing.

## CI observation

Delivery replacement: the initial8-hour stream reached terminal finished with
no validated code publication; result.json and PID absence were inspected.
It was not restarted. A fresh delivery-phase coordinator
issue524-label-types-delivery owns /tmp/dough-ci-501/watch-5DYueW, PID52028,
session29154, yielded cell g3:201, same exact checkout/remote execution branch.
Managed delivery must use this current coordinator and directory. No original
code publication is claimed observed by the expired stream.

Source: GitHub Actions python-app.yml, verified active workflow38306292,
display name Python application. Owned execution-branch observer coordinator
`issue524-label-types`, mailbox `/tmp/dough-ci-501/watch-p6uGJy`, PID39731,
session89946, yielded cell g3:17, rooted in this exact execution checkout.
Reuse for managed execution delivery. Admission/preparation trunk publications
have unobserved trunk coverage; do not register them on this branch observer.
Normal completion owns wait plus shutdown; final trunk integration needs its
own correctly targeted observation and receipt before resource retirement.

## Ordered slices

### 1. Restore label/type ownership and qualify the complete correction
Type: Behavior
Status: done
Behavior: Valid runtime labels retain their functions, continued decisions and
successors. Type-only declarations/signatures do not create runtime functions,
and annotations return enclosing delimiters exactly once while preserving
legitimate nested types. Keep all earlier #524 fixes and shared consumer metrics.
Proof: Implementer first extends public regressions and demonstrates the actual
red mismatch before edits. Cover minimal/continued labels, annotation delimiter
families, class phantom versus actual field functions, and genuine typed and
assertion controls. Run complete affected suites and traced consumers. Return
uncommitted implementation with literal terminal commands, source setup and
observing assertions, hashes, complete consumer dispositions and all gaps.
Fresh independent aggregate refactor precedes coordinator acceptance. Coordinator
runs full pytest/make and the frozen serial qualification matrix on matching
bytes. A failed semantic or performance promise returns here for diagnosis and
correction; no unresolved workload can move to the hosted-delivery slice.

### 2. Merge the qualified repair and close issue #524 with evidence
Type: Behavior
Status: done
Behavior: Publish the qualified repair PR against current master, inspect its
aggregate diff and fresh discussion/checks, preserve accepted runtime/test bytes,
and merge guarded by its exact head after applicable hosted CI passes. Confirm
the merged master, recheck every reported #524 case and new regression boundary,
then post one related-fix summary and close #524 as completed. Keep the issue
open and report any remaining failure instead of claiming partial resolution.
Proof: Root performs these external administrative actions; no redundant coding
agent is needed. Prerequisites are accepted slice1 regressions/refactor/full
checks/all<3% comparisons, matching head/base/tree, current workflow applicability
and exact-head checks. Retain merge/head/base receipts, check identities/results,
merged-state confirmation, issue comment URL and closed/completed confirmation.
Then finish ordinary retrospective/wrap-up with recoverable raw evidence and a
concise maintained disposition; observe final trunk closure before retirement.

## Concern evidence

Refinement not needed: two cohesive slices separate a fully qualified parser
result from external PR/issue delivery. Field/assertion eligibility is settled
by the explicit observations above; no human-owned product choice is unresolved.
Implementation design and measured runtime cost remain owned work, not assumed
success. No remaining slice-specific preparation concern or blocking dependency.

## Story obligations

### G1. Function-type assertions create phantom runtime functions
Reported: slice 1 — "object assertion `x as {b:(n:number)=>string}` creates phantom b; generic assertion function types create anonymous phantoms (TS/TSX)"
Story clause: "Investigate the related function-type field and object-type assertion cases, and correct confirmed failures in this concept."
Disposition: proved by slice 1: testTypeScriptAnnotationBoundaries.py assertion containers observe exact runtime-only lists at array/comma/brace boundaries; implementation red.json and current evidence/local2 full pytest/make; independently reviewed saved-token replay in typescript_type_annotation.py and declarations callback.

### G3. Assertion type reader must retain a ternary separator
Reported: slice 1 — "new assertion reader swallowing a ternary separator"
Story clause: "Preserve valid type brackets, signatures, defaults, ternaries, templates, extension metrics, CLI output and all previously fixed #524 examples."
Disposition: proved by slice 1: runtime ternary controls and conditional handoff observe both actual branches, names, formals, spans and CCN; current evidence/local2 and implementation conditional-handoff-red.json preserve the mismatch and correction.

### G4. Preserve existing JavaScript colon-member runtime functions
Reported: slice 1 — "testES6.py::test_class_with_prop_as_function expects legacy class `g:(x,y)=>x+1` and `h:function(){}` as runtime functions, while TS class colon must own type syntax to remove confirmed field phantoms"
Story clause: "Correct label-versus-type-annotation ownership and annotation termination/replay at enclosing runtime delimiters."
Disposition: proved by slice 1: seven-suffix reader/container dialect setup, legacy JavaScript class functions and actual Vue JS/default/TS script-mode assertions; dialect-red.json retained, current evidence/local2 passes; reader-owned class_field_annotations controls the same member owner.

### G2. Tuple return annotations lose boundaries or formals
Reported: slice 1 — "tuple return types fail boundary/formal preservation"
Story clause: "Preserve valid type brackets, signatures, defaults, ternaries, templates, extension metrics, CLI output and all previously fixed #524 examples."
Disposition: proved by slice 1: nested annotation parameter/return cases observe x/y, default CCN, real arrow and successor boundaries; tuple/function/generic/template types share the type bracket owner and current evidence/local2 passes.

### G5. A generic constraint must not consume the class-body role
Reported: slice 1 — "generic constraint `{x:number}` spends pending class role in TS, producing phantomd; TSX additionally drops after"
Story clause: "Function-type class fields preserve following constructors/methods, and object-type assertions preserve expression-arrow and successor boundaries wherever these cases exercise the same ownership responsibility."
Disposition: proved by slice 1: exact generic object constraint regression retains constructor, s and after with formals/CCN; generic header and aborted JSX brace ownership inspected; owner-regressions-red.json retained, current evidence/local2 passes across TS/TSX/Vue.

### G6. Negative literal return annotations must preserve anonymous formals
Reported: slice 1 — "Negative literal return annotation loses formalx in TS/TSX/Vue (anon stillfound, params=[])"
Story clause: "Preserve valid type brackets, signatures, defaults, ternaries, templates, extension metrics, CLI output and all previously fixed #524 examples."
Disposition: proved by slice 1: anonymous literal return regression observes formal x and after across TS/TSX/Vue, including context=None signature lookahead; owner-regressions-red.json retained, current evidence/local2 passes.

### G7. Semicolonless typed fields must retain following constructors
Reported: slice 1 — "semicolonless typed-field cases losing constructor after type (`d: (()=>void)[]` / `Map<...>` newline)"
Story clause: "Function-type class fields preserve following constructors/methods, and object-type assertions preserve expression-arrow and successor boundaries wherever these cases exercise the same ownership responsibility."
Disposition: proved by slice 1: actual newline-separated array/Map type fields retain constructor, s and after with names/formals/spans/CCN; type newline handoff inspected; semicolonless-red.json retained, current evidence/local2 passes.

### G8. A continued typed declarator must keep its runtime function name
Reported: slice 1 — "introduced multiline-only name regression: candidateanon instead of run; same-line candidate and both frozen da29 controls report run"
Story clause: "Preserve valid type brackets, signatures, defaults, ternaries, templates, extension metrics, CLI output and all previously fixed #524 examples."
Disposition: proved by slice 1: same-line and continued typed declarators retain run/n/CCN2 and after; comma-owned declaration role survives newline cleanup; declaration-baseline.json and declaration-red.json retained, current evidence/local2 passes.

### G9. A completed conditional type must return a runtime ternary
Reported: slice 1 — "root acceptance: unparenthesized `x as T extends U ? V : W ? function yes(a) {return a;} : function no(b) {return b;}` omits yes; the parenthesized assertion control retains yes, no and after"
Story clause: "Preserve valid type brackets, signatures, defaults, ternaries, templates, extension metrics, CLI output and all previously fixed #524 examples."
Disposition: proved by slice 1: exact parenthesized/unparenthesized compiler-valid controls return yes/a, no/b and after/c at their actual spans/CCN; conditional question role resets after its owned question; conditional-handoff-red.json retained, current evidence/local2 passes.

### G10. Optional field types must not depend on compact identifier spelling
Reported: slice 1 — "root acceptance: `d ? : (()=>void)[];` and `$d?: (()=>void)[];` report a phantom anonymous function; `d ? : (n:number)=>number = (n)=>n||0` loses the runtime field name d, while compact d?: control works"
Story clause: "Investigate the related function-type field and object-type assertion cases, and correct confirmed failures in this concept."
Disposition: proved by slice 1: compact/spaced/$ optional fields produce no phantoms and actual initializers retain d/$d and n; outer CCN3 and complex_tags observe exactly the two runtime decisions; existing is_optional protocol and initializer ownership inspected, both red receipts retained, current evidence/local2 passes.

### G11. Template type substitutions must retain their type-owned braces
Reported: slice 1 — "root-directed exact owner probe confirmed split TS/Vue template-type substitutions lose successor or anonymous formal x; atomic TSX controls already pass"
Story clause: "Preserve valid type brackets, signatures, defaults, ternaries, templates, extension metrics, CLI output and all previously fixed #524 examples."
Disposition: proved by slice 1: template-type assertion array/comma/runtime-brace, formal/named return and anonymous return cases retain successor and x; split TS/Vue and atomic TSX controls inspected; existing type stack owns ${; template-types-red.json retained, current evidence/local2 passes all 200 cases.

### G12. Existing call-argument consumers exceed the performance bound
Reported: slice 1 — "coordinator performance1 current-calls-api-parameters: valid 20-pair mean +4.7751675467%, paired 95% upper +6.0373328721%; labels and annotations passed first"
Story clause: "Independently refactor the aggregate change, qualify affected API/parameter/extension consumers and broad CLI below a strict paired 95% upper slowdown bound of 3%"
Disposition: proved by slice 1: exact-frozen-byte profiling preceded the three-owner dispatch correction; independent refactor-cost found no further edit, local3 full pytest/make pass; performance2/current-calls-api-parameters.json upper+1.0275387045% on matching candidate2 bytes, same immutable40 sources; original failed candidate and raw pairs retained.

### G13. NS call-argument uncertainty still crosses the performance bound
Reported: slice 1 — "coordinator performance2 current-calls-ns: valid20-pair mean+2.6069557267%, paired95% upper+3.2943187631%; all four current default gates passed on candidate2"
Story clause: "qualify affected API/parameter/extension consumers and broad CLI below a strict paired 95% upper slowdown bound of 3%"
Disposition: proved by slice 1: prospectively selected one80-pair four-second extension after exact NS profiling, with no product/input change; performance3/current-calls-ns-extra.json independently passes and current-calls-ns-pooled.json includes every original20+new80 pair and has strict upper+1.8250282351%; official evaluator/provenance/stability guards inspected, no unfavorable pair discarded.

### G14. Contextual type words in runtime operand position must retain callbacks
Reported: slice 1 — "root final owner probe: `flag ? as(() => 1) : value` and `flag || satisfies(() => 1)` lose their anonymous callback on candidate2 in all7 suffixes; verified da29 retains it. Dot/optional-property controls remain green through the existing field owner; genuine type-assertion control removes only the old type phantom."
Story clause: "Correct label-versus-type-annotation ownership and annotation termination/replay at enclosing runtime delimiters."
Disposition: proved by slice 1: testTypeScriptAssertionRoles.py actual operand/statement/newline call contexts observe anonymous arg/CCN2 and exact outer/successor metrics; implementation-assertion-role red.json records170 failures before correction, current languages-after-export.json and490-case capture pass; root31 owner rows preserve property/genuine controls and restore14 baseline callbacks.

### G15. Completed assertions must return unambiguous runtime operators
Reported: slice 1 — "implementation's directed official-parser/baseline/token probe confirms root instanceof/in/^ lose nested callbacks in18 rows; greater/greater-equal/less-equal also required runtime handoff. Shift-right controls pass as separate greater-than tokens."
Story clause: "Correct label-versus-type-annotation ownership and annotation termination/replay at enclosing runtime delimiters."
Disposition: proved by slice 1: 54 actual operator/type/suffix cases in testTypeScriptAssertionRoles.py retain nested callback arg/CCN2, run/CCN1 and after/tail/CCN2; root type stack owns nested syntax while END_TOKENS returns runtime operators; operator-red.json and official-parser/baseline/token receipts retained, current490 capture and focused suites pass.

### G16. A confirmed runtime left shift must not become generic type nesting
Reported: slice 1 — "implementation's directed family probe: official TS5.7.3 accepts all36 operator/type/suffix controls and da29 retains their callbacks; current role-fixed candidate loses callbacks in6 left-shift rows because << is two '<' tokens entering the type generic stack"
Story clause: "Correct label-versus-type-annotation ownership and annotation termination/replay at enclosing runtime delimiters."
Disposition: proved by slice 1: all6 << callback losses corrected using positive closed-type capability in the existing annotation owner; Vue atomic Map group and split TS/TSX controls pass;18 exact pre/post Box/import/query assertion/anonymous-return observations preserve generic type arguments and formal arg with0 parser diagnostics; all6-red/1-Vue-red attempts retained, current490 proof passes.

### G17. Export-default expression starts must retain contextual identifier calls
Reported: slice 1 — "root inspection of the changed prefix predicate found default absent from the reused regex/operand sets; actual export-default-probe baseline/candidate observations confirm export default as((arg)=>arg||0) and satisfies calls lose their callback in all14 word/suffix combinations while da29 retains it"
Story clause: "Correct label-versus-type-annotation ownership and annotation termination/replay at enclosing runtime delimiters."
Disposition: proved by slice 1: export-red.json14 failures/476 passes becomes export-green.json490 passes after local assertion-prefix default classification; exact14 root baseline/export observations now match; prior476 captures and frozen40/80 public records and dispatch counts unchanged; current declarations e91daa6499973945a5242f502abfe07c7a3ac0ed5aaeebe397c0fb69f46d7042 inspected without regex/ASI mutation.

### G18. Current assertion-role candidate needs a sufficient NS call bound
Reported: slice 1 — "coordinator performance4 current-calls-ns:40 valid pairs mean+2.2558999384%, paired95% upper+3.1432662764%; all four current default cells passed on candidate3"
Story clause: "qualify affected API/parameter/extension consumers and broad CLI below a strict paired 95% upper slowdown bound of 3%"
Disposition: proved by slice 1: performance5 current-calls-ns-pooled.json retains original40 plus new80 pairs and passes with upper+0.8488168262%; extra80 independently passes, cross-block observations match; summary.json contains all18 accepted current/cumulative gates, maximum upper+2.7992221230%; current108 runtime hashes match local5 full checks and independent refactor audit.

## Acceptance progress

Slice2 external outcome: PR548 exact qualified head7fac94f8c5df9c1791b5ef3cf4899847dcaafee1
passed31 checks with one legitimate conditional web skip across32 exact-head
results; base0e352b7751fc8838dcde8b68051e305a200ebb9c unchanged, discussion empty.
Guarded hosted merge succeeded at2416c37e91563965f5c98d00770c6e0083b3bb68.
Fetched master matches this merge; runtime/test trees match the qualified head.
An isolated master export's638-source default/NS and100-source broad CLI public
observations exactly match accepted proof, with actual module-origin guards.
Issue524 summary6569296813 is confirmed and issue state is closed/completed.
No known reported/confirmed failure remains; ordinary completion/retrospective/
Git recovery cleanup/integration/CI shutdown and owned retirement remain.

Slice1 final acceptance: all18 performance5 summary cells pass on current
candidate3 bytes; maximum paired95% upper+2.7992221230%, strictly below3.
Current calls/NS pooled120 upper+0.8488168262% includes every original40 and
new80 pair; additional80 independently passes and cross-block public hashes
match. No old candidate timing substitutes for these final cells. Full local5
pytest/make both pass2917 tests with7 existing skips. Fresh independent
refactor-assertion-role returned REFACTOR COMPLETE with no edits; source,
observations, consumers and all108 current production hashes were inspected.
All known G1–G18 gaps are proved. Hosted checks, guarded merge, merged-state
verification, summary/issue closure and ordinary wrap-up remain slice2.

Current boundary after G14–G17: implementation-assertion-role/manifest.json
c5640bfc84aed773154e0a74b1e8ed4d7e94b1a9bb47721bc6c34d3da55c3ae3
matches all108 live runtime hashes and11 story files. Current actual capture:
cases-after-export.json490; original200 and prior476 sources/wrappers/records
are unchanged. Current focused proofs:1387 language/5 existing skips,
989 consumers/2 skips,64 outputs. Root inspected the complete three-owner
correction, export supplement and actual77-line new test assertions, six
direct source edges and exact metadata. A fresh independent aggregate refactor
is active. Full local checks and all18 performance comparisons must run on
these changed bytes before acceptance; candidate2's completed matrix, worst
upper+2.386236851%, is retained as superseded evidence, not final clearance.

For the newly frozen assertion-role candidate, choose40 alternating pairs
prospectively for calls/default and calls/NS against both baselines,20 for
other families. This increases precision on the previously borderline consumer
without changing threshold, input selection or calibration (API1.2s/CLI4s,
minimum1s). Freeze all490 current actual fixture contexts before timing; retain
unchanged original35 labels,40 calls and100 broad inputs. No timing has started
on these current bytes.

Current full local5 checks both exit0:2917 passed,7 existing skips. Local4's
collection failure came from an evidence-only test snapshot; it is now stored
as unchanged .py.txt data. Four raw patches are losslessly JSON-serialized to
preserve their significant context whitespace for Git. The correspondence
index records original paths, decoded SHA256 and replacement representations;
all product/test hashes and logical evidence bytes remain unchanged.

The first code delivery stages product/tests, current plan/source observations
and a concise qualification summary. Detailed producer/benchmark/failed-attempt
working evidence remains owned and inspectable locally through hosted delivery
and issue closure. Ordinary wrap-up commits that complete raw evidence before
cleanup, then preserves it through Git recovery while publishing the clean
closure snapshot. This keeps the repair PR's current diff reviewable without
discarding any acceptance input or failed observation.

Performance4's current default gates pass (calls upper+2.79923%, labels-1.62823%,
annotations+0.25498%, complete+1.41799%); calls/NS40 mean+2.25590%, upper+3.14327%
does not pass. Root profiled exact candidate3/NS bytes before the next decision:
extension/state call counts are unchanged and no new extension hotspot appears;
profile elapsed times are diagnosis, not a gate. Prospectively add exactly80
four-second alternating pairs, keeping all original40 in pooled120 using the
official conservative evaluator. Require both extra80 and pooled120 upper<3%,
matching cross-block public observations, source/corpus/harness/interpreter and
stability. Either failure stops for correction with no further unchanged-byte
precision retry. Keep all four accepted defaults, then finish every remaining
comparison on the same frozen638 complete/35 labels/432 annotations/40 calls/
100 broad sources; cumulative call cells remain40 pairs, others20. All prior
reports and raw pairs remain. No product change follows from this choice.

The earlier execution instruction to avoid speculative ambiguous-angle grammar
work is not a human story exclusion. G16 is a confirmed introduced runtime
loss, returned to this same owner. Correct the bounded known handoff while
preserving genuine nested generic/function types before dependent acceptance.

Root inspected actual changed runtime owners, public fixture construction and
observing assertions, all ten implementation hashes and eight changed source
edges. Fresh independent aggregate refactor completed after the final 200-case
return; direct type-owner imports preserve historic aliases and the accepted
unreachable-fallback removal. Its report and current focused proof are retained
under docs/reviews/issue524-label-types-20261102/refactor-final. Root corrected
one E128 continuation space; candidate1.json proves the exact prior hash and
identical AST. Current evidence/local2: full pytest and make both exit0 with
2627 passed,7 existing skips; pylint diagnostics remain visible, not represented
as zero warnings. Earlier failed make/local1 and all semantic failures remain.

Before timing, 200 actual captured test contexts became 177 distinct same-suffix
source bytes, retaining every entry/extension provenance: labels35,
annotations142. Unchanged prior calls40, complete171 and broad100 are verified
against their original manifests. Frozen complete348 and all current/cumulative/
candidate default/NS and broad CLI preflights pass. No old-baseline exclusion
was needed. Strict 18-cell qualification is running serially on candidate1's
108 runtime-file hashes. Performance1 labels upper+0.5579% and annotations
upper-0.3364% passed, but existing calls upper+6.0373% did not meet the bound.
The runner stopped; all exact inputs and failed raw pairs are retained. Root
profiles those exact bytes before returning G12 for cost correction; no timing
result accepts the candidate, and source-review performance observations and
slice1 delivery remain incomplete until every applicable result passes.
