# Resolve issue #524 label and annotation boundaries

**Identity:** SEED-024#resolve-issue524-label-types
**Source:** [Resolve label and annotation boundaries](../../seeds/SEED-024-issue524-label-types.md#resolve-issue524-label-types)
**Source review:** [Source-only commitments](source-review.md)

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

Coordinator owns paired timing and full local checks on Territory.local. All
Nix entries (including reads) and heavy proof are serial; the starter owns each
command through terminal status. Inspect current load/memory before timing and
leave unrelated host work alone. Delegates run focused checks only and transfer
foreground Nix ownership explicitly; no concurrent worktree timing.

## CI observation

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
Status: planned
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
Status: planned
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
