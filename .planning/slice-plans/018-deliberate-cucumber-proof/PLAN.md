# Deliberately retain successful website Cucumber proof

**Identity:** SEED-028#deliberate-cucumber-proof
**Source:** [selected story](../../seeds/SEED-028-deliberate-cucumber-proof.md#deliberate-cucumber-proof)

## Goal and scope

Replace continuous proof accumulation with explicit successful-proof refresh in
the existing Cucumber.js producer, retaining normal runner behavior and readable
historical evidence. Follow installed 0.3.82 cucumber-proof.md, verified byte-identical
to release 321ca2bd8c77a00cb2c3b2ef81d1d44bee52892d. Do not edit managed guidance,
add an adapter, release/deploy the application, or compare proof with CI verdicts.

## Context and current decisions

- Origin/default checkout: /Users/terryyin/git/lizard, master at installation
  210ae3633086b989bea7fa4170268d6db3f4c711. Authorized final target: origin/master.
  Story Branch Mode owns /private/tmp/lizard-deliberate-proof on
  codex/deliberate-cucumber-proof, created by the installed admission operation.
- One cohesive Behavior slice covers option gating and snapshot refresh through
  the real runner. No numeric slice limit is supplied; no independent outcome or
  unsettled human decision remains. Locking details are implementation choices,
  not a new framework or architectural boundary.
- PFE: keep website/frontend/proof-plugin.js and its established main-process
  Messages aggregation; existing cucumber.mjs, fixture helper and test:proof own
  the same runner integration. Bounded source search found no existing project
  lock/snapshot store to reuse. Add only the necessary coherent persistence seam.
- Preserve Cucumber 13.3 outline/source, retry/timing and execution-evidence handling.
  Real dry-run undefined results previously required measured invoked step/hook
  time; preserve that discrimination and no-execution behavior.
- Intended option: RECORD_CUCUMBER_PROOF=1. Intended root generation instruction:
  `nix develop -c env RECORD_CUCUMBER_PROOF=1 npm --prefix website/frontend run test:e2e -- --profile default`.
  Verify this selects the existing default feature scope with unchanged output.
- Use stable identity ordering and latest-first legacy reduction, including
  unsuccessful ties. Each concurrent mutation rereads retained proof under an
  exclusive checkout lock; never write a stale in-memory snapshot. Handle orphaned
  locks after interruption safely. Ordinary runs allocate no proof/lock resources.
- ADR 0000 (docs/adrs/0000-use-adrs-accepted.md) retains human architectural ownership;
  ADR 0001 (docs/adrs/0001-ubiquitous-language-accepted.md) retains domain vocabulary.
  This local reversible test-infrastructure change conflicts with neither.
- Carry [Open Dough's failing and flaky test policy]
  (/Users/terryyin/git/open-dough/AGENTS.md#failing-and-flaky-tests) as written,
  plus AGENTS.md, into every handoff. Base attribution is no failure exemption;
  exact actively owned repairs stay with their owner.
- Own installation 210ae363's CI and every published migration revision through
  applicable final validation. Retained snapshots and CI are independent data.
- Supplied full Python baseline: `nix develop -c python -m pytest` on unchanged
  installation candidate: 2917 passed, 7 skipped, 14.36s. Reuse only unaffected
  Python boundaries. Required `nix develop -c make` remains a local gate; the real
  runner and ordinary full browser suite validate this changed test infrastructure.
- Run one heavy local workload at a time. After focused proof settles and the
  independent refactor, run required broad checks once; repeat only invalidated
  proof. Do not run a suite solely to generate proof.

## Outside-in proof

Extend existing real-Cucumber temporary-Git regression fixtures. Observe ordinary
pass/fail no-write against both existing and absent proof (including no locks),
explicit focused pass/failure cleanup across nested legacy files, newer-result
preservation, tie/failure-first reduction, final retries, source identities and
metadata, stable compact snapshots, concurrent mutations and interruption recovery.
Compare normal and explicit output/diagnostics/status at the runner boundary.
Normal optimization/CI never opts in; recording never commits automatically.
Verify the ordinary website suite leaves retained files unchanged. Use one
meaningful explicit focused website run for deliberate adoption/refresh, retaining
all unexecuted successful historical identities, and validate its version-1 data.

## Ordered slices

### 1. Deliberately refresh successful scenario proof
Type: Behavior
Status: planned
Proof: real-runner recorder tests, ordinary full browser run with byte-for-byte
proof preservation, explicit focused refresh/adoption, make and applicable CI.

Behavior: Given retained or missing proof, normal passing and failing runs do
no proof work. With the explicit recording option, final executed scenarios
atomically update a stable successful snapshot, clearing obsolete success on
failure and preserving unexecuted/newer identities. Concurrent/interrupted
generation preserves completed updates and existing runner reports and status.

## Concern review

Refinement not needed: one cohesive user-visible proof contract and runner-level
proof loop. Current producer/API/locked setup and canonical destinations are known;
no remaining slice-specific concern or blocking human decision is identified.

## Learnings

None yet.
