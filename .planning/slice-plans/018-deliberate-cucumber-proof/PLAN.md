# Deliberately retain successful website Cucumber proof

**Identity:** SEED-028#deliberate-cucumber-proof
**Source:** [selected story](../../seeds/SEED-028-deliberate-cucumber-proof.md#deliberate-cucumber-proof)
**Source review:** [independent source-only commitments](source-review.md)

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
- Run one heavy local workload at a time. After focused proof settles, run
  required broad checks once; repeat only invalidated proof. Python make may
  run while read-only trace/review finishes because this JS-only change does
  not alter its inputs; any refactor-focused rerun waits for make to finish.
  Reconsider if refactoring reaches a broader boundary. Do not run a suite
  solely to generate proof.

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
Status: done
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

- Admission accepted as 87231fda3cad0ec3c8b59dbb3c82163fb9bafe9f on origin/master
  and codex/deliberate-cucumber-proof, starting at installation 210ae363.
  Agent Honoka-chan; created /private/tmp/lizard-deliberate-proof. Normal workflow
  guidance is unchanged from the previous adoption (Git diff of installed
  execution/land/planning/backlog/wrap-up guidance is empty).
- `nix develop -c npm --prefix website/frontend ci` succeeded in this checkout.
  `test:e2e:dry -- --profile default` succeeded: 16 scenarios/134 steps and
  2 hooks skipped, no positional-path diagnostic. This verifies actual selection
  for the configured explicit root command without generating proof.
- Exact installation CI succeeded: 210ae3633086b989bea7fa4170268d6db3f4c711,
  Python application run 41420535783 attempt 1. Native provider evidence is
  separate from retained proof and imposes no producer comparison.
- Branch observer: codex-lizard-deliberate-20261102,
  /tmp/dough-ci-501/watch-SXzfwE, PID 36267, cell g93:13, session 77461.
  Target codex/deliberate-cucumber-proof, runtime from this owned checkout.
- Trunk observer (installation, admission and final integration):
  codex-lizard-deliberate-master-20261102, /tmp/dough-ci-501/watch-slR2cc,
  PID 53292, cell g93:14, session 90020. Target master; installation 210ae363
  and admission 87231fda registered. Both observers select python-app.yml /
  Python application, workflow 38306292. Preserve exact identities across delivery.
- Admission 87231fda exact trunk CI also succeeded: run41422328395 attempt1.
- Snapshot reduction keeps the reader's first retained pass/pass tie, but the
  incoming final pass replaces equal-time pass metadata. Equal-time legacy
  failure still wins. G1 records this discovered edge and its deterministic
  real-timestamp filesystem-fixture proof; no message/parsing rewrite was used.

## Accepted implementation proof

- `nix develop -c npm --prefix website/frontend run test:proof`: 24 real-runner
  tests passed, 7.480s. Concrete setup/assertions are in proof-plugin.test.js,
  proof-refresh.test.js and cucumber-runner.js. All 96 source commitments were
  independently mapped to inspected observing boundaries; G1 is proved.
- Three separate invocations of
  `nix develop -c node --test --test-name-pattern='real final timestamp|live checkout lock|concurrent pass and cleanup' website/frontend/test/proof-refresh.test.js`:
  exactly 3 selected tests passed each time, 9 selected executions total.
- Locked `npm ci`, browser installation and ordinary Vite build passed in this
  checkout. No shared mutable installation was copied from another checkout.
- `nix develop -c node /private/tmp/lizard-deliberate-browser-check.mjs normal`:
  real ordinary default profile passed16 scenarios/134 steps/2 hooks in25.776s;
  controller compared proof bytes and file paths before/after, all unchanged.
- `nix develop -c node /private/tmp/lizard-deliberate-browser-check.mjs record`:
  same runner/default profile with explicit option and language-name filter
  passed1 scenario/10 steps/2 hooks in0.513s. Two historical run files became
  latest.json, one identity refreshed, all15 unexecuted observations unchanged,
  HEAD unchanged. No full run was launched solely to generate proof.
- Actual 0.3.82 dashboard parseProofFile/latestSuccessfulObservations accepted
  the new16-success snapshot and historical input; primary comparison confirmed
  exactly1 refreshed identity and15 historical observations preserved byte-for-
  byte as objects. This validates proof data, independently of provider verdicts.
- `nix develop -c make`: terminal exit0; 2917 passed/7 skipped in17.07s under
  coverage, total94%; pycodestyle passed and advisory pylint score9.10.
  Python inputs were unchanged, so the supplied normal pytest baseline remains
  applicable. Log: /private/tmp/lizard-deliberate-make.log, not committed.
- Consumer trace /private/tmp/lizard-deliberate-proof-consumer-trace.md has131
  complete dispositions, including shared npm/CI/release callers. Application,
  deployment, Python selectors and provider configuration are unchanged.
- Implementation is quiescent. Independent refactoring remains before delivery;
  only proof it invalidates needs repetition.
- Independent refactor completed: shared Git object-ID regex in proof-snapshot.js,
  renamed obsolete interruption-test wording, simplified documentation wording.
  No API/schema/selection/locking/persistence boundary changed. It ran
  `nix develop -c npm --prefix website/frontend run test:proof` unfiltered:
  24 passed, 0 failed/skipped, 7.460s; G1, live-lock reload, concurrency and
  interruption assertions all selected. Browser, snapshot adoption, build and
  make proof remain applicable. ## REFACTOR COMPLETE received; no gaps.
  Trace: /private/tmp/lizard-deliberate-proof-refactor-consumer-trace.md,
  15 consumers disposed. Whitespace including untracked sources passed.


## Execution complete

Product advice: no change. This maintenance outcome follows the backlog's
maintenance, bug-fixing and PR-review direction; no dependent product work is
supported by the delivered behavior.

Retrospective: reviewed admission 87231fda (provenance) and published implementation
7e69ae7b158b1a9de43a1b8359198344d6ccc44c against the original source, aggregate
producer/configuration/docs/tests and the accepted proof above. Installation
210ae363 is context, not an implementation change. Native Messages aggregation
and the serialized successful-snapshot store have distinct coherent ownership;
normal application, Python, release and CI consumers retain their boundaries.
Real-runner filesystem assertions cover option gating, legacy reduction, retry
finality, timestamp ties, lock contention and interruption; existing browser
journeys retain their integration role. No supported correction, ADR conflict,
obsolete compatibility branch or redundant test removal was identified. Planning
is unchanged. CI remains owned separately until the completion receipt.

Process review is enabled. The retained execution summary, proof/trace records
and current tool history support the accepted checks and bounded reruns; earlier
raw conversation is unavailable after compaction, limiting detailed process
attribution. No new supported process finding was established, so no process-log
write is made. This limitation does not invalidate independently observed product
proof or the completed independent refactor.

## Story obligations

### G1. Incoming final pass loses a pass/pass timestamp tie
Reported: slice 1 — "Coordinator inspection found retainLatest preserved an earlier passed observation when the incoming final pass had the same ISO-millisecond finishedAt, potentially retaining old attempt metadata."
Story clause: "Explicit focused success updates that identity"
Disposition: proved by slice 1: website/frontend/test/proof-refresh.test.js real final timestamp fixture observes incoming pass/pass replacement and failure-tie blocking; unexecuted legacy pass/pass test preserves reader metadata; 24 real-runner tests and three consecutive focused runs passed
Commitment: 34
