# Own executable proof

Map each applicable, checkable final-state promise from the plan's source and
current decisions to an owning slice and observable proof. Use inline links or
a compact table. A passing command must produce the promised observation.
Keep the map through refinement and resume. Before replacement slices are
ready, reassign the promises of the slices they replace, retain still-valid
proof, and align callers, fixtures, and assertions with the final success
and rejection behavior.

Apply [the test boundary rule](../../dough-post-change-refactor/references/refactor-checks.md#choose-the-test-boundary)
when choosing or accepting automated tests. For a new test or changed
boundary, check the layer, production entry point, and need for each
substitute. In a plan, name the typed inputs and collaborators for each
contract variant the unit cases exercise. If setup needs unrelated data or
services, [narrow the production contract](../../dough-post-change-refactor/references/refactor-checks.md#design-production-code-for-testability)
in the plan before accepting its proof design.

Inspect setup and assertions. Setup supplies preconditions. An outcome
supplied by setup is unproved. Retain independent observations. Completion
inside an operation leaves its caller's completion unproved.

Before editing or selecting proof, identify the changed behavior, output,
default, or precondition. Follow it through production, tests, and fixtures,
including unchanged files. Start with the diff, selectors, helpers, and
planned checks. Search for retired messages, statuses, labels, and values,
then inspect the enclosing observations and caller setup.

- For output changes, inspect enclosing surfaces and their aggregate text,
  counts, roles, and keyboard order. Whole-container assertions can observe
  hidden content and text boundaries without naming the changed component.
- For operation or default changes, follow entry points, continuations, and
  recovery paths. Include callers that rely on the old default and cases that
  omit resources a previously conditional operation now needs. A fixture
  that supplies the resource cannot prove those callers still work.
- For shared layout, input composition, or read infrastructure, follow the
  journeys under the conditions that change, such as narrow or zoomed views
  with focus return, exact composed input and keyboard order, or readers with
  caching and refusal contracts.
- For cross-file renames, removals, relocations, or callable-contract changes,
  search old symbols, module paths, and call forms, including private symbols
  used across files. Follow fixtures, stand-ins, and harness helpers that
  invoke or imitate the contract.

Use the [consumer-trace record](#record-the-consumer-trace) to account for
what the effect reaches. If many tests fail on an incidental effect, plan a
preceding Structure slice that narrows their assertions to their promises.
Apply the [failure policy](../../dough-execute-plan/references/failure-policy.md)
to every failure, including known pre-existing failures. Record that
attribution for diagnosis and preserve the failing assertion's promise.

For artifact preservation, use project-supplied identities and scope to name
the installation, physical store, and predecessor where applicable. Surface
target conflicts before dependent work, such as a Docker volume being proposed
for data held in a native installation. Match ordinary continuity proof to the
same store. A transfer needs migration authority and observed transfer proof,
and remains incomplete while deferred.

Obtain any required pre-change baseline before dispatching the change.
Reuse evidence with a known matching revision and relevant environment and
selection. If the original baseline is lost, label the reconstruction and
show comparable revisions and conditions before relying on it. A missing or
failed prerequisite stops its dependent work. Name the gap and continue
independent work.

| Situation | Proof |
| --- | --- |
| Existing untested behavior | Establish regression proof before changing it |
| Structure slice | Keep external behavior green |
| Interim behavior | Name the later slice that removes or replaces it |

Run focused checks at slice boundaries. Run every suite only when the project
workflow or user requires it. If asynchronous ownership changes, observe the
named lifecycle owner detecting background failure in time and cleaning up as
applicable after failure or shutdown. Use the lifecycle contract for timing.
An awaited exception alone proves neither obligation.

Obtain only missing observations within authority. If one is unavailable,
report the covered cases and the exact promise that remains incomplete. For
filtered runs, name observed variants and list unrun readers as unverified.
Give counts with the unit and matching retained observation.

## Record the consumer trace

Run the installed [consumer-trace script](../../dough-execute-plan/scripts/consumer-trace.mjs)
against the checkout. Use `--base HEAD` for uncommitted changes.

```sh
node '<installed-execute-plan>/scripts/consumer-trace.mjs' --checkout '<checkout>' --base <revision> [--term '<text>']…
```

The script searches the whole checkout using changed paths, basenames,
declarations, import paths, and removed literals. It lists consumers by kind,
including empty kinds. Supply `--term` for effects the diff may miss, such as
retired behavior and altered preconditions, in the words observers use.
Resupply a derived term marked too broad when it names the changed contract.
For matched step definitions, page objects, helpers, and stand-ins, add the
relevant step phrases or exports and rerun until their journeys and callers appear.

Give each listed consumer `run`, `reused`, or `excluded: <reason>`. Exclude
only when the effect does not reach its boundary. Name groups of generic or
historical matches together when one reason covers them. Reassess earlier
exclusions against the current effect. A consumer that reads changed output
is reached even if it asserts another subject.

For each reached purpose, inspect the actual setup and observing assertion or
signal. Select proof by what the consumer uses, including indirect callers
and enclosing observations. Equivalent purposes can share sufficient proof.
Independent behavior, static, layout, and contract obligations each need
their own observation. A match, passing count, re-export, or file-level
`run` alone is insufficient proof.

Supply the observed surface as a trace term, such as a stand-in, recorder,
reader, or rendered output. Run each suite that reads the changed output,
including readers in untouched tests and helpers. Run the whole suite if it
fits the slice's focused-check time, otherwise run the affected readers. If
the trace and inspection find no affected consumers, keep focused proof.
Ask for the specific unresolved domain requirement and stop only the proof
that depends on the answer.

Keep the full discovery record. For each affected consumer purpose, connect
the changed effect and reached boundary to the proof type and observation or
gap. In its disposition, record or link the literal command or steps, setup,
assertion or signal, and result. For reused proof, record why it still
applies to the effect, boundary, implementation, and conditions. Preserve
independent proof and evidence for unchanged boundaries. Before dependent
acceptance or publication, obtain each missing observation or return the
slice with its consumer, proof type, and gap. Leaving it to CI or passing
nearby checks does not settle it. Apply the failure policy to failed checks.

## Accept a repair on its affected behavior

Cover every failure the diagnosis names, including CI failures. Replay the
whole journey the cause reaches, including transitions before and after
the failing step, such as opening, operating, dismissing, focus return, and
navigation. Also prove the consumers of the repair's own changed effect.
Combine failures on one surface in one check. If the repair removes or
loosens the only assertion that distinguishes a promise, restore an
equivalent observation or report that promise incomplete.

## Plan and reuse developer-operated observation

Request developer action only when an open proof question requires a step
your tools cannot perform or an observation they cannot make. The probe
slice names each question, the rounds that answer it
(such as refusal and retry per supported browser), and the steps the developer
must do, such as authenticating locally or operating a native dialog.
Include all of them in the first request.

Judge baseline reuse by
[correspondence to the candidate and conditions](../../dough-execute-plan/references/live-action-regression-proof.md).
If text or another detail changes while the observed path stays the same,
reuse the baseline and prove the change with a regression check at the
emitted source. Cite both in acceptance. If the path changes what it consults
or does, such as checking a different permission or using a different sender
or click mechanism,
request one fresh observation for that path and reuse the rest. A promise
the baseline never observed remains unproved.

Use the plan's [retained-proof fields](planning.md#write-an-executable-plan)
for the native baseline. Record the promise, covered boundary, inspected setup
with revision and settings, observation locations, literal command or
developer steps, and result.

## Choose expensive proof before launch

Before long, repeated, or resource-intensive verification, assess the target
host using available evidence about workload, concurrency, duration or cost,
load, memory, and competing work. Name the host and conditions, and flag
missing, stale, or inaccessible observations.
Local measurements cover the local host. Evidence gathered after failure
informs the next attempt.

Apply the project's proof policy, including any thresholds or approval
requirements. Within existing authority, choose to run, reduce concurrency
while preserving coverage, sequence owned heavy work, or defer while
independent work continues. A healthy host with a credible workload can
proceed after a brief inspection. Coordinate owned work through existing
handoffs and pause/resume rules, including across worktrees on the same host.
Leave unrelated processes with their owners.

Record the host, workload, evidence and unknowns, decision, command and proof
owner, and reconsideration condition in the existing plan, handoff, or
conversation. Apply the [artifact survival check](../../dough-execute-plan/references/proof-evidence.md#preserve-decisive-artifacts).
For a deferral, name the outstanding observation, owner, and next condition.
The command owner follows a run through its terminal result.

Before repeating, reassess if failure, contention, workload changes, or stale
evidence invalidates the prior decision. Repeat an overloaded run only with
changed conditions or a supported basis. Use a bounded diagnostic retry to
test a stated explanation.

Keep required verdicts and accepted unchanged proof. A focused pass covers its
own boundary and leaves a failed required broad verdict open. Diagnose
independent fixture and behavior defects under the
[failure policy](../../dough-execute-plan/references/failure-policy.md).
A headroom decision leaves timeouts, coverage, scope, and authority with
their existing human owners. Report missing proof and keep the affected
promise incomplete.
