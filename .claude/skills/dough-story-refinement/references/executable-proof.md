# Own executable proof

Map every checkable final-state promise in the plan's source and current
decisions to an owning slice and observable proof. Inline links or a compact table
are sufficient. Include applicable promises, not broader aspirations. Passing
commands without the promised observation does not establish completion.

Preserve mappings through refinement, replacement, and resume. Repoint promises
before declaring replacement slices ready; orphaned promises leave the plan
incomplete. Preserve completed evidence unless a changed boundary invalidates
what it covers. For interim replacements, align affected callers, fixtures,
assertions, and documentation with the final success and rejection behavior.

Choose the smallest sufficient proof at the stable boundary of the promise.
Inspect assertions and setup: distinguish starting preconditions from behavior
the product promises to establish. A fixture or seam supplying that behavior
leaves it unproved; keep the evidence for what it actually observes. An inner
operation finishing does not prove completion for its caller.

Before changing an operation, contract, message, value, default, or rendered
output, or choosing proof for that change, identify its effect: what can now be
observed differently, where it appears, or which operation preconditions now
apply. Trace that effect to its consumers in production and test support. Changed
files, selectors, helper references, literals, and the plan's named proof locate
starting points; they do not bound what observes or relies on the effect.

For output, follow it into enclosing surfaces and inspect how they are observed,
including aggregate text, counts, roles, and keyboard order. A whole-container
assertion may include hidden content and text boundaries without naming the
changed component. For an operation or default, follow the entry points,
continuations, and recovery paths that reach it, then inspect their scenarios
and setup. Moving an operation out of a conditional path can make its resources
necessary for callers that previously bypassed it; include scenarios that omit
those resources. A helper check or setup supplying the newly required resource
does not prove those callers still work.

For removals, renames, or relocations of symbols or modules, or changes to callable
contracts, search production code, tests, and fixtures for old symbol names,
module paths, and call forms, including in unchanged files. Include private
symbols used across files. Keep this targeted to meaningful cross-file changes,
not every local variable.

Reuse applicable searches, extending matches to the enclosing observation or
entry-point scenario and its support. Inspect actual assertions and setup before
selecting focused proof for affected consumers' behavior; a list of matches alone
does not establish the affected boundary. Filename patterns and passing test
counts alone do not establish coverage. Include retired literals and values
(message text, status code, label, output form), callers relying on a changed
default by not overriding it, and every fixture, stand-in, or harness helper that
invokes or imitates a changed contract, including untouched ones. Account for the trace
and the observing proof in the existing proof location or implementation return.
Derive obligations from each consumer's actual purpose, not its method name or
dominant use. Exclude a consumer only when the effect does not reach its boundary;
retain sufficient current proof for unchanged boundaries. A prior
unaffected-suite or unused-consumer exclusion is invalid when this change still
reaches that consumer: reassess current consumers before relying on it.
Incompatible purposes each need an observation; equivalent purposes may share
sufficient proof. When the change has such consumers and the suite covering the
changed surface runs within the slice's focused-check time, run that suite instead
of hand-picking the consumers it contains; run consumers outside it as well. A
change nothing else observes, such as renaming a helper used only within one
file with no shared contract, text, value, default, or rendered output, keeps
focused proof. For unresolved domain purpose, ask precisely about that consumer's
requirement and stop its dependent obligation until answered rather than guessing
policy.
Discovering a failing assertion does not authorize weakening its promise; align
behavior and proof under the project's existing failure policy.

For artifact-preservation promises, identify installation, physical store, and
predecessor using project-supplied identities/scope. Same-store continuity proves no
transfer from another store. Surface target/scope conflicts before dependent work
(e.g. preserving a Docker volume while the owner's native data lives elsewhere).
Migration needs authority and proof; deferred migration is not completed. Ordinary
single-store continuity reuses matching evidence without inventing a predecessor or
migration task. Apply conditionally, not as a mandatory story section.

When acceptance needs a pre-change observation, obtain it before dispatching the
change that would invalidate it. Reuse adequate baselines with known matching revision
and relevant environment/selection conditions. Missing/failed prerequisites stop only
the dependent path; name the gap and continue unrelated work. If the original baseline
is unrecoverable, label a reconstructed comparison and prove revision/conditions
comparable; otherwise its claim (e.g. speedup) remains unproved. Apply conditionally,
without benchmarking every story or delaying independent work/setup.

Reuse sufficient evidence. Obtain only missing observations within authorized
work; if unavailable, report what is covered and the specific unproved promise.
That promise remains incomplete; reporting the gap does not fulfill or remove
it. Limit success claims to the observed cases.

| Situation | Proof |
| --- | --- |
| Main user behavior | Targeted end-to-end check or another real high-level boundary |
| Edge, error, or pure contract | Focused unit proof |
| Existing untested behavior | Regression proof before changing it |
| Structure slice | Existing external behavior remains green |
| Interim behavior | Name the later slice that removes or replaces it |

Run focused relevant checks, including the consumers above, at slice boundaries.
Run every suite only when this project's workflow or user requires it. When
asynchronous ownership changes, prove that the named lifecycle owner observes
background failure in time and performs applicable cleanup after failure or
shutdown; an awaited exception alone proves neither. Derive timing from the
selected lifecycle contract rather than an arbitrary timeout.

## Choose expensive proof before launch

Before long, repeated, or resource-intensive verification, make a proportionate
decision about the machine that will actually run the command. Identify the
planned scope and concurrency, available duration or cost evidence, current
load and memory evidence, and known competing work on that machine. Attribute
observations to their host and relevant conditions; qualify missing, stale or
inaccessible evidence. Your local machine's measurements say nothing about an
unobserved remote runner's headroom. Evidence obtained only after a failure
informs the next decision; do not backdate it to the launch.

Apply the project's proof policy when supplied. Otherwise make an explicit,
bounded choice within existing authority: run, reduce concurrency while
preserving required coverage, sequence owned heavy work, or defer the expensive
proof while useful independent work continues. A healthy target host with a
credible workload can proceed after a brief inspection; no universal load or
duration cutoff or routine human approval is required. Coordinate only owned
work through existing handoffs and pause/resume rules. Separate worktrees can
share the same machine; leave unrelated processes with their owners.

Retain the host, workload, evidence and unknowns, choice, command/proof owner,
and reconsideration condition in the existing plan, handoff or execution
conversation. Keep this concise enough for the decision; create no host registry.
A deferral leaves the exact required observation outstanding with its owner and
next condition. The command owner remains responsible through its terminal
result under the existing workflow.

Reconsider before another expensive attempt when failures, contention, changed
workloads or stale observations remove the previous basis. Do not repeat an
unchanged overloaded attempt without changed conditions or another supported
basis. A bounded diagnostic retry tests a stated explanation; repeated runs
until green do not establish it.

Preserve strict verdicts and sufficient accepted unchanged proof. A focused pass
observes its own boundary and cannot erase a failed required broad verdict.
Host pressure does not dispose of an independent fixture or behavior defect;
diagnose and repair it through the existing failure policy. A headroom decision
does not itself authorize relaxed timeouts, reduced proof obligations or changed
scope. Leave changes to coverage, scope or authority with their existing human
owner, and report missing proof instead of declaring success.
