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

Before changing a shared operation or choosing its proof, inspect affected production
and relevant test-support call sites — fixtures, stand-ins, and harness helpers that
invoke the contract — reusing available product-wide search. Derive obligations from
each caller's actual use, not method name or dominant use; exclude unrelated consumers.
A prior unaffected-suite or unused-consumer exclusion is invalid when this change still
reaches that caller: reassess current consumers before relying on it. Incompatible
purposes each need an observation; equivalent purposes may share sufficient proof. Do
not require every suite or an exhaustive caller inventory when sufficient
equivalent-purpose proof already exists. For unresolved domain purpose, ask precisely
about that caller's requirement and stop its dependent obligation until answered rather
than guessing policy.

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

Run focused relevant checks at slice boundaries. Require broader suites only when
this project's workflow or user requires them. When asynchronous ownership
changes, prove that the named lifecycle owner observes background failure in
time and performs applicable cleanup after failure or shutdown; an awaited
exception alone proves neither. Derive timing from the selected lifecycle
contract rather than an arbitrary timeout.
