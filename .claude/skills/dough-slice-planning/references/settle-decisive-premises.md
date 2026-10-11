# Settle decisive premises

A decisive premise is a factual claim about this project's current state on which a
slice's approach, sizing, or proof depends: code and tests, host or environment state,
fixture content, workload data, or a named proof or measurement command. Inherited
premises such as "works as today" count the same as those you write.

Derive premises from key examples: trace each from trigger to observable result through
existing code. Every step assumed to behave is decisive, written down or not, including
colliding overlays or rules and transformations from fixture inputs to their evaluation.
A premise is settled only when its recorded observation reaches the consuming operation
in the checkout the plan names and records the promised outcome: the example's result,
or a proof's failure without the behavior it claims to prove. "Exists", "found", "callers
listed", "step defined", "server answered", and "no commit to the spec since" are
presence, not settling; leave the premise open and name the unreached operation.
A claimed fix is a premise: reproduce the symptom before dependent work. A remedy spec
that already passes leaves the symptom unexplained. Run an existing fixture when it can
observe the journey cheaply and safely, instead of reading call sites.

Before `ready`, use the smallest safe observation reaching the claim's outcome, wherever it lives.
Reading, searching, listing, and read-only host queries locate it; presence cannot settle it:

- For a behavior-consumer premise (dependents, paths, or effects of a change), follow the
  effect from its state write or server response through every transformation to what
  asserts or relies on it. Search tests and all callers, including scripts, step definitions,
  and specs that drive HTTP sequences, rather than only names or direct imports.
  With a cheap local suite, the smallest complete observation is one scratch application
  of the planned change, one relevant test run, and a revert; failures identify consumers.
  For a moved function reached through a script, run the feature's script steps with a
  scratch failure in the function; record that step's failure. Caller traces or unit imports do not settle it.
- For a proof premise (a named test proves a behavior), observe discrimination: it fails
  with the behavior disabled in a scratch edit and passes with it enabled. If it stays
  green without the behavior, record the premise false and give the slice a discriminating proof.
- For a proof-route premise (an example runs through a route in a named checkout), execute
  its unpaid, side-effect-free prefix there (runner spec selection, tools, token route, or log source),
  stopping at the first paid, credentialed, owner-held, or state-changing step. A refusal or
  missing step makes the route unavailable. Name an available route (CI or the primary checkout
  under this project's rules), or record the missing step and decision owner as an early probe slice or
  pre-Take decision. Reachability without the example's real shape (authenticated write or attached file) is presence.

Scratch observations run only in the preparation's owned workspace, never a default or shared
checkout; revert scratch edits before any record write or commit. For uncertain infrastructure
or storage behavior, use one isolated representative proof against the relevant engine and
version, unless matching evidence exists. Record each premise, its consuming operation, the
literal observation reaching it, and its result in the plan. A false premise changes the plan
before broad implementation. Do not inspect claims the approach does not depend on, and keep
experiments off shared and production systems.

When only a paid, credentialed, owner-held, or state-changing observation can
settle a premise, observe its cheap parts now and make the remainder an early
probe slice whose failure stops dependent slices and changes the plan. Such a
plan can be `ready`; the probe's observation keeps its existing authority
requirements.
