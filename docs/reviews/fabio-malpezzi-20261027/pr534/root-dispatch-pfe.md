# Canonical reader-root binding and dispatch

The exact 13c qualification in dispatch-performance.md remains failed:
original128/current median +3.794%, paired95% [3.102%,4.156%]; augmented166/
current median +2.882%, upper3.556%. Four other gates pass. All six children,
freeze, measurement driver and postcheck terminated0; outputs/imports/hashes
held. This is not a basis to retry unchanged code or relax independent gates.

## Actual retained cost and strongest simpler choice

No additional profile was launched. The already retained working profiles in
seed015-reader-dispatch exercise the exact product later committed as13c:

| TypeScriptStates.__call__ | Original128 | Augmented166 |
| --- | ---: | ---: |
| Own seconds, root and nested combined | 0.1996 | 0.2767 |
| Primitive calls | 561,900 | 735,800 |
| Total calls including recursion | 1,109,400 | 1,492,000 |

The whole own-time total is not removable: child states own real binding work.
This identifies another wrapper to inspect, not a throughput or full-savings
claim. Generic reader dispatch is already reduced; completed-binding metadata
would require new receipt fields/callback plumbing, and type-header receipts
would address only successful probes. Bypassing an unused canonical-root
wrapper is a smaller representation/dispatch change than either alternative.

## Read-only whole-owner PFE

The actual PFE launch/complete return and source readings are retained in the
parent execution conversation. It searched assignments across all language,
core, extension and test code, following existing owners and Accepted ADRs.

- JavaScriptBindingsMixin.__init__ initializes _binding_candidate toNone.
- _read_binding_candidate always creates a distinct self.__class__ child,
  installs JavaScriptBindings on that child and delegates via sub_state.
  Completion updates the parent's pending summary, not its binding owner.
- TypeScriptReader constructs one exact TypeScriptStates(context) root;
  JS/TSX/Vue reuse it. No product writer installs a binding owner on this root.
- TypeScriptStates.__call__ only checks/updates a binding candidate before
  delegating to CodeStateMachine.__call__. The root's check is unused.
- CodeStateMachine still invokes its active state. When that is a candidate
  child, Python calls that child's unchanged TypeScriptStates wrapper, keeping
  defaults/formals. Saved-state restoration, callback-once, last_token and exit
  propagation remain canonical.
- Parser-only next/replay calls still use normal self(token). Named formals,
  catch/object child summaries and initializer children keep their existing
  owners. Bypass must not expand to those children or replay operations.

The root lifetime is a static product invariant, not a promise supporting
private midstream reconfiguration. Ordinary custom hooks/state/container
fallbacks remain; a configured root with a binding owner must keep normal
behavior. The existing generic protocol and original counter stream remain.

## Selected remaining structure and proof

Use direct canonical CodeStateMachine dispatch only for the exact recognized
reader root additionally confirmed without a binding candidate. Change no
CodeStateMachine, TypeScriptStates, binding summary, parser or metric counter.
Retain root finalization/eof and current custom/configured-root fallbacks.

Preserve all166 seven-field public/FileAnalyzer rows and fixed-corpus CLI
bytes against exact13c; observe nested/default/formal/callback/replay behavior,
configured binding-root behavior and current reader/hook/worker consumers.
Use a fresh implementation and independent refactor, with one bounded labelled
after-change diagnostic only. Statistical qualification is after exact commit.

Keep completed1–7. Refine unfinished8 into this Structure8, then unchanged
six-gate qualification9 and contributor delivery10. Ten cohesive slices fit
the supplied unconstrained sizing; no resplit or source-scope choice is needed.
Reassess readiness through the shared recorder. Source-review inventories stay
with their unchanged obligations; new structure receives a fresh isolated one.
All six future paired95% upper bounds remain strictly below3%, without waiver.
