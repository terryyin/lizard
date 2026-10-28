# Recover an interrupted delegated result

Interruption may not stop the delegated agent. A delegated implementation or
refactor agent has been observed to keep working after an interruption and
even to commit and push despite a delegation that forbade committing and
pushing. The absence of a return proves only that no return arrived; it does
not prove the agent stopped or that the checkout is as it was. A return that
arrives later claiming work after the interruption is likewise a claim to
check, not acceptance evidence.

When a delegated implementation or refactor call is interrupted and no return
arrives, or a return arrives later claiming work after the interruption:

1. Treat the slice as incomplete. Never mark it done on the interrupted
   agent's claim or on a promised return.
2. Before any replacement writer, formatter, or delivery proceeds in that
   checkout, inspect the checkout itself — the uncommitted diff, the plan
   when plan edits are in scope, and the remote branch — for the interrupted
   agent's edits and for unexpected commits.
3. Re-establish focused proof from the checkout: rerun the focused proof
   against the actual files before accepting, repairing, or discarding
   partial edits. Partial edits without that proof are not accepted.
4. On an unexpected commit from the interrupted agent, whether it is in the
   checkout's branch or on the remote, pause and leave accepting or
   discarding it to the developer's decision. Do not deliver on top of it
   blindly.

Recovery rests on checkout and remote inspection and on re-established proof,
never on the absence of a return. This adds the interrupted-return path to the
existing delegation rule that the delegated agent does not commit, push, or
mark a slice done; it claims no way to stop an agent already running inside a
session.
