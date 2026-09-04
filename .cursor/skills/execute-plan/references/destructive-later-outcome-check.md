# Destructive later-outcome check

Run this compact check at the pre-slice gate, before delegation.

1. Decide from the current slice text and active PLAN whether the slice would
   remove or disable a user-visible path, field, persisted state, or scheduled
   behavior. If not, proceed without extra analysis or another agent.
2. If it would, compare the named removal target with the explicitly named
   outcomes in every later slice in document order, regardless of status. Keep
   the comparison compact: removal target, relevant later outcome(s), and
   compatible or conflict. Do not invent implicit future requirements.
3. If no later named outcome depends on the target, proceed with normal
   pre-slice gates and delegation.
4. If the current instruction is unambiguously stale relative to a later named
   outcome, update the PLAN and record which outcome exposed the conflict and
   why the instruction was stale. Restart the complete coordinator loop at step
   1; do not delegate until the PLAN has been reread and every pre-slice Jidoka,
   grammar, dependency, destructive-outcome, and refinement gate has rerun.
5. If both outcomes remain valid and resolving the conflict requires a value or
   design choice, Jidoka before implementation. Report the two outcomes and the
   decision required; do not silently rewrite the PLAN.

This is an inverse safety check: it prevents current work from destroying an
explicitly planned outcome. It does not widen post-change-refactor's existing
rule that only the immediate next slice can justify retaining otherwise
speculative code.
