# Slice Delegation

Use a fresh general-purpose sub-agent (or GSD `gsd-executor` when inside
`/gsd-execute-phase`). Keep wrap-up coordinator-owned; do not rely on
`gsd-executor` to run local post-change-refactor.

The implementer prompt must include:

1. Plan path, current slice, and its mapped promises/observations from across
   the selected contract under `planning.mdc`'s Proof decisions (including
   applicable replacement/lifecycle obligations). Omit full history/Jidoka lists.
2. A Jidoka stop for value/design forks, missing credentials, undiagnosed
   unrelated failures, or ambiguity.
3. `problem-decomposition.mdc`, `planning.mdc`, and `gsd-coexistence.mdc`,
   including the ~5-minute fuzzy / >10-minute hard split budget, relevant-test
   proof, no commit on red, no deliberately broken CI, and capability naming.
   Also `basic-development.mdc`, `issue.mdc`, and `lizard-rule.mdc` when they
   apply. Do not run a broader suite unless the slice's proof names that suite.
   Full pytest is coordinator wrap-up, not the implementer's job.
4. A hard stop before wrap-up: do not commit, push, mark the plan done, run
   post-change-refactor, run `make format-changed`, or run standalone
   `make lint-changed`. Leave relevant tests green and the tree uncommitted.
5. `revert_and_refine` when the slice is too big; the coordinator will invoke
   **slice-plan-refinement** on the existing PLAN.
6. `nix develop -c <command>`; Git needs no Nix prefix.
7. A short return: ready for wrap-up with one or more compact proof blocks,
   Jidoka stop, or reverted and ready for refinement. Do not claim the slice is
   done in Git terms. Use this repeatable shape for every green focused command:

   ```text
   proof:
     command: <exact focused test command>
     covers: <behavior or paths this command covers>
     result: pass
   ```

   Connect the returned evidence to those mapped promises; report uncovered
   behavior as incomplete implementation, not refactor work.
   The command must be literal and complete. A placeholder, abbreviation, or
   paraphrase is missing or ambiguous proof.

Resume context remains in the plan on disk.
