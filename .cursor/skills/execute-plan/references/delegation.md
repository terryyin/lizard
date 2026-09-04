# Slice Delegation

Use the **Task tool** (`subagent_type: "generalPurpose"`; or GSD `gsd-executor`
when inside `/gsd-execute-phase`). Keep wrap-up coordinator-owned; do not rely on
`gsd-executor` to run local post-change-refactor.

The implementer prompt must include:

1. The plan path and current slice text, but not the full plan history or Jidoka
   list.
2. A Jidoka stop for value/design forks, missing credentials, undiagnosed
   unrelated failures, or ambiguity.
3. `problem-decomposition.mdc`, `planning.mdc`, and `gsd-coexistence.mdc`,
   including the ~5-minute fuzzy / >10-minute hard split budget, relevant-test
   proof, no commit on red, no deliberately broken CI, and capability naming.
   Also `basic-development.mdc`, `issue.mdc`, and `lizard-rule.mdc` when they
   apply. Full pytest is coordinator wrap-up, not the implementer's job.
4. A hard stop before wrap-up: do not commit, push, mark the plan done, or run
   post-change-refactor. Leave relevant tests green and the tree uncommitted.
5. `revert_and_split` when the slice is too big.
6. `nix develop -c <command>`; Git needs no Nix prefix.
7. A short return: ready for wrap-up with tests, Jidoka stop, or reverted and
   split. Do not claim the slice is done in Git terms.

Resume context remains in the plan on disk.
