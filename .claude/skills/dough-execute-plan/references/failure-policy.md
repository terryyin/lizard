# Failing and flaky tests

Apply this policy to every failing or flaky test met while planning, executing,
repairing or wrapping up work. Carry it by link or verbatim into every plan,
delegation brief and repair handoff, not as a weaker paraphrase such as "fix or
report". Apply any additional failure rules this project's instructions give
alongside it.

- A failing or flaky test is a defect to fix. Tests must be stable and give the
  same result locally and in CI. A test that fails locally but passes in CI, or
  the reverse, or passes alone but fails in the full run, has a bug in the test
  or its environment assumptions. A flaky test counts as failing until it is no
  longer flaky.
- Reproducing a failure on the base or trunk is attribution, not an exemption.
  A plan may record that a known failure predates its story; it does not
  exclude that failure from the executing agent's repair, and no executor
  reports it and moves on.
- CI failing on a revision you published is yours to diagnose and repair, even
  when the step also failed on earlier commits or does not reproduce locally,
  through to the applicable final CI. Rerunning until green is not a fix. Stop
  for a human only when the fix needs product, design or credential judgment.
- When another active execution already owns that exact repair, preserve its
  uncommitted files: do not stash, claim, or write a duplicate repair. Report
  that owner and the remaining gate as the coordination boundary. A claim that
  a failure is "pre-existing" is not evidence of such an owner.
- A human-authorized exception, such as waiving one native probe or authorizing
  one landing despite a failure, covers only its stated scope. Report the
  outcome it qualifies truthfully; it never becomes the default for later work.
