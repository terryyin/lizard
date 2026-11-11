# Approaches for speeding up tests

Use the [shared behavioral test guidance](../../dough-post-change-refactor/references/refactor-checks.md#tests-as-behavioral-documentation)
for test boundaries, fixtures, and replacement proof.

Consider these approaches in order. Choose and combine the ones that address
the measured cost and fit this project's constraints. Look beyond this list for
a better way to remove the bottleneck. Revisit your choice when a measurement
reveals a different cost.

1. **Use the smallest real scenario and data.** Keep the facts, boundaries, and
   steps needed to express the intent and catch the failure. Arrange incidental
   preconditions directly through an existing builder or application API.
   Keep the real interaction when it is the behavior under test.
2. **Test behavior this project owns.** Exercise the real implementation.
   Keep real protocol interactions when they prove an integration contract.
   Remove checks that only exercise a dependency's behavior. Avoid introducing
   mocks to save time.
3. **Remove repeated expensive work.** Share one real setup or operation across
   compatible observations and check each outcome independently. When immutable
   setup dominates, build it lazily behind the existing fixture or builder.
   Start each run with an empty seed cache, keep the seed read-only, and give
   each case a private copy of anything it mutates. Reuse application or render
   contexts when cases stay isolated. Look for unnecessary builds, installs,
   process launches, Git hooks, and repeated reads. Remove avoidable work before
   adding workers. Check isolation and contention before changing parallelism.
4. **Wait for observable events.** Replace sleeps and interval polling with
   completion events or observable state changes, then shorten the work that
   produces them. Advance a controlled clock at the component that owns the
   deadline when a controlled clock preserves the timing contract. For UI or
   terminal checks, await the visible state needed by the next action. Confirm
   the cost of selectors or input before changing them and retain any
   accessibility or real-input behavior they prove.
5. **Keep fewer checks with equivalent proof.** Combine related observations
   into one short scenario or remove a narrow check once a surviving check
   catches the same failure. Use representative cases and meaningful edges.
   Count executions when parameterizing. Use a targeted deliberate fault when
   replacement coverage is uncertain.
6. **Use a few full journeys for critical wiring.** After simplifying the
   expensive path, aim for one or two real end-to-end checks that cover its
   critical wiring. Move detailed variants to the cheapest real boundary that
   can catch their failures. Keep additional journeys for distinct outcomes that
   require them.
7. **Keep the intent visible.** Use shared builders for recurring domain
   setup and, where the project uses them, page objects for repeated browser
   interactions. Keep the facts that drive the case and the expected outcome
   in the test. Assess test and support code together and measure speed
   independently of code size.
