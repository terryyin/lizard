# Refactor Checks

Inspect these checks in order during the skill's read-only decision pass.
Edit only after its scope and subsystem gates permit the candidates.

## Duplication

- **"New" duplication** means at least one copy is newly introduced or closely
  related to newly introduced code — not that every copy is new. Collapse it
  even when the other side already existed.
- Trace repeated domain knowledge through handlers, orchestration, and parallel
  representations, including existing code outside the diff. Shared persistence
  or utility helpers do not establish cohesion when callers still repeat the
  same rules or recognize successive example layouts separately.
- Distinct representations may serve distinct responsibilities; identify which
  rules need one authoritative home and which differences the domain requires.
- **Action:** use the simplest common rule supported by current evidence and
  make the complete implicated concept coherent. Reuse a suitable existing seam
  when it owns that rule; helper extraction alone is insufficient if repeated
  knowledge remains in orchestration. Preserve promised behavior and genuine
  constraints under the skill's decision gates. Do not introduce another
  recognizer or an extensibility framework justified only by hypothetical cases.

## Domain naming

- Read identifiers across the implicated concept, including untouched parallel
  representations — files, modules, classes, functions, variables, tests,
  feature files, fixtures.
- Ask: does the name match what a domain reader expects? Does it match this
  project's domain vocabulary?
- **Action:** rename when intent is unclear, misleading, mixes layers, or leaks
  development sequence numbers. Name product code by capability; keep sequence
  numbers in planning artifacts. Renaming story-shaped handlers is insufficient
  when their responsibilities still encode delivery groupings; check whether
  independent domain reasons justify the structure.

## Direct domain mapping

- Map persistence, implementation, APIs, UI, and tests directly to the domain's
  concepts and meanings, with the minimum necessary translation. When a system
  boundary requires a transfer shape carrying a subset of domain data, preserve
  domain concepts and share or generate one typed contract across internal
  callers rather than translating by hand on each side.
- When grouping operations behind a production boundary, use
  [what the unit test suite needs to construct it](#design-production-code-for-testability)
  to check whether those operations belong together.
- **Action:** collapse unnecessary mapping layers, parallel handwritten payload
  shapes, and domain rules trapped inside untyped transport or command adapters
  behind the typed boundary that owns the concept.

## Design production code for testability

Give handlers, controllers, and UI components strongly typed contracts.
Each variant accepts only the data and services it uses. A unit case builds
those inputs and calls or renders the same entry point production uses.
You may restructure production code for testability while preserving its
observable behavior. Never add behavior to production code solely for tests.

For request handlers, the adapter resolves request context and supplies the
process-wide state the handler needs. Pass the request signal through every
cancellable read, including admission, and keep local deadlines.

## Shotgun surgery

- Shotgun surgery: **one logical concept** (e.g. a version string) forces edits
  in many places for one purpose.
- Give shared knowledge **one** authoritative home across the concept's
  representations and orchestration. The next rule change should not require
  parallel updates to handlers for successive delivery examples.
- Acceptable extra touchpoints: tests whose own names promise the concept,
  and generated code derived from it. A test edited for a change it is not
  about has this smell: loosen what it observes rather than update its value. Do not
  hardcode the same value in product paths, fixtures, E2E config, and feature
  files in parallel.
- **Action:** consolidate now behind one seam (one constant, config, or module).
  Leave only low-likelihood one-offs unabstracted.

## Dead or redundant code

Apply the [scope and justification boundary](../SKILL.md#discover-scope)
to code the current change introduced or exposed. Remove:

- Code with no caller.
- Unreachable branches.
- Pairs of edits that cancel each other (added then worked around, flags that
  never flip).
- Production code only exercised by unit tests — no real caller from a
  controller, mounted component, CLI command, MCP tool, or other entry.

Remove speculative code within that boundary; do not retain it for an
unspecified future need.

## Tests as behavioral documentation

### Choose the test boundary

Maintain exactly two layers of automated behavioral tests, end-to-end (E2E)
and unit. Use this project's test conventions for the tests in scope.

- **E2E.** Keep a few representative workflows. Start at a supported product
  entry point and use the real services whose interaction is needed to prove
  the observable outcome.
- **Unit.** Put cases that one service can prove in unit tests, exercising its
  real collaborators and persistence:
  - **Backend / API:** Start at the highest typed handler or controller
    behind a thin wire adapter. Test the wire directly only for wire behavior.
  - **UI:** Start at the smallest rendered component or view that owns the
    interaction under test, keeping hooks and state behind rendered UI.
  - **CLI / scripts:** Keep untyped command surfaces thin over typed or domain
    modules where practical; otherwise start at the command or exported module
    contract.
  - Use a lower-level function only when it exposes a stable, domain-meaningful
    contract that alone proves the behavior, such as a pricing rule. Keep
    private helpers behind their production callers.

  If setup at the chosen entry point requires unrelated data or services,
  [review the production contract](#design-production-code-for-testability).
- **Mocks.** Keep unit tests independent of other live services. Substitute an
  external dependency only when needed and only at its service boundary,
  through a shared or generated typed contract rather than handwritten URLs or
  payloads. Limit proof claims to what the real code in the test establishes.

### Write observable cases

Use this project's test style and assert observable behavior.

- Before adding or changing a test, name a concrete regression its assertions
  would catch, and assert the observable result of exercising the code or
  workflow. Do not restate configuration, constants, fixture data, builder
  output, or a guidance file's own wording. Assert the effect, reading the
  expected value from its source. Keep an agreement between two settings or
  executed results. When reviewing a test diff, remove assertions that would
  catch no such regression.
- Give each test one behavior; its subject is what its own name promises,
  not its file, header or plan row. Give a promise the change adds its own
  test, sharing setup; do not add it to another behavior's test or widen that
  test's name or header. An assertion fails only for a change that test is
  about. In a test about one part of an output, a fixed position, a count,
  whole-list or whole-object equality, or a full-string match is too strict;
  observe that part. Assert the whole shape in the one test whose subject is the shape, and
  a default's value in the one test about that default. Omit a repeated
  assertion only when it is independent of the changed precondition. Prefer a
  positive distinguishing signal when sufficient; retain absence or negative
  assertions when absence is the actual promise.
- When several tests observe the same output, give test support one
  domain-named reader for it, such as the value of a command's option.
- Synchronize on an observable event or advance a controlled clock. Never
  sleep or wait for wall-clock time to pass.
- Assert the result of a loop, retry, or threshold scenario without repeatedly
  checking unrelated intermediate state. Keep intermediate observations when
  ordering, progress, or transient behavior is itself the contract.
- Parameterize cases when only data changes and the behavior and assertion
  focus stay the same. A body that branches on its case parameter holds
  separate tests: write each one, sharing setup through a builder or helper.
  Keep distinct behaviors visible; do not turn unrelated tests into a
  branching scenario or one long journey to reduce the test count.
- Compose test inputs from the project's domain builders. Use their defaults
  for details that do not affect the case. If setup needs data or services
  beyond the entry's contract,
  [revisit the subject's contract](#design-production-code-for-testability).
  Extend existing builders with domain-named operations for recurring setup.
  Use them in place of repeated field lists and post-construction mutation.
  Give each case its own mutable state.
- Preserve fixture completeness for destructive operations: exercise every
  relevant relationship in the deletion or cascade path. For relational data,
  include rows throughout the affected foreign-key/cascade closure. A bare
  target record proves selection, not the effects on related records. Minimal
  setup must not erase this protection.

Show the precondition, trigger, and observable result. Use setup to supply
preconditions and let the product establish the outcome. Consolidate cases only
when meaningful behavior and integration proof survive at lower cost. Identify
the surviving proof and establish replacement unit coverage before narrowing or
removing detailed E2E cases. Follow the invoking workflow's scope and authority.

In Cucumber scenarios and Playwright step titles, express one domain intent
per step. `Given` declares the starting state, `When` names the action in active
voice, `Then` asserts an observable result, and `And` continues the preceding
keyword's role. A Playwright test body contains only named `test.step(...)`
calls. Put all setup, actions, and assertions inside them.

Prefer one statement per Cucumber step definition or Playwright step callback.
Each body has at most three statements and ten nonblank lines of code, excluding
the step declaration and closing delimiter. Count a fluent chain as one
statement even when wrapped or split by `await` to use the returned object.

## File size

For every file in the current diff and every file proposed for editing:

```bash
wc -l <path>
```

Shorten or split every checked file exceeding **250 lines** before claiming
refactoring complete.

- Split along **cohesive seams** — one concept per module, not arbitrary line
  cuts.
- Update imports. Keep the public API stable for callers outside the change.
- When the caller supplied a reorganization decision for an already-oversized
  file this change edits, check that the delivered structure matches that
  decision and preserves the stated behavior and proof. A fulfilled planned
  reorganization for that file completes this check without another exception
  question. A missing, contradicted, or undelivered decision — including a plan
  mention without the delivered seams — is not a clean result: return through
  the skill's existing decision stop with the file, current size, missing or
  contrary evidence, and affected work. Do not treat a mechanical line cut as
  the design, and do not grant an automatic exception from a short diff, a
  prior exception, or a plan mention alone.
