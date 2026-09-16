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

## Shotgun surgery

- Shotgun surgery: **one logical concept** (e.g. a version string) forces edits
  in many places for one purpose.
- Give shared knowledge **one** authoritative home across the concept's
  representations and orchestration. The next rule change should not require
  parallel updates to handlers for successive delivery examples.
- Acceptable extra touchpoints: tests that assert the concept, and generated
  code derived from it. Do not hardcode the same value in product paths,
  fixtures, E2E config, and feature files in parallel.
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

Assess coverage at observable boundaries and its execution cost. Retain E2E
scenarios that document important user journeys and prove integration; neither
newness nor age is a reason to remove a test. Move detailed rule variations to
black-box unit coverage when they do not need the integrated path. Consolidate
overlapping scenarios, including unit tests, only when their meaningful behavior
coverage and important integration proof survive at lower cost.

Use this project's preferred test style when supplied. Otherwise assert
observable black-box behavior in the following "small test" style. Small means
fast, focused proof through a stable boundary, not one isolated test per class:

- Drive the public surface or an intentional domain-stable contract, such as a
  mounted component, command entry, controller, or independent algorithm.
  Exercise real lower-level production code with crafted preconditions. Do not
  widen exports for tests or create thin tests for each internal collaborator.
- Mock true external dependencies relative to that boundary, not internal
  collaborators. Prefer real persistence and rendering when they are part of
  the behavior being proved; apply the project's explicit exceptions. A mocked
  service response cannot also prove that service or its transport works.
- Give each test one behavior and a descriptive name. Assert the canonical
  result shape once; sibling cases assert their distinguishing outcome. Omit a
  repeated assertion only when it is independent of the changed precondition.
  Prefer a positive distinguishing signal when sufficient; retain absence or
  negative assertions when absence is the actual promise.
- Assert the result of a loop, retry, or threshold scenario without repeatedly
  checking unrelated intermediate state. Keep intermediate observations when
  ordering, progress, or transient behavior is itself the contract.
- Parameterize cases when only data changes and the behavior and assertion
  focus stay the same. Keep distinct behaviors visible; do not turn unrelated
  tests into a branching scenario or one long journey to reduce the test count.
- Use the project's builders and meaningful defaults for concise fixtures.
  Specify only data affecting the behavior or observation. Prefer builder
  operations to post-construction mutation and domain-shaped helpers to repeated
  field lists. Extend an existing builder for recurring setup rather than
  introducing a parallel fixture framework. Share construction, not mutable
  state that makes tests depend on order.
- Preserve fixture completeness for destructive operations: exercise every
  relevant relationship in the deletion or cascade path. For relational data,
  include rows throughout the affected foreign-key/cascade closure. A bare
  target record proves selection, not the effects on related records. Minimal
  setup must not erase this protection.

An E2E test is not automatically a better boundary than a unit test. Replace
tests of internal structure with behavioral coverage, not merely fewer
assertions. Scenarios should show precondition, trigger, and observable result;
setup must not supply the very outcome the test claims the product establishes.

Identify the surviving proof for each proposed consolidation and establish
replacement unit coverage before removing or narrowing detailed E2E cases.
Apply the invoking workflow's scope and authority: post-change refactoring owns
tests implicated by its change; an implementation retrospective assesses the
whole suite and plans corrections without implementing them.

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
