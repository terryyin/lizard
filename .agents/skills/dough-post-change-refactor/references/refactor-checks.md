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

### Choose the test boundary

Maintain exactly two layers of automated behavioral tests, end-to-end (E2E)
and unit. Use this project's test conventions for the tests in scope.

- **E2E.** Keep a few representative workflows. Start at a supported product
  entry point and use the real services whose interaction is needed to prove
  the observable outcome.
- **Unit.** Put cases that one service can prove in unit tests. Start
  at that service's highest practical entry point for the behavior, such as its
  UI, API, or command, and exercise its real lower-level code. Use a lower-level
  entry point only when it exposes a stable, domain-meaningful contract that
  can prove the behavior, such as a pricing rule. Keep private helpers behind
  their production callers.
- **Mocks.** Keep unit tests independent of other live services. Substitute an
  external dependency only when needed and only at its service boundary, such
  as a backend response consumed by the frontend. Use real collaborators within
  the service, including persistence or rendering when the behavior depends on
  them. Limit proof claims to what the real code in the test establishes.

### Write observable cases

Use this project's test style and assert observable behavior.

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

Show the precondition, trigger, and observable result. Use setup to supply
preconditions and let the product establish the outcome. Consolidate cases only
when meaningful behavior and integration proof survive at lower cost. Identify
the surviving proof and establish replacement unit coverage before narrowing or
removing detailed E2E cases. Follow the invoking workflow's scope and authority.

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
