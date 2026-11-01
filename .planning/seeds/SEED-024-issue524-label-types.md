# Resolve remaining label and type-annotation boundaries

<a id="resolve-issue524-label-types"></a>

### Resolve label and annotation boundaries for issue #524

**Identity:** SEED-024#resolve-issue524-label-types
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/016-issue524-label-types/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"65f470e0f727139fdfd41003c3339e532a233b543f863e42db9e2d096ed11a9c","plan":"1b1f52efcc40d02d61ef2a06092b48c5f0c63e06668a76f43574f6c1faddf846"}}
```

**Goal:** JS/TS users retain every runtime function and its metrics when a
labeled block or a type annotation shares enclosing brackets; maintainers
receive a corrected, refactored, performance-qualified change merged into
current master and issue #524 closed with a summary after all checks pass.

**Beneficiary:** Lizard users affected by the remaining issue #524 regression.

**Source:** https://github.com/terryyin/lizard/issues/524 and the current human
instruction: "Please fix the remaining labeled-block and type-annotation
parsing issues, add regression tests, keep slowdown below 3%, and merge and
close #524 with a summary once all checks pass."

**Scope:** Correct label-versus-type-annotation ownership and annotation
termination/replay at enclosing runtime delimiters. Investigate the related
function-type field and object-type assertion cases, and correct confirmed
failures in this concept. Preserve valid type brackets, signatures, defaults,
ternaries, templates, extension metrics, CLI output and all previously fixed
#524 examples. Independently refactor the aggregate change, qualify affected
API/parameter/extension consumers and broad CLI below a strict paired 95%
upper slowdown bound of 3%, run full pytest/make and applicable hosted CI,
then merge the exact qualified head and close #524 with the related-fix summary.
The human authorizes implementation, publication, PR creation/merge and the
issue summary/closure. Retain failed observations and immutable timing inputs.

**Known expectations:**

- `const x = [() => { a: 1 }];` reports one anonymous function at line 1,
  matching 1.24.1; current master `da29d5d` reports none. A following function
  retains its own name, formals, span and CCN.
- A runtime labeled statement is not a type annotation. Its block continues
  through later decisions/statements and closes at its actual runtime brace.
- Type annotations do not swallow an unmatched `}`, `]` or `,` belonging to
  their enclosing runtime reader. Legitimate nested type syntax keeps its own
  brackets, commas and function-type arrows.
- Function-type class fields preserve following constructors/methods, and
  object-type assertions preserve expression-arrow and successor boundaries
  wherever these cases exercise the same ownership responsibility.
- The six original issue examples, all eleven authored PR #525 examples,
  seven JS-family suffixes and NS/nonstrict, CSV/`-o` and HTTP consumers retain
  their established behavior.
- No merge or issue closure before discriminating regression proof,
  independent refactoring, required <3% performance gates, full local checks
  and applicable exact-head hosted CI pass. Report any remaining #524 failure
  instead of representing partial correctness as closure.

**Remaining uncertainty:** Determine the smallest existing reader/signature
owners that distinguish runtime labels and own nested type syntax; confirm
the exact related field/assertion failures before choosing implementation.
No parallel runtime bracket counter, whole-stream buffer or issue-specific
switch. Preserve public metric meanings and unrelated working-tree changes.
