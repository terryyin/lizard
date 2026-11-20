# Nesting depth after a parenthesized assignment without `;`

**Identity:** SEED-068#js-asi-parenthesized-assignment
**Source:** [Parenthesized assignment without `;` hides the next `if`](../../seeds/SEED-068-js-asi-parenthesized-assignment.md#js-asi-parenthesized-assignment)

## Goal and scope

In no-semicolon JavaScript/TypeScript, `name = (…)` ending at a line break
must not hide the nesting level (ND) of a structure keyword that starts the
next line. The seed owns the contract and key examples. CCN, NS and token
counts stay as today. Arrow functions whose parameters span lines keep being
functions. Other ASI discrepancies are deferred.

## Existing solution and decisions

The reader treats `name = (` as an optimistic arrow-function signature: it
pushes a function at `(` (`JavaScriptFunctionsMixin._function`,
`lizard_languages/javascript_functions.py`) and abandons it when the token
after `)` is not `{`, `=>` or `:` (`_expecting_func_opening_bracket` →
`_abandon_function`). `_abandon_function` already carries the expression's
CCN, tokens and NLOC to the enclosing function, but not ND state.

The ND extension (`lizard_ext/lizardnd.py`) runs before the reader on each
token and credits `open_level()` to `context.current_function`. After `;`,
the reader abandons the optimistic function at `;`, so the following `if` is
credited to the enclosing function. Without `;`, the `if` itself is the token
that triggers abandonment, so ND has already credited its level to the
optimistic function and that level is discarded. The extension already knows
the statement ends there (`ImpliedSemicolons.before` in
`lizard_languages/javascript_statements.py`), but the reader does not end
the optimistic signature at that point.

Fix this with one rule: the level belongs to the function that is current once
the statement boundary is resolved. Either end the optimistic signature at
the implied statement end before the extensions credit the next token, or carry
ND state across abandonment as CCN/tokens/NLOC already are. The executor
chooses whichever keeps one concept in one place. Do not add a special case
just for `if`, and do not add a second statement-end recognizer next to
`ImpliedSemicolons`. A line break before `=>` is not valid JavaScript, so ending
the signature at a line break after `)` cannot lose a real arrow.

No relevant Accepted ADR beyond
[ADR-0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
naming (ND, Function, Language reader). No North Star topic needed.

File-size note: `test/testNestingDepth.py` (521 lines) and
`lizard_languages/javascript_functions.py` (302 lines) are already over the
250-line refactor check. Put the new proof in a cohesive new test module (or
split the JavaScript without-semicolons class out as part of this change)
instead of growing `testNestingDepth.py`. If the reader file is edited,
post-change refactoring applies its size rule.

## Outside-in proof ownership

All promises belong to Slice 1. Tests go through
`FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code`, the same
helper form as `TestJavaScriptNestingDepthWithoutSemicolons`, with real
parsing and no mocks.

| Source promise | Observable proof |
| --- | --- |
| Minimal: `r = (m < 2)` + line break + `if (r) w()` | max ND 1, same as with `;` |
| Nested structures keep their full depth | `r = (m < 2)` + `if (r) { if (m) { w() } }` → ND 2 |
| Other structure keywords and TypeScript | `while (r) w()` after it → ND 1; same `if` source as `a.ts` → ND 1 |
| js-yaml shape | `handleYamlDirective` excerpt with `state.checkLineBreaks = (minor < 2)` then `if (minor !== 1 && minor !== 2) {…}` → ND 2, CCN 7 |
| Real arrows unaffected | `x = (a,\n b) => { if (a) w() }` stays a function `x` with ND 1; enclosing ND 0 |
| CCN/NS unchanged | the minimal example keeps CCN 2 and NS 1 (`-ENS`) |

The new ND tests must fail on the base revision (see premises).

## Decisive premises and observations

Observed 2026-11-20 in this preparation workspace at
`51e33ddf787d871ebb295edcde8728e874fdf21d`, via `nix develop -c python`.

| Premise / consuming operation | Form, profile and observed result | Unverified remainder |
| --- | --- | --- |
| Symptom exists in the proof's form | Scratch script calling `FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code`: without `;` → minimal 0, `while` 0, `.ts` 0, nested 1; with `;` → 1, 1, 1, 2 | None. Proof examples discriminate |
| Symptom only when the structure keyword immediately follows | `python lizard.py -ENS -END`: `r = (m < 2)` / `w()` / `if (r) w()` → ND 1 (correct today); CCN and NS equal with and without `;` in all probes | None |
| js-yaml shape reproduces | `python lizard.py -ENS -END` on a `handleYamlDirective` excerpt: without `;` CCN 7 NS 1 ND 1; with `;` CCN 7 NS 1 ND 2 | The actual npm package was not re-run; the excerpt reproduces the function's shape |
| Multi-line arrow parameters stay a function | Same scratch script: `x = (a,\n b) => { if (a) w() }` → functions `x` ND 1, `g` ND 0 | Preserved behavior; proof keeps it |
| Mechanism: ND credits the level before the reader abandons the optimistic function | Code reading of `lizardnd.py` `__call__`/`open_level` and `javascript_functions.py` `_expecting_func_opening_bracket`/`_abandon_function`, consistent with the "plain statement between" probe | The fix's exact seam is the executor's choice (see decisions) |

No paid, credentialed or external premise; no early probe slice needed.

## Ordered slices

### 1. Structure after a parenthesized assignment keeps its nesting level

Type: Behavior
Status: planned

**Pre-condition:** A no-semicolon JS/TS function contains `name = (expr)`
ending at a line break, then a structure keyword on the next line.
**Trigger:** The user analyzes it with ND (`-END`) enabled.
**Result:** ND equals the semicolon version's ND. CCN, NS and arrow-function
detection are unchanged.

Write the failing observable examples above first. Then apply the one common
rule from "Existing solution and decisions", and refactor after the change.

**Local verification:**

```
nix develop -c python -m pytest test/testNestingDepth.py <new test module> test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/testJavaScriptNestedStructures.py
nix develop -c make
```

`AGENTS.md` requires the full suite before completion; `make` runs it with
style checks. Hosted CI stays with execution's CI ownership.

**Sizing:** One rule at one reader/extension seam, with one proof loop. If the
fix needs changes beyond the optimistic-signature/ND seam, such as reworking
the shared implied-semicolon scan (SEED-069's area), stop and replan.

## Current decisions

- Correct ND by giving the level to the function that is current once the
  statement boundary is resolved, not by a keyword-specific special case.
- Proof uses inline sources. No external package fixture.
- [SEED-069's plan](../034-cheaper-implied-semicolon-scan/PLAN.md) changes how
  ND/NS get implied statement ends, which may touch the same extension code.
  Neither story depends on the other. Whichever lands second rebases onto the
  other and keeps both proofs green.

## Concern evidence

Slice-plan refinement not needed: one Behavior slice with one cohesive proof
loop and no boundary concern. This review found no remaining slice-specific
concerns.
