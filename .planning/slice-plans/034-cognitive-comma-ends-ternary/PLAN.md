# A comma ends a ternary in cognitive complexity

**Identity:** SEED-067#cognitive-comma-ends-ternary
**Source:** [A comma ends a ternary in cognitive complexity](../../seeds/SEED-067-cognitive-comma-ends-ternary.md#cognitive-comma-ends-ternary)

## Goal and scope

Users of `-Ecognitive` get sibling ternary scores when a `,` separates peer
arguments, elements, or expressions: the open ternary ends there, so the next
ternary is not nested inside it. Structurally nested ternaries still take the
nesting increment. Every language whose cognitive profile counts ternaries
shares this counter behavior. No change to when a ternary starts, to the base
ternary increment, or to CCN / other metrics.

## Existing solution and decisions

PFE: the responsibility already lives in the brace-based cognitive counter.
`_BraceCounter._pop_expression_frames` ends ternary frames when bracket depth
drops below the frame's open depth; `,` is already in `_EXPRESSION_BREAKERS`
but that path only clears logical-operator sequences. Change that existing
ternary end-token handling so a peer `,` at the ternary frame's bracket depth
ends the frame — one coherent rule with closing brackets, not a second
nesting model. Indentation-based Python ternaries (`x if c else y`) are out of
the reported `?`/`:` path; this story does not add a parallel comma rule there.

No consequential ADR or North Star topic: follow existing cognitive-complexity
ownership in `lizard_ext/cognitive_*.py` and ADR-0001 domain terms
(Function, complexity metrics).

## Outside-in proof ownership

All promises belong to Slice 1. Proof uses `FileAnalyzer(get_extensions([Cognitive()])).analyze_source_code`
(or the existing `cogc` / language helpers in `test/test_extensions/testCognitiveComplexity.py`);
no mocked parsing.

| Source promise | Observable proof |
| --- | --- |
| Peer argument ternaries score as siblings | `function f(a, b) { go(a ? x : b, b ? 1 : 2) }` → cognitive complexity 2 |
| Template form from PR #576 | ``function f(a, b) { go(a ? `${a}` : b, b ? 1 : 2) }`` → 2 |
| Peer array elements | `function f(a, b) { const xs = [a ? 1 : 2, b ? 3 : 4]; }` → 2 |
| Nested ternary still nests | `function f(a, b) { x = a ? (b ? 1 : 2) : 3 }` → 3 (unchanged) |
| Shared counter for ternary-counting C-like profiles | Equivalent sibling-argument case in C++ (or another ternary profile) → 2 |

## Decisive premises and observations

Observations were made on 2026-11-20 in this preparation workspace at
`42aaba117a39e92674b79f8d3e734e53024787de`, via `.venv/bin/python` importing
`FileAnalyzer` / `Cognitive` the same way the suite helpers do.

| Premise / consuming operation | Form, profile and observed result | Unverified remainder |
| --- | --- | --- |
| Sibling argument ternaries currently over-nest | `analyze_source_code('a.js', 'function f(a, b) { go(a ? x : b, b ? 1 : 2) }')` → cognitive complexity **3** | Settled bug baseline |
| Template sibling form currently over-nests | same entry → **3** | Settled |
| Array peer elements currently over-nest | same entry → **3** | Settled |
| Nested ternary already scores correctly | `x = a ? (b ? 1 : 2) : 3` → **3** | Settled preserve target |
| C++ sibling args share the bug | `void f(int a, int b) { go(a ? 1 : 2, b ? 3 : 4); }` → **3** | Settled shared-counter premise |
| End-token home is the brace counter | Code read: `_pop_expression_frames` on `)`/`]`; `,` in `_EXPRESSION_BREAKERS` clears logical only | Approach premise; no further host check |

No early probe or pre-Take human decision.

## Ordered slices

### 1. Peer commas end open ternaries for `-Ecognitive`

Type: Behavior
Status: planned

**Pre-condition:** Source with two or more `?`/`:` ternaries separated by `,` as
peers in a call, list, or equivalent, and a nested ternary control.
**Trigger:** Analyze with the cognitive extension (`-Ecognitive` / `Cognitive`).
**Result:** Peer ternaries score 2 (siblings); the nested control still scores 3;
CCN and other metrics for those snippets are unchanged.

Extend the brace counter's ternary end tokens so `,` at the open ternary's
bracket depth pops the frame (same ownership as `_pop_expression_frames`).
Add discriminating cases to `test/test_extensions/testCognitiveComplexity.py`
covering the source examples (JS siblings, template form, array peers, nested
preserve, and one non-JS ternary profile). Focused verification:
`nix develop -c python -m pytest -q test/test_extensions/testCognitiveComplexity.py`.

## Refinement and concerns

Slice-plan refinement: not needed — one Behavior slice, one end-token rule,
proof mapped, premises settled, under the eight-slice limit.
Remaining slice-specific concerns: none identified.
