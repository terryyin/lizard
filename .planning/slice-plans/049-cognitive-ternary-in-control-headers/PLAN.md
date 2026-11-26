# Count ternaries in control headers for all brace languages

**Identity:** SEED-078#cognitive-ternary-in-control-headers

**Source:** [Count ternaries in control headers for all brace languages](../../seeds/SEED-078-pr-batch-follow-ups.md#cognitive-ternary-in-control-headers)

## Goal and scope

Users of `-Ecognitive` on a brace language that already counts ternaries see a ternary in a control header add its increment at that control structure's nesting level. The structure's nesting penalty starts at its body. A ternary inside the header ternary still nests. In a `for` header, `;` between the three clauses makes ternaries siblings. A `;` or `,` nested inside braces, an object, a class body, a template interpolation, or a JSX attribute expression does not end the enclosing header ternary.

[#684](https://github.com/terryyin/lizard/pull/684) does this only for plain JavaScript. Supersede it when this lands. Its `.js` scores are kept. Its `.jsx` and conditional-type assertions of 1 are the language gate this story removes.

Excluded: CCN, ND, NS, and which functions are reported. Languages whose profile does not count ternaries. Which `?` is a ternary, including Swift `a ? true : false` (already 0 in a return) and optional chaining. The regex-`?`-after-a-comment miscount ([#687](https://github.com/terryyin/lizard/pull/687)). Structures already ignored before the function body. A separate retune of ternaries that are already counted outside a header, beyond what the shared separator rule does while keeping pinned tests green.

Key examples are the seed's. Today, through `analyze_source_code`, the `if` plus `for` pair scores 2 in `.js`, `.ts`, `.jsx`, `.vue`, `.c`, `.java` and `.cs`. Swift `if a ? 1 : 0 { }` scores 1.

## Current decisions

- **One counter, not a JavaScript gate.** Ternary recognition already lives in `lizard_ext/cognitive_actions.py` (`_question`, `_resolve_pending_ternary`) and the brace counter. [#684](https://github.com/terryyin/lizard/pull/684) forks that path behind `language_names[0] == "javascript"`. Change the shared head path so a `?` in a head reaches that recognition. Do not add a language gate.
- **Nesting starts at the body.** Confirming a ternary while its enclosing control structure is still in its head adds the same amount that structure added, not a further nesting level. Letting `?` through `_in_head` without that adjustment scores the pair 6, the two-clause `for` 6, and Swift `if a ? 1 : 0 { }` 3.
- **Separators.** A `for`-clause `;` at the header's depth ends a header ternary. A `;` or `,` nested inside braces does not. A template interpolation's `}` is rewritten to `,` in `_route` before the pop sees it (`cognitive_counter.py`), at the ternary's brace depth, because `${` does not open a cognitive block. That rewrite is why `prefix ? \`${prefix}/${route}\` : useDefault ? fallback : route` scores 2 outside a header. Distinguish that end from a real comma so the header case can score 4, and so a real comma still separates siblings.
- **Pinned peer commas stay.** `test/test_extensions/testCognitivePeerCommaEndsTernary.py` promises that a real `,` at the ternary's bracket depth makes the next ternary a sibling, including `go(a ? \`${a}\` : b, b ? 1 : 2)` scoring 2. Those assertions stay.
- **Files over 250 lines.** `wc -l`: `cognitive_brace.py` 252, `cognitive_counter.py` 337, `cognitive_actions.py` 178, `test/test_extensions/testCognitiveComplexity.py` 1575. Slice 2 edits the counter, so slice 1 moves per-function token holding out of it first. The behavior edits head advancement, so that move is in slice 1 as well. New tests go in `test/test_extensions/testCognitiveControlHeaders.py` and do not edit the 1575-line file. `cognitive_actions.py` may change and must stay at or under 250 lines. Do not leave either oversized file over 250 lines.
- **No Accepted ADR covers this.** Cognitive-complexity scoring has no Accepted ADR. ADR 0001 defines nesting depth, which this story does not change. There is no North Star topic to follow.
- **Local gate.** [AGENTS.md](../../../AGENTS.md) requires `nix develop -c python -m pytest` and `nix develop -c make` before the change is complete. Slice 2 runs both. Focused proof for both slices is `nix develop -c python -m pytest test/test_extensions/testCognitiveComplexity.py test/test_extensions/testCognitivePeerCommaEndsTernary.py test/test_extensions/testCognitiveDefaults.py test/test_extensions/testCognitiveComplexityDict.py`. Slice 2 adds `test/test_extensions/testCognitiveControlHeaders.py`.

## Decisive premises

Observed in this workspace on 2026-11-27 at `e5df1994`, with `nix develop -c python`, through `FileAnalyzer(get_extensions([Cognitive()])).analyze_source_code`. The scratch edit was reverted before the seed was recorded; `git diff` showed no product change.

| Premise and operation | Observation and result |
| --- | --- |
| The header swallows a ternary. The pair scores 2, not 4. Swift's brace header scores 1. | The `if (a ? b : 0)` plus `for` pair scored 2 for `.js`, `.ts`, `.jsx`, `.vue`, `.c`, `.java` and `.cs`. Swift `if a ? 1 : 0 { }` scored 1, and `return a ? 1 : 0` scored 1. `return a ? true : false` scored 0. A nested `if (b ? 1 : 0)` scored 3. The two-clause `for` scored 1. The JSX element `if` scored 1 and the same expression in a `return` scored 3. |
| The existing action nests a header ternary under the open structure. Existing cognitive tests do not pin the swallow. | Scratch: `_in_head` returned false for `?` in a parenthesized or brace head. The pair scored 6, the two-clause `for` scored 6, the JSX `if` scored 6, the template `if` scored 5, the object `if` scored 5, the conditional-type `if` scored 3, Swift `if a ? 1 : 0 { }` scored 3. `if (a?.b)` stayed 1. `if (a) x = b ? c : d` stayed 3. `pytest -q test/test_extensions/testCognitiveComplexity.py` passed 150. |
| A conditional type is already a counted `?` outside a header. Optional chaining is not. | `return input as number` scored 0. `return input as (T extends string ? boolean : number)` scored 1. The same cast in an `if` scored 1. |
| A template end currently behaves as a comma. A real peer comma is pinned. | The template expression in a `return` scored 2. `test_javascript_template_form_peer_arguments` asserts 2 for a template ternary followed by a real comma and a second ternary. |

## Ordered slices

### 1. Head advancement and token holding sit in their own modules

Type: Structure

Status: planned

Proof: `nix develop -c python -m pytest` on the cognitive files named above, except the new header file, which does not exist yet. Scores stay as they are. `wc -l lizard_ext/cognitive_brace.py lizard_ext/cognitive_counter.py` prints 250 or below for each. This enables slice 2 to change header recognition and the interpolation-end token in files that are under the line limit.

Change: `cognitive_brace.py` is 252 lines. Move `_in_head`, `_start_body` and `_open_braced_body` into `lizard_ext/cognitive_heads.py`, a mixin `_BraceCounter` uses the way it already uses `_BraceActions`. Only `_BraceCounter` calls them.

`cognitive_counter.py` is 337 lines. Move per-function token holding and state selection into `lizard_ext/cognitive_candidates.py`: `_Candidate`, `_settle`, `_keeps_tokens`, `_ended`, `_involved`, `_hold`, `_merge`, `_release`, `_switch_function` and `_in_body`. Bracket routing (`_route`) and the increment helpers stay, so slice 2 can distinguish a template end from a comma without editing a file over 250 lines. Callers are `_Counter.process` and `_Counter.finish`.

### 2. A header ternary adds its increment at the structure's level

Type: Behavior

Status: planned

Proof: the new tests below, then the focused cognitive files, then `nix develop -c python -m pytest` and `nix develop -c make`. The new tests fail before this slice's production change and pass after it. `cognitive_brace.py`, `cognitive_counter.py` and `cognitive_actions.py` stay at or under 250 lines.

Behavior: analyzing a function through `analyze_source_code` with the cognitive extension, a ternary in a control header adds the increment of that control structure, in every brace language that already counts ternaries. Existing peer-comma scores stay. `#684` is superseded by this behavior rather than merged.

| Promise | Proof owned by this slice |
| --- | --- |
| `if` plus a `for`-header ternary scores 4, not 2 | One test, parameterized by `.js`, `.ts`, `.jsx`, `.vue`, `.c`, `.java` and `.cs`, asserts 4 for `if (a ? b : 0) {}` followed by one ternary in a `for` header. |
| Nesting follows the structure, and a nested ternary still nests | The nested `if (b ? 1 : 0)` asserts 5. `if (options ? (options.exact ? options.enabled : fallback) : fallback)` asserts 4. |
| `for`-clause semicolons make siblings; nested separators do not | The two-clause `for` asserts 3. The `.js` cases from #684 keep their scores: class-field semicolon in a `for` header 4, object comma 4, template interpolation 4, class-field initializers 5, object-property ternaries 6, `while` header 2. |
| JSX attribute syntax does not hide the ternary | The `<Card title={title} />` `if` in `.jsx` asserts 4. |
| Swift's brace header counts a ternary the counter already counts | `if a ? 1 : 0 { }` asserts 2. `return a ? true : false` stays 0. |
| A `?` that is not a new kind of ternary | `if (a?.b)` asserts 1. The conditional-type `if` asserts 2. The conditional-type `return` stays 1. |
| Real peer commas stay siblings | `testCognitivePeerCommaEndsTernary.py` passes unchanged, including the template-then-comma case at 2. |
| Body nesting stays | `test_ternary_nests_and_is_nested` stays 3 for `if (a) x = b ? c : d`. |

## Plan review

Refinement was not needed. Slice 1 is the file-size split the behavior has to edit, and it keeps current scores. Slice 2 is one header-ternary rule, including the separators that make the #684 `.js` scores true, proved through `analyze_source_code`. The naive "let `?` through" result is already observed, so the nesting adjustment is part of this behavior rather than a later slice. This review found no remaining concern.
