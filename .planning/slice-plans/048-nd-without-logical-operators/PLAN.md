# Measure ND from control structures only

**Identity:** SEED-078#nd-without-logical-operators

**Source:** [Measure ND from control structures only](../../seeds/SEED-078-pr-batch-follow-ups.md#nd-without-logical-operators)

## Goal and scope

Users who gate on nesting depth (`-END`, `-N`) get ND that counts only control structures, as [ADR 0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md) defines it. `&&` and `||` stop opening a level, inside or outside a condition, in every language whose ND counts them today. The CHANGELOG tells users that ND values drop.

Excluded: CCN, NS (`-ENS`) and cognitive complexity; the ternary `?`/`?:`, which keeps its ND level; word operators such as Python's `and`/`or`, which add no ND today.

Key examples, from the seed:

- #693 in C: `for(;;){ if (a) { x(); } }` and `for(;;){ if (a && b) { x(); } }` both report ND 2. The second reports 3 today.
- `int c = a && b;` alone in a function reports ND 0. It reports 1 today.
- `if (a || b && c)` inside a loop reports the same ND as `if (a)` there.
- `-ENS` results stay the same.

## Current decisions

- **Where the rule lives.** ND's only logical-operator rule is in `lizard_ext/lizardnd.py`: the `&&`/`||` branch, the `in_condition`/`logical_operator_added` state that serves only that branch, and `&&`/`||` in the default `loops` set. `GroovyReader.loops` in `lizard_languages/groovy.py` also lists them. Remove them in those places. Other readers' `_logical_operators` feed CCN, not ND, and stay. Keep `condition_depth`, because it decides where a statement ends.
- **Tests that use `&&` as a probe.** Many ND tests use `&&` outside a condition as a cheap way to open a level. They then check that a statement boundary closes that level, or that it does not. These tests keep the boundary they promise. Slice 1 swaps their probe for a ternary `?`, which still opens a level, or for a control structure, so the expected ND stays the same. Tests whose promise is the logical-operator count change their expected values in slice 2.
- **Release note.** CHANGELOG entries are normally written at release time. This change goes under a new `## Unreleased` heading at the top of `CHANGELOG.md`, with a `### Changes` entry that says ND values and thresholds drop. `setup.py` reads the version from the first heading that starts with a version number, so `## Unreleased` does not affect it (see premises).
- **Local gate.** [AGENTS.md](../../../AGENTS.md) requires `nix develop -c python -m pytest` and `nix develop -c make` before the change is complete. Focused runs use `test/testNestingDepth.py` and the other files named under premises.
- No Accepted ADR changes. The change brings ND into line with ADR 0001's definition.

## Decisive premises

Observed in this preparation workspace on 2026-11-27 at `bc4ebc01`, under `nix --extra-experimental-features 'nix-command flakes' develop -c`. Scratch edits were reverted.

| Premise and operation | Observation and result |
| --- | --- |
| The CLI reproduces the #693 symptom and the outside-condition case. | `python lizard.py -END -ENS t.c` on the three key-example functions gave `plain` ND 2 NS 2, `logical` ND 3 NS 2, and `outside` (`int c = a && b;`) ND 1 NS 0. |
| Groovy's own `loops` set makes `&&` add ND today. | `python lizard.py -END t.groovy`, with each function on separate lines: `for (x in y) { if (a) { f() } }` gave ND 2, and the same with `if (a && b)` gave ND 3. |
| Neutralizing the `&&`/`||` branch is enough to remove the contribution, and the failing tests are its consumers. | Scratch edit: the branch body became `pass`. `python -m pytest -q` gave 20 failed and 4074 passed. 17 were in `test/testNestingDepth.py` (C++ value tests such as `test_one_function_nd_with_and` and the `..._bug_426` operator tests; JavaScript ASI/statement-boundary probe tests such as `test_object_literals_stay_in_their_statement` and `test_line_break_inside_a_declaration`). The other 3 were `test/test_js_asi_parenthesized_assignment_nd.py::test_js_yaml_handle_yaml_directive_shape`, `test/test_languages/testJavaScript.py::test_key_named_like_a_keyword_is_no_structure` and `test/test_languages/testPHPMemberNames.py::test_comparison_after_a_member_named_function`. No NS, CCN or cognitive test failed. |
| `## Unreleased` above `## 1.26.1` leaves the package version unchanged. | Scratch `CHANGELOG.md` with that heading: `python setup.py --version` printed `1.26.1`. `lizard_ext/version.py` stayed unchanged. |

## Ordered slices

### 1. Boundary tests probe ND with structures that keep their level

Type: Structure

Status: planned

Proof: on current code, the rewritten tests pass with their original expected ND. With the scratch `pass` edit from the premises applied, none of them fails. Revert the scratch edit before committing.

Change: among the 20 consumers, find each test whose promise is a statement or structure boundary, a member name, or an arrow body rather than logical-operator counting. Replace its `&&`/`||` probe with a ternary `?` or a control structure that gives the same depth. Do not change the expected value. This enables slice 2: after slice 1, the only tests that slice 2 changes are tests about logical-operator counting.

### 2. `&&` and `||` add no nesting depth

Type: Behavior

Status: planned

Proof: see the table. Run the full `pytest` suite and `make`.

Behavior: a function with `&&`/`||` in a condition or in a plain expression, in C-family languages and Groovy, is analyzed through `analyze_file` or the CLI. Its ND counts only control structures. NS is unchanged. `CHANGELOG.md` says that ND no longer counts logical operators and that values and `-N` thresholds may drop.

| Promise | Proof owned by this slice |
| --- | --- |
| #693: `if (a && b)` inside a loop has the same ND as `if (a)` | A new C test through `analyze_file` asserts ND 2 for both functions. It fails before the change (3). |
| A logical operator outside a condition adds no ND | A C test asserts ND 0 for `int c = a && b;`. The existing `test_one_function_nd_with_non_r_value_ref_in_body` changes from 1 to 0. |
| Mixed and repeated operators add nothing | The existing mixed/multiple/`..._bug_426` tests change their expected ND to the depth of the structures alone. |
| Groovy follows the same rule | A Groovy test asserts that `if (a && b)` in a loop has the same ND as `if (a)`. |
| NS is unchanged | A CLI test with `-END -ENS` on the #693 functions asserts NS 2 for both. Existing NS tests pass unchanged. |
| Release note | `CHANGELOG.md` has the `## Unreleased` entry. `python setup.py --version` still prints `1.26.1`. |

## Plan review

Refinement was not needed: the plan has two slices. Slice 1 is a preparatory Structure slice that keeps the boundary promises green before slice 2 changes the metric. Slice 2 is one cohesive Behavior slice. This review found no remaining concern for either slice.
