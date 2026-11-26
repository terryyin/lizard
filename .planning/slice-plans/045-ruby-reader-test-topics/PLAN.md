# Split the Ruby reader tests by topic

**Identity:** SEED-076#ruby-reader-test-topics

**Source:** [Split the Ruby reader tests by topic](../../seeds/SEED-076-ruby-reader-test-topics.md#ruby-reader-test-topics), deferred from SEED-075#ruby-percent-literal-context at its file-size stop ([DD-490](../../../DearDough.md#dd-490--planned-proof-home-was-already-oversized-without-a-reorganization-decision)).

## Goal and scope

Maintainers changing the Ruby reader find and extend its tests in topic files of at most 250 lines, the [post-change refactor limit](../../../.claude/skills/dough-post-change-refactor/references/refactor-checks.md). Test-only: no reader or other product change, no test added, dropped, or rewritten beyond moving it and its imports. `testRubyRegex.py`, `testRubyParser.py`, `testRubyControlFlow.py` and `test_lizard_ruby_noncode.py` stay unchanged.

## Decisive premises

| Premise | Consuming operation | Observation | Result |
| --- | --- | --- | --- |
| Baseline suite | `nix --extra-experimental-features 'nix-command flakes' develop -c python -m pytest test/test_languages/testRuby.py -q` (and `--collect-only`) at `3d73019f` | run in this workspace | 54 collected, 54 passed |
| Class sizes | splitting by class | `grep -n '^class Test\|    def test_'` on `testRuby.py` at `3d73019f` | tokenizing 12–101, CCN+def 102–155, comment continuation 156–175, keyword method names 176–288, blocks that end a function early 289–708 (~420 lines, 17 tests), percent literals 709–822. Only the blocks class exceeds 250 alone. |
| Discovery | pytest and `make` | `setup.cfg` `[tool:pytest] python_files = test*`; `test/__init__.py` and `test/test_languages/__init__.py` are empty; no module imports `testRuby` | New files are collected only when named `test*`; no registration to update |
| Shared helpers | moved tests | `testRuby.py` imports `analyze_file, FileAnalyzer, get_extensions`, `RubyReader, MyToken`, `pickle`, `re`, and defines `get_ruby_function_list`; sibling Ruby files each define their own copy of that helper | Each new file carries the imports and helper it uses, as the sibling files already do |

## Current decisions

- Name files `testRuby<Topic>.py` beside the existing ones. Suggested grouping, each well under 250 lines: Tokenizing (tokenizing class); `testRuby.py` keeps CCN and def parsing plus comment line continuation; KeywordMethodNames; BlockEnds (control-flow blocks: `case`/`until`, loops, `if` expressions and modifiers, tests at lines 293–489); EndlessMethods (endless, setter/operator and generated method definitions, 490–647); RSpecExamples (648–708); PercentLiterals. Execution may regroup within the 250-line limit as long as each file stays one topic.
- Classes split from the blocks class keep their test method names; new class names describe their sub-topic.
- Local gate: full `nix … develop -c make` per AGENTS.md (tests plus pep8/pylint, which the new files enter).

## Ordered slices

### 1. Ruby reader tests live in topic files within the size limit

Type: Structure

Status: planned

Internal change: move every test in `test/test_languages/testRuby.py` into topic files as above, with unchanged test bodies. Unchanged external behavior: the reader is untouched, and the same 54 Ruby test methods run and pass. This directly owns the evidenced test-suite weakness from DD-490 (an oversized proof home that stalled a refactor gate); it enables no new Behavior.

Proof:
- Before moving, save `pytest test/test_languages/testRuby.py --collect-only -q` test names (method names with their class) for comparison; after moving, collect the new files and the kept `testRuby.py` and compare method names: same 54, none duplicated, none missing.
- `wc -l test/test_languages/testRuby*.py`: every file at most 250 lines.
- `nix --extra-experimental-features 'nix-command flakes' develop -c python -m pytest test/test_languages` passes, then `nix --extra-experimental-features 'nix-command flakes' develop -c make`.

## Concern evidence

Slice-plan refinement not needed: one mechanical Structure slice with one proof loop and no design choices beyond grouping. No remaining slice-specific concern identified.
