# Keep Perl and Ruby documentation from acting as Lizard directives

**Identity:** SEED-075#source-documentation-directives

**Source:** [Keep Perl and Ruby documentation from acting as Lizard directives](../../seeds/SEED-075-reader-corrections.md#source-documentation-directives). The original proposals are [#636](https://github.com/terryyin/lizard/pull/636) at `7e89c51a83bdc2254ac832872e01acf5d97c4430`, [#642](https://github.com/terryyin/lizard/pull/642) at `0bf972c66cce3e4b6cb44d8fa450ca2b5a303ddb`.

## Goal and scope

Perl and Ruby users get metrics for executable code only. Embedded documentation and trailing data sections add no phantom functions, decisions or code lines. Text in them that resembles a Lizard directive never hides, ends or forgives the real code around it. This delivers the intent of both proposals and follows their direction.

Excluded: other languages, new directive syntax, changes to how actual comments are recognized (including Perl's existing `#lizard forgive` handling), heredocs and other string forms. The story's key examples are the acceptance examples.

## Decisive premises

Observed at `1660636f` (this workspace, which matches master on the reader paths) and on exported trees of each proposal's exact revision. Command form: `nix develop -c env PYTHONPATH=<tree> python <script>` calling `analyze_file.analyze_source_code`, and `nix develop -c python lizard.py -C 3 -w <files>` for warnings. These are the same API and CLI routes the slice's proof uses.

| Premise | Consuming operation | Result |
| --- | --- | --- |
| The proposals lose the following function | `analyze_source_code` on `=head1 NOTES`/`GENERATED CODE`/`=cut`/`sub real { if ($x) { 1 } }` and the Ruby `=begin`/`GENERATED CODE`/`=end`/`def real … if …` | #636 Perl: NLOC 0, no functions. #642 Ruby: NLOC 0, no functions. Master: `real` at line 4, CCN 2 in both languages. |
| Master lacks the proposals' intent, so the proof discriminates | `analyze_source_code` on `=pod`/`sub fake …`/`=cut`/`sub real …` and Ruby `=begin example` with a fake `def` | Master Perl reports `fake` and `real` with NLOC 4. #636 reports only `real` at line 4 with NLOC 1. Master Ruby reports `real` with NLOC 9. #642 reports NLOC 3. |
| Documentation forgiveness | CLI `-C 3 -w` on a POD or `=begin` section holding `#lizard forgive`, then a CCN 4 function | Master and #636: Ruby `real` is forgiven, so there is no warning. #642: Ruby warns. Perl warns on all three revisions. |
| Actual comments keep their directives | `# GENERATED CODE` between two functions. CLI on `#lizard forgive` before a CCN 4 function | Analysis stops after the first function on all revisions. The Ruby comment forgives. A Perl comment does not forgive on master; that stays as it is. |

No premise remains open.

## Current decisions

Preserve the shared comment counter and an explicit distinction between language comments, which carry analyzer directives, and opaque document or data tokens, which keep line positions but carry no directive. Routing documentation through the directive handler as a `#`-prefixed comment is the observed cause; do not use that route. Keep the Perl documentation recognition separate from its function state machine when organizing the already oversized reader file. Use Lizard's established
[analysis vocabulary](../../../docs/adrs/0001-ubiquitous-language-accepted.md).

## Ordered slices

### 1. Keep Perl and Ruby documentation from acting as Lizard directives

Type: Behavior

Status: planned

Proof: The key examples through `analyze_file.analyze_source_code` with real source fixtures, plus a CLI warning check for the forgiveness example. Each new GENERATED CODE and forgiveness assertion fails on its proposal and passes on the corrected revision. Each phantom and NLOC assertion fails on master.

Behavior: Perl or Ruby source with documentation or a data section, followed or surrounded by real code → analysis reports only the real functions, with their original lines and CCN. Directive-like text inside the documentation has no effect. Actual comments behave as on master.

Bring in both proposals' tests and their non-code recognition. Compare master, the exact proposals and the corrected revision. Run `nix develop -c python -m pytest test/test_languages/testPerl.py test/test_languages/test_perl_func_names.py test/test_languages/testRuby*.py` and the new non-code tests, then full pytest and `nix develop -c make`, as AGENTS.md requires before completion. Pair affected API and broad CLI timing on the exact corrected production bytes, as the `lizard-process-pr` qualification requires. Require green hosted checks before any merge.

## Plan review

Refinement was not needed. There is one cohesive Behavior slice: the two readers share one defect, one rule and one proof route, and each proposal already supplies its reader's recognition. No slice-specific concern remains.
