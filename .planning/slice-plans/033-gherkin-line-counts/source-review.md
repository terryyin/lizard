# Source-only review: SEED-055#gherkin-support

**Obtained:** 2026-11-11 before first planned delegation of slice 1.
**Reviewer launch:** fresh generalPurpose agent, no conversation inheritance;
input was only the seed Goal, Scope, Deferred promises, Required restrictions,
and Key examples from `seeds/SEED-055-gherkin-support.md#gherkin-support`.
No plan, product, proof, or slice scope was supplied. Reviewer agent id:
`4990c6dd-e217-4784-8adf-7943d4a74279`.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | Teams that maintain Gherkin specifications can use Lizard to see how long each written scenario and shared setup block is in their `.feature` files, using the existing API, reports, and NLOC warning thresholds. | slice 1 | observed: testGherkin.py TestGherkinSeedExample1 + testGherkinCLI.py API/CSV/warning |
| 2.1 | Discover `.feature` files through the normal language-reader registry, including directory analysis | slice 1 | observed: testGherkinCLI.test_directory_csv_and_language_filter directory CSV rows |
| 2.2 | Discover `.feature` files through the normal language-reader registry, including language selection with `-l gherkin` | slice 1 | observed: testGherkinCLI.test_directory_csv_and_language_filter `-l gherkin` filter |
| 3.1 | Report one Lizard function for each written `Background` | slice 1 | observed: TestGherkinSeedExample1 Background: common |
| 3.2 | Report one Lizard function for each written `Scenario` | slice 1 | observed: TestGherkinSeedExample1 Scenario: first |
| 3.3 | Report one Lizard function for each written `Scenario Outline` | slice 1 | observed: TestGherkinSeedExample1 Scenario Outline: second |
| 3.4 | Include the English alias `Example` | slice 1 | observed: test_rule_feature_tags_aliases_unnamed_duplicates_eof Example: inside |
| 3.5 | Include the English alias `Scenario Template` | slice 1 | observed: test_rule_feature_tags_aliases_unnamed_duplicates_eof Scenario Template: again |
| 4.1 | Recognize blocks inside `Rule` | slice 1 | observed: test_rule_feature_tags_aliases_unnamed_duplicates_eof Background/Example under Rule |
| 4.2 | Recognize blocks directly inside `Feature` | slice 1 | observed: TestGherkinSeedExample1 blocks under Feature |
| 5 | Neither `Feature` nor `Rule` is a function | slice 1 | observed: test_rule… names omit Feature/Rule; test_empty_and_container_only_have_no_functions |
| 6.1 | Count nonblank, non-comment source lines in each block, including its declaration | slice 1 | observed: SeedExample1 NLOC includes declaration lines |
| 6.2 | Count description lines in each block | slice 1 | observed: test_description_comment_blank_and_step_punctuation description counted |
| 6.3 | Count steps in each block | slice 1 | observed: SeedExample1 / ownership step lines in NLOC |
| 6.4 | Count data tables in each block | slice 1 | observed: SeedExample1 Examples table rows in outline NLOC 7 |
| 6.5 | Count doc-string delimiters and nonblank content in each block | slice 1 | observed: test_doc_strings_and_table_cells_are_opaque NLOC 9 |
| 6.6 | Count attached `Examples`/`Scenarios` sections and rows in each block | slice 1 | observed: SeedExample1 + test_tagged_examples + Scenarios under Template |
| 7 | Comments are lines whose first non-whitespace character is `#` outside a doc string | slice 1 | observed: test_description… excludes `# full line comment` |
| 8 | A `#` in step text, a table cell, or a doc string is data (not a comment) | slice 1 | observed: test_description… step `#`; test_doc_strings… `# literal` / `# also data` |
| 9 | A block ends before the next block or `Feature`/`Rule` container, or at EOF | slice 1 | observed: test_rule… before.end_line=3 before Rule; EOF without newline still reported |
| 10 | Report each block’s declaration line and last counted content line | slice 1 | observed: SeedExample1 ranges 2–3 / 5–7 / 9–15 |
| 11 | A new container’s description belongs to that container, not the previous block | slice 1 | observed: test_rule… before.nloc=2 excludes Rule description |
| 12 | Tags before a scenario declaration are outside that function and must not leak into its predecessor | slice 1 | observed: test_description… first.end_line=6; @keep outside both enlargements |
| 13 | Tags decorating an outline’s Examples section remain part of the outline’s contents | slice 1 | observed: test_tagged_examples_enlarge_only_their_outline outline NLOC 9 |
| 14 | Preserve the written block kind and title in its function name, including an unnamed `Background` | slice 1 | observed: test_rule… name `Background` without title |
| 15 | Distinct declarations with the same title remain distinct results at their own source locations | slice 1 | observed: test_rule… two `Scenario: before` distinct start_line |
| 16 | Every reported function has CCN `1` | slice 1 | observed: SeedExample1 / ownership / rule tests all CCN 1 |
| 17 | Prose, table values, and example-row counts do not introduce control flow | slice 1 | observed: CCN remains 1 with description/`if`/`&&`/tables/Examples |
| 18 | Background lines count only in their own function | slice 1 | observed: test_background_is_not_copied_into_scenarios NLOC [3,2,2] |
| 19 | Outline rows never expand into additional function results | slice 1 | observed: test_tagged_examples… len=2 (one outline + one scenario) |
| Q1 | English is the working boundary from the seed’s keywords | slice 1 | observed: docs/cli.rst English keywords working boundary; reader English patterns only |
| Q2 | This boundary adds no rejection rule for naturally handled input | slice 1 | observed: no unsupported-language rejection path in reader; empty/container-only yields [] |
| Q3 | Existing file-read errors retain normal Lizard handling | slice 1 | observed: no Gherkin-specific read path; uses ordinary analyze_file / CLI |
| Q4 | Token-count and parameter semantics receive no new metric contract | slice 1 | observed: no token/parameter assertions or docs contract added |
| R1 | Do not interpret text inside doc strings or data tables as block declarations or comments | slice 1 | observed: test_doc_strings_and_table_cells_are_opaque single function |
| R2 | CCN stays `1` | slice 1 | observed: all function CCN assertions = 1 |
| R3 | Shared setup is not copied into scenarios | slice 1 | observed: test_background_is_not_copied_into_scenarios |
| E1 | Feature + Background + Scenario + Outline with Examples yields three functions NLOC 2/3/7, CCN 1; directory CSV and `-l gherkin` expose the same | slice 1 | observed: SeedExample1 + test_directory_csv_and_language_filter |
| E2 | Description/blank/comment/step with `#`/`if`/`&&`; tag before next Scenario does not enlarge prior block | slice 1 | observed: test_description_comment_blank_and_step_punctuation |
| E3 | Doc string with header-looking and `#` literal data stays one function; triple quotes/backticks and table cells share opacity | slice 1 | observed: test_doc_strings_and_table_cells_are_opaque both delimiters |
| E4 | Rule after Scenario with unnamed Background and Example; EOF without newline; empty file has no functions | slice 1 | observed: test_rule… + test_empty_and_container_only_have_no_functions |
| E5 | Outline with two Examples (one tagged) remains one function; reused titles never merge | slice 1 | observed: test_tagged_examples… + duplicate titles in test_rule… |

## Deferred (not current commitments; retained for exclusion clarity)

| Id | Deferred text |
| --- | --- |
| D1 | Localized keywords |
| D2 | Executing specifications |
| D3 | Expanding outlines |
| D4 | Resolving step definitions |
| D5 | Syntax validation and recovery diagnostics |
| D6 | A new Gherkin-specific complexity metric |
