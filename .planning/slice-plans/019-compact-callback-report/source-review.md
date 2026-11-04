# Source review: Show a compact JavaScript complexity report

Launch: fresh general-purpose subagent (no conversation inheritance) on 2026-11-04, given only the selected story's Goal, Scope and Key examples from `.planning/seeds/SEED-030-compact-javascript-callback-report.md#compact-callback-report`, told to read no files and change nothing. Its numbered return is reproduced below.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| G1 | A JavaScript developer scanning Lizard's CLI report can opt into a compact table that leaves out rows for trivial nested callbacks. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it compact rows == ['sum'] |
| G2 | The purpose is to make the functions that need attention easier to find. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_nested_callback_with_decisions and test_compact_keeps_warned_callback_and_exit_status keep rows needing attention |
| G3 | No function's measurements change. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_analysis_results_keep_trivial_nested_callback; compact and default summaries equal after 'file analyzed.' |
| G4 | The default report does not change. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it default rows list both; full suite (testOutput) unchanged |
| S1 | A long-only --compact option changes the default human-readable text report. | slice 1 | observed: lizard_core/options.py --compact (store_true, no short alias); docs/cli.rst listing |
| S1.1 | The option is long-form only. There is no short alias. | slice 1 | observed: lizard_core/options.py add_argument('--compact') with no short flag |
| S1.2 | Its effect is on the function table of the default human-readable text report. | slice 1 | observed: lizard_core/text_output.py print_result passes shown filter only to print_and_save_modules function rows |
| S2.1 | Left out only when the function is named (anonymous). | slice 1 | observed: lizard_core/text_output.py is_trivial_nested_callback; test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_named_and_top_level_callbacks keeps g |
| S2.2 | Left out only when defined inside another function, not at file/global scope. | slice 1 | observed: lizard_core/file_info_builder.py push_new_function defined_in_function; test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_named_and_top_level_callbacks keeps top-level (anonymous) |
| S2.3 | Left out only when it has CCN 1. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_nested_callback_with_decisions keeps CCN 2 reducer |
| S2.4 | Left out only when not reported as a warning under current thresholds, options and whitelist. | slice 1 | observed: lizard_core/warning_filter.py warning_policy reused by print_result; test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_warned_callback_and_exit_status |
| S3 | The enclosing function's row is unchanged. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it keeps sum row |
| S4 | Each retained row keeps its own metrics. | slice 1 | observed: lizard_core/text_output.py print_and_save_modules prints retained rows through unchanged scheme.function_info |
| S5 | Each retained row keeps its own source location. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_warned_callback_and_exit_status asserts (anonymous)@1-1@code.js |
| S6.1 | The file table still covers all analyzed functions. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it asserts function_cnt 2 for code.js |
| S6.2 | The warning section still covers all analyzed functions. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it compact == default after 'file analyzed.'; test_compact_keeps_warned_callback_and_exit_status warnings list (anonymous) |
| S6.3 | Totals still cover all analyzed functions. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it compact == default after 'file analyzed.' |
| S6.4 | Function counts still cover all analyzed functions. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it function_cnt 2 and equal totals |
| S6.5 | Averages still cover all analyzed functions. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it compact == default after 'file analyzed.' |
| S7 | The warning exit status is unchanged by --compact. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_warned_callback_and_exit_status returncode 1; others 0 |
| S8 | Without --compact, every output is unchanged. | slice 1 | observed: full suite nix develop -c make: existing output tests pass; whitelist read timing kept when compact is off (text_output.print_result) |
| S9.1 | With --compact, XML output is unchanged. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_leaves_other_formatters_unchanged ['--xml'] |
| S9.2 | With --compact, CSV output is unchanged. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_leaves_other_formatters_unchanged ['--csv'] |
| S9.3 | With --compact, Checkstyle output is unchanged. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_leaves_other_formatters_unchanged ['--checkstyle'] |
| S9.4 | With --compact, HTML output is unchanged. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_leaves_other_formatters_unchanged ['--html'] |
| S9.5 | With --compact, clang warning style is unchanged. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_leaves_other_formatters_unchanged ['-w','-a','1'] |
| S9.6 | With --compact, MSVS warning style is unchanged. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_leaves_other_formatters_unchanged ['--warning-msvs','-a','1'] |
| S9.7 | With --compact, extension printers are unchanged. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_leaves_extension_printer_unchanged (-Eduplicate) |
| S10 | Delivery is verified for JavaScript. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli uses code.js through the real CLI |
| S11 | Other readers that name nested functions (anonymous) get the same table behavior naturally. | slice 1 | observed: lizard_core/file_info_builder.py push_new_function (shared builder) and text_output.is_trivial_nested_callback have no language check |
| S11.1 | No language filter is added. | slice 1 | observed: lizard_core/text_output.py is_trivial_nested_callback has no language condition |
| S11.2 | No other reader's naming is changed. | slice 1 | observed: git diff: no lizard_languages/ change |
| K1.1 | sum example with --compact: the function table shows one row, sum at CCN 1. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it rows == ['sum'] |
| K1.2 | sum example: file and total function counts are 2. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it function_cnt 2; totals equal default |
| K2 | Same source without --compact: both functions appear at CCN 1. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_omits_trivial_nested_callback_but_counts_it default rows ['(anonymous)','sum'] |
| K3 | Same source: analysis results contain both functions at CCN 1. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_analysis_results_keep_trivial_nested_callback |
| K4 | Ternary reducer keeps its CCN 2 row with --compact, sum stays CCN 1. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_nested_callback_with_decisions |
| K5 | With --compact, function f() { function g() {} } keeps the g row. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_named_and_top_level_callbacks |
| K6 | With --compact, top-level [1, 2].forEach(x => console.log(x)); keeps its (anonymous) row. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_named_and_top_level_callbacks |
| K7.1 | With --compact -a 1, the two-parameter reducer stays visible at CCN 1 as a parameter warning. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_warned_callback_and_exit_status |
| K7.2 | With --compact -a 1, the warning still makes the run exit non-zero. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_warned_callback_and_exit_status returncode 1 |
| X1 | Does not add a callback's complexity, tokens or parameters to its enclosing function. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_analysis_results_keep_trivial_nested_callback sum CCN 1; no analysis change |
| X2 | No marker or count of omitted rows. | slice 1 | observed: lizard_core/text_output.py print_and_save_modules prints nothing for omitted rows |
| X3 | No compacting of named callbacks. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_named_and_top_level_callbacks |
| X4 | No compacting of callbacks with CCN > 1. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_keeps_nested_callback_with_decisions |
| X5 | No compact behavior for other formatters. | slice 1 | observed: test/testCompactReport.py::TestCompactReportCli::test_compact_leaves_other_formatters_unchanged |
