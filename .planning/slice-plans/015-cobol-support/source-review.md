# Source review — SEED-020#cobol-support

**Launch:** fresh general-purpose reviewer (no conversation inheritance),
source-only; read no project files; change nothing.
**Input:** Goal, Scope (required + deferred), rejection constraints, boundary
assumptions, and Key examples 1–9 from
`.planning/seeds/SEED-020-cobol-support.md#cobol-support`. Explicit human
amendments: none.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | COBOL developers/maintainers and mixed-language maintainers can point Lizard at COBOL sources via the existing CLI and Python API and receive each PROCEDURE DIVISION paragraph with CCN, NLOC, and token count, with parameter count 0, so they can find paragraphs needing review or simplification; this closes issue #431 for the common fixed-format case and extends Lizard’s many-language promise to COBOL. | slice 2 | |
| 2a.1 | `.cbl` files are discovered by default. | slice 1 | observed: testCobol.py::test_extensions_registered; TestCobolCli::test_discovery_language_filter_and_help |
| 2a.2 | `.cob` files are discovered by default. | slice 1 | observed: testCobol.py::test_extensions_registered |
| 2a.3 | `.cobol` files are discovered by default. | slice 1 | observed: testCobol.py::test_extensions_registered |
| 2b | COBOL analysis is selectable with `-l cobol`. | slice 1 | observed: TestCobolCli::test_discovery_language_filter_and_help |
| 2c | `cobol` is listed in the `-l` help. | slice 1 | observed: TestCobolCli::test_discovery_language_filter_and_help (`-h` contains cobol) |
| 2d.1 | COBOL is analyzable through `analyze_file`. | slice 1 | observed: get_cobol_function_list → analyze_file.analyze_source_code in testCobol.py |
| 2d.2 | COBOL is analyzable through `analyze_source_code`. | slice 1 | observed: get_cobol_function_list helper in testCobol.py |
| 2e | The README language list includes COBOL. | slice 1 | observed: TestCobolCli::test_readme_lists_cobol |
| 3 | Target source format is fixed-format COBOL. | slice 1 | observed: preprocess_fixed_format + fixed() fixtures in testCobol.py |
| 4a.1 | Columns 1–6 (sequence area) are ignored for tokens. | slice 1 | observed: preprocess_fixed_format strips cols 1–6; fixed() fixtures still parse |
| 4a.2 | Column 7 `*` → comment line. | slice 1 | observed: test_comments_and_string_literals_are_inert |
| 4a.3 | Column 7 `/` → comment line. | slice 1 | observed: cobol.py preprocess_fixed_format maps `/` with `*`/`D`/`d`; same comment path as `*` |
| 4a.4 | Column 7 `D`/`d` → debugging line treated as comment for analysis. | slice 1 | observed: test_column7_debug_line_is_comment |
| 4a.5 | Column 7 space → code. | slice 1 | observed: all fixed() fixtures use space indicator for code lines |
| 4a.6 | Column 7 `-` → continuation handled so the continued token does not invent decisions. | slice 1 | observed: test_column7_continuation_keeps_string_inert |
| 4b | Area A/B layout is not otherwise enforced beyond recognizing paragraph and section headers. | slice 1 | observed: cobol.py `_emit_line` Area-A header recognition only |
| 5 | Each paragraph in the PROCEDURE DIVISION is one Function, named by its paragraph name. | slice 1 | observed: test_straight_line_paragraph (MAIN) |
| 6 | A section header (`NAME SECTION.`) is not itself a Function; paragraphs inside the section are. | slice 1 | observed: test_section_is_not_a_function |
| 7a | IDENTIFICATION DIVISION contributes no Functions and no decisions. | slice 1 | observed: test_non_procedure_divisions_yield_no_functions |
| 7b | ENVIRONMENT DIVISION contributes no Functions and no decisions. | slice 1 | observed: test_non_procedure_divisions_yield_no_functions |
| 7c | DATA DIVISION contributes no Functions and no decisions. | slice 1 | observed: test_non_procedure_divisions_yield_no_functions |
| 8 | Statements in PROCEDURE DIVISION before any paragraph or section header, if present, form one Function named after `PROGRAM-ID` when known, otherwise `(procedure)`. | slice 1 | observed: test_preamble_before_paragraph_named_after_program_id; test_preamble_without_program_id |
| 9a | `IF` is a decision. | slice 2 | G1 |
| 9b.1 | Each `ELSE IF` is a decision. | slice 2 | G1 |
| 9b.2 | Each `ELSEIF` is a decision. | slice 2 | G1 |
| 9c | Each `WHEN` in `EVALUATE` except `WHEN OTHER` is a decision. | slice 2 | G1 |
| 9d.1 | `PERFORM … UNTIL` counts as one decision. | slice 2 | G1 |
| 9d.2 | `PERFORM … TIMES` counts as one decision. | slice 2 | G1 |
| 9d.3 | `PERFORM … VARYING` counts as one decision. | slice 2 | G1 |
| 9e.1 | Each `WHEN` in `SEARCH` is a decision. | slice 2 | G1 |
| 9e.2 | Each `AT END` in `SEARCH` is a decision. | slice 2 | G1 |
| 9f.1 | `AND` in conditions is a decision. | slice 2 | G1 |
| 9f.2 | `OR` in conditions is a decision. | slice 2 | G1 |
| 10a | `ELSE` is not a decision. | slice 2 | |
| 10b | `END-IF` is not a decision. | slice 2 | |
| 10c | `END-EVALUATE` is not a decision. | slice 2 | |
| 10d | `END-PERFORM` is not a decision. | slice 2 | |
| 10e | `END-SEARCH` is not a decision. | slice 2 | |
| 10f.1 | Unconditional `PERFORM` is not a decision. | slice 2 | |
| 10f.2 | Unconditional `GO TO` is not a decision. | slice 2 | |
| 10f.3 | Unconditional `GOBACK` is not a decision. | slice 2 | |
| 10f.4 | Unconditional `EXIT` is not a decision. | slice 2 | |
| 10f.5 | Unconditional `STOP RUN` is not a decision. | slice 2 | |
| 10g | `WHEN OTHER` is not a decision. | slice 2 | |
| 10h | Period terminators are not decisions. | slice 2 | |
| 10i.1 | Content of comments does not introduce decisions. | slice 1 | observed: test_comments_and_string_literals_are_inert; test_area_b_inline_comment_is_inert |
| 10i.2 | Content of string literals does not introduce decisions. | slice 1 | observed: test_comments_and_string_literals_are_inert; test_column7_continuation_keeps_string_inert |
| 11a | Fixed-format indicator comments are recognized. | slice 1 | observed: test_comments_and_string_literals_are_inert; test_column7_debug_line_is_comment |
| 11b | `*>` inline comments are recognized when they appear in Area B. | slice 1 | observed: test_area_b_inline_comment_is_inert |
| 11c.1 | Alphanumeric literals with `"..."` are recognized. | slice 1 | observed: test_comments_and_string_literals_are_inert |
| 11c.2 | Alphanumeric literals with `'...'` are recognized. | slice 1 | observed: test_comments_and_string_literals_are_inert |
| 11d.1 | Comment contents introduce neither decisions nor Functions. | slice 1 | observed: test_comments_and_string_literals_are_inert |
| 11d.2 | String-literal contents introduce neither decisions nor Functions. | slice 1 | observed: test_comments_and_string_literals_are_inert |
| 11e | Comment lines are excluded from NLOC. | slice 1 | observed: test_comments_and_string_literals_are_inert; test_column7_debug_line_is_comment |
| 12 | COBOL paragraphs have no parameter list; parameter count is 0. | slice 1 | observed: test_straight_line_paragraph |
| 13 | Existing language analysis is unchanged; the full suite stays green. | slice 2 | |
| D1 | Free-format and variable-format source not built or verified. | | GX-D1 |
| D2 | `.cpy` / `.copy` and COPY/REPLACE expansion not built or verified. | | GX-D2 |
| D3 | Nested programs, declaratives, rolled-up section/program CCN not built or verified. | | GX-D3 |
| D4 | Vendor/dialect decision catalogues not built or verified. | | GX-D4 |
| D5 | Nesting-depth or other extension metrics beyond core set not verified. | | GX-D5 |
