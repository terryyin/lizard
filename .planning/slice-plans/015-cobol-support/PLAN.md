# COBOL language support

**Mapped story identity:** SEED-020#cobol-support
**Source:** [canonical story](../../seeds/SEED-020-cobol-support.md#cobol-support)
**Source review:** [source-review.md](source-review.md)

## Goal and scope

COBOL developers analyze fixed-format `.cbl` / `.cob` / `.cobol` sources
through the existing CLI and Python API and get each PROCEDURE DIVISION
paragraph as a Function with CCN, NLOC, and token count (parameter count 0),
so they can find paragraphs that need review. The story's Scope section is
authoritative for required behavior, deferred promises, and boundary
assumptions.

**In scope:** a new `CobolReader` in `lizard_languages/cobol.py` (states may
live beside it), registered in `lizard_languages/__init__.py`; fixed-format
preprocess (sequence area and column-7 indicator); paragraph Function
boundaries; core decision keywords; inert comments and string literals; the
README language list; tests under `test/test_languages/testCobol.py`.

**Excluded (story's deferred promises):** free/variable format; `.cpy` and
COPY/REPLACE expansion; nested programs and section/program CCN rollups;
vendor EXEC CICS / EXEC SQL decision catalogues; verification of extension
metrics beyond the core set.

**Architecture / PFE:** The reader is Lizard's established extension point
(ADR-0001 *Language reader*: inherits `CodeReader`, registered in
`lizard_languages/__init__.py`), so no ADR or North Star change is needed.
Reuse found across the product:

- **Gap:** no COBOL reader or extensions exist today (premise P1). Add a new
  `CobolReader`, do not overload Fortran or Structured Text.
- **Reuse pattern:** Fortran's fixed-format preprocess and case-insensitive
  token additions, and the non-brace custom `CodeStateMachine` pattern used by
  Fortran and Structured Text. COBOL paragraph headers (`NAME.`) replace
  Fortran `SUBROUTINE`/`FUNCTION` starts; column-7 `*`/`/` comments replace
  Fortran column-1 `C`/`*`.
- **Proof entry:** `analyze_file.analyze_source_code("a.cbl", source)` through
  a `get_cobol_function_list` helper, matching
  `test/test_languages/testFortran.py` / Kotlin helper style; CLI discovery
  follows `test/testApplication.py` conventions.

Naming follows ADR-0001's alignment policy: capability names (`CobolReader`,
`cobol.py`, `testCobol.py`), no phase numbers in product code. Paragraph →
Function is a reader mapping; the API still reports Functions.

**Assumptions:** fixed-format ANSI/ISO-style PROCEDURE DIVISION with
paragraphs; no external COBOL toolchain; case-insensitive reserved words.

## Outside-in proof

Primary boundary: `analyze_file.analyze_source_code("a.cbl", source)` in
`test/test_languages/testCobol.py`, one test per key example (or focused
groups), asserting names, CCN, NLOC, and parameter counts. CLI discovery and
`-l cobol` selection are proved at the `lizard.main` boundary with a temporary
directory holding a `.cbl` and a `.py` file (filesystem boundary only; no
parsing mocks).

Local gates, cited from `AGENTS.md`: focused
`nix develop -c python -m pytest test/test_languages/testCobol.py` during each
slice; before completing the change, the full suite and format check
`nix develop -c make`. Hosted CI (`python-app.yml`) runs pytest across Python
3.8–3.14 after publication; avoid syntax newer than 3.8.

| Promise (story scope) | Owning slice | Proof signal |
| --- | --- | --- |
| `.cbl`/`.cob`/`.cobol` discovered, `-l cobol`, help lists `cobol`, README | 1 | CLI test on temp dir; help contains `cobol`; README line present |
| Straight-line paragraphs; sections not Functions; non-PROCEDURE silent; fixed-format comments inert (examples 2, 6, 7-comments, 8, 9) | 1 | `MAIN` CCN 1; `INIT`/`RUN` only under a SECTION; DATA DIVISION yields no Functions; full suite green |
| IF and AND/OR decisions (example 3) | 2 | `CHECK-LIMIT` CCN 3 |
| EVALUATE WHEN / SEARCH (example 4) | 2 | `CLASSIFY` CCN 3; `LOOKUP` CCN 3 |
| Conditional PERFORM (example 5) | 2 | `DRIVE` CCN 4; unconditional PERFORM adds 0 |
| String literals inert (example 7-strings) | 1 | CCN 1 when literals hold reserved words |

## Settled decisive premises

Observed on 2026-10-29 in this workspace
(`/Users/terryyin/git/lizard/.worktrees/support-cobol-analysis`, branch
`cursor/support-cobol-analysis` at `b8e0761`), using in-process
`languages()` monkeypatches only; no product file was left edited.

| # | Premise | Consuming slices | Observation | Result |
| --- | --- | --- | --- | --- |
| P1 | No reader matches `.cbl` / `.cob` / `.cobol` / `.cpy`; analyzing `.cbl` yields no Functions | 1 | `get_reader_for` → `None` for each; `analyze_source_code('pay.cbl', …)` → `function_list == []` | True |
| P2 | Default tokenization keeps fixed-format sequence numbers and splits `MAIN` + `.` (and `END-IF` → `END`,`IF`; `PROGRAM-ID` → `PROGRAM`,`-`,`ID`) | 1 | `generate_tokens` on a fixed-format sample: first tokens include `000100`; `MAIN.` is never one token; `MAIN` then `.` appears; `IF`/`AND`/`END`/`IF` for `END-IF` | True |
| P3 | A `CodeReader` subclass with `ext` and `language_names = ['cobol']` is selected by filename (case-insensitive) once registered | 1 | In-process `CobolReader` appended to `languages()` → `get_reader_for('pay.cbl')` returns it; `match_filename('x.CBL')` true | True |
| P4 | Condition categories on the reader drive CCN once tokens reach the shared counter | 2 | Existing `CodeReader._build_conditions` unions `_control_flow_keywords`, `_logical_operators`, `_case_keywords`; Fortran/ST set case-inclusive keyword sets the same way | True |
| P5 | Nothing in product code or user docs references COBOL yet | 1 | Repo grep for `cobol` / `.cbl` outside planning: only this seed/backlog/agent assignment; `README.rst` language list has no COBOL | True |

## Ordered slices

### 1. COBOL files are discovered and fixed-format paragraphs are analyzed
Type: Behavior
Status: done
Proof: Accepted 2026-10-29 — `nix develop -c python -m pytest test/test_languages/testCobol.py` (12 passed): `MAIN` CCN 1 / param 0 / NLOC 3; section → `INIT`/`RUN` only; non-PROCEDURE empty; literals + col-7 `*`/`D`/`-` continuation + Area B `*>` → CCN 1; preamble PROGRAM-ID/`(procedure)`; CLI `-l cobol` / default / `-h`; README lists COBOL. Full suite remains a delivery/CI gate (commitment 13 → slice 2). Refactor: `CobolStates` extracted to `cobol_states.py`.

Behavior: Given a fixed-format `.cbl` / `.cob` / `.cobol` file → analyze
through API or CLI → each PROCEDURE DIVISION paragraph is a Function with
straight-line CCN 1, sections are not Functions, other divisions are silent,
fixed-format comments and string literals introduce neither decisions nor
Functions.

Interim behavior (named, replaced later): branching constructs undercount
until slice 2 (`IF`, `AND`/`OR`, `EVALUATE`/`WHEN`, `SEARCH`, conditional
`PERFORM` may be absent or wrong).

Files: `lizard_languages/cobol.py` (+ states module if kept separate),
`lizard_languages/__init__.py`, `README.rst`,
`test/test_languages/testCobol.py` (helper optional).

Design notes: preprocess strips columns 1–6 and handles column-7 indicators
before ordinary tokenization (P2); tokenizer additions keep `END-IF`,
`END-EVALUATE`, `END-PERFORM`, `END-SEARCH`, `PROGRAM-ID`, `ELSE IF` /
`ELSEIF`, and similar multi-word forms intact where needed so they are not
mistaken for decisions or paragraph names; paragraph start is an Area-A-style
name followed by `.` after PROCEDURE DIVISION; end the previous Function when
the next paragraph/section header or end of PROCEDURE DIVISION arrives;
`get_comment_from_token` required (probe without it raised
`AttributeError`).

### 2. Core COBOL decisions raise CCN
Type: Behavior
Status: done
Proof: Accepted 2026-10-29 — `nix develop -c python -m pytest test/test_languages/testCobol.py test/test_languages/testCobolDecisions.py` (18 passed): CHECK-LIMIT CCN 3; CLASSIFY CCN 3; LOOKUP CCN 3; DRIVE CCN 4; unconditional transfers add 0; ELSE IF/ELSEIF/TIMES covered. Full suite `nix develop -c python -m pytest` → 2179 passed, 7 skipped.

Behavior: Given the story's decision forms inside a paragraph → analyze →
CCN matches the Key examples; non-decision keywords listed in Scope add
nothing.

Design: set `_control_flow_keywords` / `_logical_operators` / `_case_keywords`
(and any COBOL-specific counting in the state machine for `PERFORM … UNTIL` /
`TIMES` / `VARYING` and `AT END`) so the shared condition counter and any
explicit `add_condition` calls match the scope rule. Prefer growing one
condition model over parallel recognizers per verb.

Safe stop: after either slice the suite is green and earlier behavior holds;
interim undercounting named in slice 1 is gone after slice 2.

## Execution complete

Product advice: Fixed-format COBOL paragraph Functions with core decision CCN
close issue #431 for the common case and fit current direction (maintenance /
PR work) as finishing Taken language intake — do not queue free-format,
copybooks, nested/rollups, or vendor EXEC catalogues from deferred D1–D5 while
near-future direction stays maintenance-first. Keep G2 (global `AT END`) as
settled; optional hygiene if COBOL is touched again: an explicit `OR` fixture
(wired, not story-example-proved) and a column-7 `/` comment case. No backlog
change from this review.

## Current decisions

- New `CobolReader` + custom states; do not extend `CLikeReader`.
- Functions are paragraphs only; section headers are not Functions.
- Parameter count is always 0 for paragraphs.
- `PERFORM VARYING … UNTIL` counts both `VARYING` and `UNTIL` when both
  appear (story example 5 → CCN 4).
- Tokenizer multi-word / hyphenated reserved forms stay subgroup-free regex
  alternatives where possible (`token_pattern.py` rule).

## Learnings

- `line_counter` drops `\n` before the state machine, so Area-A paragraph/section
  detection must run in reader `preprocess` (while leading spaces/newlines still
  exist), then emit synthetic `@COBOL_PARAGRAPH` / `@COBOL_SECTION` markers.
- Conditional PERFORM fits the shared model via `UNTIL`/`TIMES`/`VARYING`, not
  `PERFORM`. Keep `WHEN OTHER` and `END-*` as intact tokens. Fall-through
  EVALUATE (`WHEN 2 WHEN 3`) needs state-machine arm collapse after the shared
  condition counter.

## Story obligations

### G1. Decision keywords undercount CCN until slice 2
Reported: slice 1 — "Decision CCN (IF/AND/OR/EVALUATE/SEARCH/conditional PERFORM) — interim undercount until slice 2"
Story clause: "Decisions (CCN): `IF`; each `ELSE IF` / `ELSEIF`; each `WHEN` in `EVALUATE` except `WHEN OTHER`; `PERFORM … UNTIL`, `PERFORM … TIMES`, and `PERFORM … VARYING` (one each); each `WHEN` and `AT END` in `SEARCH`; `AND` and `OR` in conditions."
Disposition: proved by slice 2: test/test_languages/testCobolDecisions.py::test_if_and_raises_ccn, test_evaluate_when_arms_and_when_other, test_search_at_end_and_when, test_conditional_perform_until_and_varying, test_else_if_elseif_and_perform_times

### G2. AT END also counts outside SEARCH
Reported: slice 2 — "AT END counts globally (incl. READ), not SEARCH-only"
Story clause: "each `WHEN` and `AT END` in `SEARCH`"
Disposition: no user cost "so they can find paragraphs that need review or simplification": AT END on READ is still a real binary decision that raises paragraph CCN; SEARCH AT END remains proved, and no story text forbids counting other AT END phrases.

### GX-D1. Free-format deferred
Reported: slice 1 — "deferred free/variable format"
Story clause: "Free-format and variable-format source (`SOURCEFORMAT"FREE"` / equivalent)."
Disposition: excluded "Free-format and variable-format source (`SOURCEFORMAT"FREE"` / equivalent)."
Commitment: D1

### GX-D2. Copybooks deferred
Reported: slice 1 — "deferred copybook inputs and COPY/REPLACE"
Story clause: "`.cpy` / `.copy` copybook files as first-class inputs, and `COPY` / `REPLACE` expansion into the including program."
Disposition: excluded "`.cpy` / `.copy` copybook files as first-class inputs, and `COPY` / `REPLACE` expansion into the including program."
Commitment: D2

### GX-D3. Nested programs and rollups deferred
Reported: slice 1 — "deferred nested programs / declaratives / rolled-up CCN"
Story clause: "Nested programs, declarative sections (`USE` after `DECLARATIVES`), and rolled-up section- or program-level CCN beyond the per-paragraph Functions and ordinary file aggregates."
Disposition: excluded "Nested programs, declarative sections (`USE` after `DECLARATIVES`), and rolled-up section- or program-level CCN beyond the per-paragraph Functions and ordinary file aggregates."
Commitment: D3

### GX-D4. Vendor decision catalogues deferred
Reported: slice 1 — "deferred vendor/dialect decision catalogues"
Story clause: "Vendor/dialect decision catalogues (EXEC CICS HANDLE/LINK/XCTL, EXEC SQL, `ALTER`, `DEPENDING` on `GO TO`, and similar Sonar-listed extras)."
Disposition: excluded "Vendor/dialect decision catalogues (EXEC CICS HANDLE/LINK/XCTL, EXEC SQL, `ALTER`, `DEPENDING` on `GO TO`, and similar Sonar-listed extras)."
Commitment: D4

### GX-D5. Extension metrics deferred
Reported: slice 1 — "deferred nesting-depth / extension metrics verification"
Story clause: "Nesting-depth or other extension metrics beyond the core set the shared machinery already yields; not verified here."
Disposition: excluded "Nesting-depth or other extension metrics beyond the core set the shared machinery already yields; not verified here."
Commitment: D5

## Concern evidence

- Slice-plan refinement: not needed — two Behaviors, each with one proof loop
  and a safe stop; Structure is folded into slice 1's reader creation because
  there is no reusable recogniser to prepare first; decisive premises P1–P5
  are observed in this workspace.
- Remaining slice-specific concerns: none identified. Slice 2 bundles several
  decision forms under one scope rule and one proof loop; split during
  execution only if PERFORM/EVALUATE handling overruns.
- Interim behavior named in slice 1 is an accepted trade-off with its
  replacing slice.
