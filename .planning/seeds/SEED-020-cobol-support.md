---
id: SEED-020
status: active
planted: 2026-10-29
planted_during: Product backlog intake requested by the developer
scope: COBOL language support
---

# SEED-020: COBOL support

## Why This Matters

COBOL developers and maintainers need to identify complex code in their
codebases using the same Lizard workflow they use for other languages. GitHub
issue #431 asks for COBOL support with no further detail; the request closes
that gap for the common fixed-format program case.

## Alternatives and Decision

Compared against the goal and the constraint that Lizard readers are
self-contained token/state-machine parsers (ADR-0001, *Language reader*):

- **Recommended: a new `CobolReader` with a custom `CodeStateMachine`,
  reporting each PROCEDURE DIVISION paragraph as a Function.** Fixed-format
  source is the initial target; `.cbl`, `.cob`, and `.cobol` are discovered.
  Decision counting follows McCabe-style binary decisions on core COBOL
  control flow (IF, EVALUATE WHEN, conditional PERFORM, SEARCH, AND/OR), not
  vendor-specific EXEC CICS catalogues. This matches SonarQube's published
  COBOL rule that a "function" is a paragraph, and keeps reviewable units
  small enough to find complex code.
- Whole PROCEDURE DIVISION as one Function: simpler, but a typical program is
  one huge unit and fails the purpose of locating code that needs review.
  Rejected for the initial story.
- Sections only as Functions: coarser than paragraphs; PERFORM targets and
  review granularity are usually paragraphs. Rejected.
- Reusing the Fortran reader by swapping keywords: Fortran procedures and
  COBOL paragraphs share case-insensitivity and a fixed-format heritage, but
  the declaration shape (SUBROUTINE/FUNCTION vs paragraph-name period) and
  control vocabulary differ; a keyword swap does not parse COBOL. Rejected.
- Delegating to an external COBOL toolchain or parser: contradicts the
  self-contained reader architecture every existing language follows.
  Rejected.

Borrowed mechanism: Fortran's fixed-format preprocess (column-oriented
comment/indicator handling) and case-insensitive token additions, plus the
non-brace custom state-machine pattern used by Fortran and Structured Text.
Mapped roles: Fortran column-1 `C`/`*` comment → COBOL column-7 `*`/`/`
comment indicator; Fortran procedure/function start → COBOL paragraph header
ending with `.`; Fortran `IF`/`DO` conditions → COBOL `IF` / `PERFORM … UNTIL`
/ `EVALUATE … WHEN`. Limit where the analogy breaks: COBOL has no nested
procedure scopes like Fortran; `PERFORM` transfers control without opening a
lexical nest the way a Fortran `CONTAINS` procedure does; and `COPY` injects
text from other files, unlike Fortran's self-contained sources. The adapted
candidate therefore owns fixed-format preprocess and paragraph boundaries
inside one file, and leaves COPY expansion out of this story.

## Selected Stories

<a id="cobol-support"></a>

### 1. Support COBOL analysis

**Identity:** SEED-020#cobol-support
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/015-cobol-support/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"891516042f10404ccd5940bb970f00d6f693ae4d04da4ab5ea4cf7a3ed443ee7","plan":"d931288b3a82e646e63e7fb2de3dee3d6239dace602de620ca509895d99a0a7e"}}
```

**Goal:** COBOL developers and maintainers, and maintainers of mixed-language
codebases, point Lizard at their COBOL sources through the existing CLI and
Python API and receive each PROCEDURE DIVISION paragraph with CCN, NLOC, and
token count (parameter count 0), so they can find paragraphs that need review
or simplification. This closes the request behind issue #431 for the common
fixed-format program case and extends Lizard's many-language promise to COBOL.

**Scope:**

Required behavior:

- `.cbl`, `.cob`, and `.cobol` files are discovered by default, selectable with
  `-l cobol`, listed in the `-l` help, and analyzable through `analyze_file`
  and `analyze_source_code`. The README language list gains COBOL.
- Source format: fixed-format COBOL. Columns 1–6 are sequence area (ignored for
  tokens); column 7 is the indicator (`*` or `/` → comment line; `D`/`d` →
  debugging line treated as comment for analysis; space → code; `-` →
  continuation handled so the continued token does not invent decisions).
  Area A/B layout is not otherwise enforced beyond recognizing paragraph and
  section headers.
- Function boundaries: each paragraph in the PROCEDURE DIVISION is one
  Function (ADR-0001 *Function*), named by its paragraph name. A section
  header (`NAME SECTION.`) is not itself a Function; paragraphs inside the
  section are. Divisions other than PROCEDURE DIVISION (IDENTIFICATION, ENVIRONMENT,
  DATA) contribute no Functions and no decisions. Statements that appear in
  PROCEDURE DIVISION before any paragraph or section header, if present, form
  one Function named after `PROGRAM-ID` when known, otherwise `(procedure)`.
- Decisions (CCN): `IF`; each `ELSE IF` / `ELSEIF`; each `WHEN` in `EVALUATE`
  except `WHEN OTHER`; `PERFORM … UNTIL`, `PERFORM … TIMES`, and
  `PERFORM … VARYING` (one each); each `WHEN` and `AT END` in `SEARCH`;
  `AND` and `OR` in conditions. Not decisions: `ELSE`, `END-IF`, `END-EVALUATE`,
  `END-PERFORM`, `END-SEARCH`, unconditional `PERFORM` / `GO TO` /
  `GOBACK` / `EXIT` / `STOP RUN`, `WHEN OTHER`, period terminators, and
  content of comments or string literals.
- Comments and strings: fixed-format indicator comments; `*>` inline comments
  when they appear in Area B; alphanumeric literals (`"..."` / `'...'`). Their
  contents introduce neither decisions nor Functions; comment lines are
  excluded from NLOC.
- Parameters: COBOL paragraphs have no parameter list; parameter count is 0.
- Existing language analysis is unchanged: the full suite stays green.

Deferred promises (not built or verified by this story):

- Free-format and variable-format source (`SOURCEFORMAT"FREE"` / equivalent).
- `.cpy` / `.copy` copybook files as first-class inputs, and `COPY` / `REPLACE`
  expansion into the including program.
- Nested programs, declarative sections (`USE` after `DECLARATIVES`), and
  rolled-up section- or program-level CCN beyond the per-paragraph Functions
  and ordinary file aggregates.
- Vendor/dialect decision catalogues (EXEC CICS HANDLE/LINK/XCTL, EXEC SQL,
  `ALTER`, `DEPENDING` on `GO TO`, and similar Sonar-listed extras).
- Nesting-depth or other extension metrics beyond the core set the shared
  machinery already yields; not verified here.

Rejection constraints: none. No product requirement calls for rejecting any
COBOL input; unrecognized dialects may yield incomplete Functions without a
hard error.

Boundary assumptions:

- Fixed-format ANSI/ISO-style COBOL PROCEDURE DIVISION with paragraphs is the
  target; Micro Focus / IBM / GnuCOBOL sources that stay within that shape are
  covered by the same rules without vendor-specific verification.
- Lizard depends on no external COBOL toolchain, consistent with every
  existing reader.
- Case-insensitive reserved words, matching Fortran and Structured Text.

**Architecture:**

- New `CobolReader` under `lizard_languages/`, registered in
  `lizard_languages/__init__.py`, following ADR-0001 (*Language reader*,
  *CodeReader*, *CodeStateMachine*, *Function*). No new ADR: this is the
  established language-extension point used by Fortran, Structured Text, and
  every other reader.
- Paragraph → Function is a product mapping inside the reader, not a glossary
  change; CLI and API still expose Functions with CCN, NLOC, and token count.
- Fixed-format column handling stays local to the reader's preprocess/tokenizer,
  analogous to Fortran's column-1 comment rewrite; it does not change the
  shared analysis pipeline.

**Key examples:**

1. **Discovery and selection.** A directory holds `pay.cbl` and `tool.py` →
   `lizard .` → both files' Functions are reported; `lizard -l cobol .`
   reports only the COBOL paragraphs; the `-l` help text lists `cobol`.
2. **Straight-line paragraph.** Fixed-format program with `PROGRAM-ID. HELLO.`
   and PROCEDURE DIVISION paragraph `MAIN.` containing only `MOVE "HI" TO WS-MSG.`
   and `GOBACK.` → one Function `MAIN`, CCN 1, parameter count 0; NLOC counts
   the non-comment procedure lines of that paragraph.
3. **Branching.** Paragraph `CHECK-LIMIT.` with
   `IF WS-COUNT > 10 AND WS-FLAG = "Y" THEN ADD 1 TO WS-OVER ELSE MOVE 0 TO WS-OVER END-IF.`
   → Function `CHECK-LIMIT`, CCN 3 (`IF`, `AND`).
4. **EVALUATE and SEARCH.** Paragraph `CLASSIFY.` with
   `EVALUATE WS-CODE WHEN 1 MOVE "A" TO WS-OUT WHEN 2 WHEN 3 MOVE "B" TO WS-OUT WHEN OTHER MOVE "Z" TO WS-OUT END-EVALUATE`
   → CCN 3 (two counted `WHEN` arms; `WHEN OTHER` adds nothing). Paragraph
   `LOOKUP.` with `SEARCH WS-TABLE AT END MOVE "X" TO WS-STATUS WHEN WS-KEY = WS-IN MOVE "O" TO WS-STATUS END-SEARCH`
   → CCN 3 (`AT END`, `WHEN`).
5. **Conditional PERFORM.** Paragraph `DRIVE.` with
   `PERFORM PROCESS-ONE UNTIL WS-DONE = "Y"` and
   `PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5 CONTINUE END-PERFORM`
   → CCN 4 (base 1; one `UNTIL` on the first PERFORM; `VARYING` and `UNTIL` on
   the second). An unconditional `PERFORM PROCESS-ONE` elsewhere adds nothing.
6. **Sections do not create Functions.** `MAIN-SECTION SECTION.` followed by
   paragraphs `INIT.` and `RUN.` → Functions `INIT` and `RUN` only; no Function
   named `MAIN-SECTION`.
7. **Fixed-format comments and strings stay inert.** A paragraph whose Area B
   contains `MOVE "IF AND OR WHEN" TO WS-TXT.` and a column-7 `*` comment line
   reading `* IF WHEN PERFORM UNTIL` → that paragraph's CCN is 1; the comment
   line is excluded from NLOC.
8. **Non-procedure divisions are silent.** IDENTIFICATION, ENVIRONMENT, and
   DATA DIVISION content, including `01` record layouts with names that look
   like verbs, produce no Functions.
9. **Other languages unchanged.** Running the full test suite after the reader
   lands → green, with no change to any existing language's results.

**Investigation notes (facts vs hypotheses):**

| Claim | Status | Source / observation |
| --- | --- | --- |
| No COBOL reader exists; extensions `.cbl`/`.cob`/`.cobol` are unregistered | Fact | `lizard_languages/__init__.py` language list; `languages()` probe in this workspace |
| Issue #431 requests COBOL support | Fact | `gh issue view 431` (open, empty body) |
| SonarQube defines COBOL "functions" as paragraphs and lists decision keywords | Fact | SonarQube Server metrics documentation |
| Fixed format uses columns 1–6 sequence, column 7 indicator, Area A/B | Fact | Micro Focus / ISO COBOL format docs |
| Copybooks commonly use `.cpy`; Veracode lists COB/CBL/COBOL/PCO and CPY | Fact | Veracode COBOL packaging docs |
| Fortran fixed-format preprocess and case-insensitive reader are the closest in-repo pattern | Fact | `lizard_languages/fortran.py` |
| Free-format share of the issue #431 audience | Hypothesis (deferred) | No audience data; free format deferred without blocking the purpose |
| Exact CCN for `PERFORM VARYING … UNTIL` (one vs two) under McCabe | Settled in scope: both `VARYING` and `UNTIL` count when both appear (example 5 → CCN 4) | Product rule recorded above |

**Open decisions:** none. Competing interpretations (whole-program Function,
free-format first, copybook expansion) were considered and deferred or
rejected under Alternatives and Decision.
