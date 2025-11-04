# TODO List

## Completed Items
- [x] Fix ES6/JavaScript template string tokenization to match test expectations (completed)

## Checkstyle Output Feature
- [ ] Create or update `ongoing/checkstyle_output.md` with requirements and design notes.
- [ ] Add `--checkstyle` option to the CLI and ensure it appears in `lizard --help`.
- [ ] Implement `lizard_ext/checkstyleoutput.py` to generate Checkstyle XML output.
- [ ] Integrate the new output so `--checkstyle` and/or `.checkstyle.xml` triggers it.
- [ ] Add/extend tests to verify:
      - CLI accepts `--checkstyle`
      - Output is valid Checkstyle XML
      - Output is accepted by Checkstyle consumers (e.g., Jenkins Warnings NG)
      - Help message includes the new option
- [ ] Update documentation as needed (requirement doc, README).

## Separate Conditions Refactoring

### Phase 1: Research and Planning ✅ COMPLETED
- [x] Research how _conditions is used across all language readers
- [x] Identify proper domain names for the concepts
- [x] Find languages with potential bugs related to mixing
- [x] Create comprehensive inventory of all language conditions
- [x] Document refactoring plan

### Phase 2: Infrastructure ✅ COMPLETED
- [x] Add four new class fields to CodeReader base class:
  - `_control_flow_keywords`
  - `_logical_operators`
  - `_case_keywords`
  - `_ternary_operators`
- [x] Update `CodeReader.__init__()` to:
  - Build combined `conditions` set from new fields
  - Expose individual sets as instance attributes
  - Support backward compatibility with `_conditions`
- [x] Add helper method `_build_conditions()` to combine sets
- [x] Run all tests to ensure no regression

### Phase 3: Update Language Readers ✅ COMPLETED (All 21+ languages)
- [x] Update C# (csharp.py)
- [x] Update GDScript (gdscript.py)
- [x] Update Kotlin (kotlin.py)
- [x] Update Rust (rust.py) - also fix 'case' bug
- [x] Update Scala (scala.py)
- [x] Update Solidity (solidity.py)
- [x] Update Swift (swift.py)
- [x] Update TSX (tsx.py)
- [x] Update TypeScript (typescript.py)
- [x] Update Erlang (erlang.py) - also fix '?' meaning
- [x] Update Fortran (fortran.py)
- [x] Update PL/SQL (plsql.py)
- [x] Update Python (python.py)
- [x] Update Zig (zig.py)
- [x] Update TNSDL (tnsdl.py)
- [x] Update TTCN (ttcn.py) - also review 'else' inclusion
- [x] Update ST (st.py) - also review missing logical operators
- [x] Update Perl (perl.py) - consolidate two _conditions definitions
- [x] Update PHP (php.py)
- [x] Update Ruby/Lua (rubylike.py)
- [x] Update R (r.py) - fix element-wise vs short-circuit operators
- [x] Languages using base class (Go, Java, JavaScript, C++, Objective-C, Vue) - verified OK

### Phase 4: Update Extensions ✅ COMPLETED
- [x] Update lizardnonstrict.py to use `logical_operators`
- [x] Update lizardmccabe.py to use `case_keywords`
- [x] Update lizardmodified.py to use `case_keywords`
- [x] Review lizardcomplextags.py for any needed updates
- [x] Test all extensions with new structure

### Phase 5: Testing and Bug Reproduction (TDD Approach) ✅ COMPLETED
- [x] Ensure all existing tests pass after refactoring
- [x] Write test for R language element-wise (&, |) vs short-circuit (&&, ||) operators
  - Added 2 tests documenting operator behavior
- [x] Write test for Rust 'case' keyword bug
  - Added 2 tests - bug already fixed in Phase 2-3
- [x] Write test for Erlang '?' operator meaning
  - Added test documenting macro expansion operator
- [x] Write test for Perl duplicate _conditions definitions
  - Bug already fixed in Phase 2-3 (consolidated)
- [x] Write test for ST missing AND/OR operators
  - Added test showing AND/OR not counted
- [x] Write tests for TTCN and GDScript 'else' keyword inclusion
  - Added 2 tests - bugs already fixed in Phase 2-3
  - BONUS: Found NEW BUG - GDScript missing 'elif'!
- [x] Test backward compatibility with custom readers
- [x] Run all tests and verify which bugs are reproduced (8 new tests added)

### Phase 6: Fix Identified Bugs ✅ COMPLETED
- [x] Fix GDScript: add missing 'elif' keyword (NEW BUG found in Phase 5)
  - Added 'elif' to _control_flow_keywords
  - Test now passes with correct CCN
- [x] Fix ST: add missing AND/OR operators
  - Added 'and', 'or', 'AND', 'OR' to _logical_operators
  - Test now passes with correct CCN
- [x] R language: document element-wise operator decision
  - Decision: Keep both & and && (all represent conditional logic)
  - Updated documentation with rationale
  - Tests updated to reflect decision
- [x] Erlang: clarify '?' meaning
  - Documented that '?' is macro expansion, not ternary
  - Keep current behavior (macros add to complexity)
  - Test updated to document accepted behavior
- [x] Bugs already fixed in Phase 2-3:
  - Perl duplicate _conditions (consolidated)
  - Rust incorrect 'case' (removed)
  - TTCN 'else' keyword (removed)
  - GDScript 'else' keyword (removed)
- [x] Run all tests to confirm all bugs are fixed (1021 tests passing)

### Phase 7: Documentation
- [ ] Update language implementation guide (add-language.mdc)
- [ ] Add migration guide for custom language readers
- [ ] Document the four condition categories
- [ ] Add examples of proper categorization
- [ ] Update theory.rst if needed

### Phase 8: Cleanup
- [ ] Consider adding deprecation warning for `_conditions`
- [ ] Review and finalize documentation
- [ ] Close related issues/PRs
