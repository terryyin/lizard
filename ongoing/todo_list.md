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

### Phase 1: Research and Planning (COMPLETED)
- [x] Research how _conditions is used across all language readers
- [x] Identify proper domain names for the concepts
- [x] Find languages with potential bugs related to mixing
- [x] Create comprehensive inventory of all language conditions
- [x] Document refactoring plan

### Phase 2: Infrastructure (No behavior change)
- [ ] Add four new class fields to CodeReader base class:
  - `_control_flow_keywords`
  - `_logical_operators`
  - `_case_keywords`
  - `_ternary_operators`
- [ ] Update `CodeReader.__init__()` to:
  - Build combined `conditions` set from new fields
  - Expose individual sets as instance attributes
  - Support backward compatibility with `_conditions`
- [ ] Add helper method `_build_conditions()` to combine sets
- [ ] Run all tests to ensure no regression

### Phase 3: Update Language Readers (Group A - Symbol-based logical operators)
- [ ] Update C# (csharp.py)
- [ ] Update GDScript (gdscript.py)
- [ ] Update Kotlin (kotlin.py)
- [ ] Update Rust (rust.py) - also fix 'case' bug
- [ ] Update Scala (scala.py)
- [ ] Update Solidity (solidity.py)
- [ ] Update Swift (swift.py)
- [ ] Update TSX (tsx.py)
- [ ] Update TypeScript (typescript.py)

### Phase 3b: Update Language Readers (Group B - Word-based logical operators)
- [ ] Update Erlang (erlang.py) - also fix '?' meaning
- [ ] Update Fortran (fortran.py)
- [ ] Update PL/SQL (plsql.py)
- [ ] Update Python (python.py)
- [ ] Update Zig (zig.py)
- [ ] Update TNSDL (tnsdl.py)
- [ ] Update TTCN (ttcn.py) - also review 'else' inclusion
- [ ] Update ST (st.py) - also review missing logical operators

### Phase 3c: Update Language Readers (Group C - Mixed/Special cases)
- [ ] Update Perl (perl.py) - consolidate two _conditions definitions
- [ ] Update PHP (php.py)
- [ ] Update Ruby/Lua (rubylike.py)
- [ ] Update R (r.py) - fix element-wise vs short-circuit operators

### Phase 3d: Review Languages Using Base Class
- [ ] Audit and test Go (go.py)
- [ ] Audit and test Java (java.py)
- [ ] Audit and test JavaScript (javascript.py)
- [ ] Audit and test C++ (clike.py)
- [ ] Audit and test Objective-C (objc.py)
- [ ] Audit and test Vue (vue.py)

### Phase 4: Update Extensions
- [ ] Update lizardnonstrict.py to use `logical_operators`
- [ ] Update lizardmccabe.py to use `case_keywords`
- [ ] Update lizardmodified.py to use `case_keywords`
- [ ] Review lizardcomplextags.py for any needed updates
- [ ] Test all extensions with new structure

### Phase 5: Testing and Bug Reproduction (TDD Approach)
- [ ] Ensure all existing tests pass after refactoring
- [ ] Write test for R language element-wise (&, |) vs short-circuit (&&, ||) operators
  - Test should demonstrate the difference in CCN counting
  - Test should currently fail or show incorrect behavior
- [ ] Write test for Rust 'case' keyword bug
  - Verify Rust doesn't have 'case' in its syntax
  - Test should show 'case' incorrectly in _conditions
- [ ] Write test for Erlang '?' operator meaning
  - Demonstrate '?' is macro operator, not ternary
  - Document expected behavior
- [ ] Write test for Perl duplicate _conditions definitions
  - Verify both definitions are consistent
  - Test should expose any discrepancies
- [ ] Write test for ST missing AND/OR operators
  - Test ST code with AND/OR logical operators
  - Verify CCN counting is correct
- [ ] Write tests for TTCN and GDScript 'else' keyword inclusion
  - Verify whether 'else' should add to CCN
  - Document rationale
- [ ] Add integration tests with real codebases
- [ ] Test backward compatibility with custom readers
- [ ] Run all tests and verify which bugs are reproduced

### Phase 6: Fix Identified Bugs (After Tests Written)
- [ ] Fix R language: separate element-wise (&, |) from short-circuit (&&, ||) operators
  - Decide: should element-wise operators count toward CCN?
  - Update R _conditions accordingly
  - Run tests until they pass
- [ ] Fix Rust: remove incorrect 'case' keyword
  - Remove 'case' from Rust _conditions
  - Run tests until they pass
- [ ] Fix Erlang: clarify '?' meaning
  - Document or fix '?' operator handling
  - Run tests until they pass
- [ ] Fix Perl: consolidate two _conditions definitions
  - Merge into single definition
  - Verify no functionality lost
  - Run tests until they pass
- [ ] Fix ST: add missing AND/OR if needed
  - Add logical operators if tests show they're missing
  - Run tests until they pass
- [ ] Fix TTCN and GDScript: resolve 'else' inclusion
  - Keep or remove based on test results
  - Run tests until they pass
- [ ] Run all tests to confirm all bugs are fixed

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
