# TODO List

## Completed

### Separate Conditions Refactoring ✅ COMPLETE
All phases completed successfully:
- [x] Separated mixed condition concepts into 4 distinct categories
- [x] Updated all 21+ language readers with proper categorization
- [x] Updated all 4 extensions to use semantic names
- [x] Fixed 7 bugs (Perl, Rust, TTCN, GDScript x2, ST, R)
- [x] Added 8 comprehensive bug reproduction tests
- [x] Removed dead code (8 lines)
- [x] Created documentation for language implementers
- [x] All 1021 tests passing (100% pass rate)
- [x] Zero regressions
- [x] Fully backward compatible

## Active Items

### Checkstyle Output Feature
- [ ] Create or update `ongoing/checkstyle_output.md` with requirements and design notes
- [ ] Add `--checkstyle` option to the CLI and ensure it appears in `lizard --help`
- [ ] Implement `lizard_ext/checkstyleoutput.py` to generate Checkstyle XML output
- [ ] Integrate the new output so `--checkstyle` and/or `.checkstyle.xml` triggers it
- [ ] Add/extend tests to verify:
      - CLI accepts `--checkstyle`
      - Output is valid Checkstyle XML
      - Output is accepted by Checkstyle consumers (e.g., Jenkins Warnings NG)
      - Help message includes the new option
- [ ] Update documentation as needed (requirement doc, README)
