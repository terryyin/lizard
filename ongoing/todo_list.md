# TODO List for Perl Language Support

## High Priority (Basic Function and Control Flow)
- [x] Test basic function variations
  - [x] Package methods (e.g. `Package::method`) (completed)
  - [x] Methods with attributes (e.g. `sub method :attr`) (completed)
  - [x] Anonymous subroutines (completed)
  - [x] One-liner functions (completed)
- [x] Test core control structures
  - [x] `if-elsif-else` chains (completed)
  - [x] `unless` conditions (completed)
  - [x] `while` and `until` loops (completed)
  - [x] `for` and `foreach` loops (completed)
  - [x] Compound conditions with `&&` and `||` (completed)
  - [x] Ternary operator `?:` (completed)

## Medium Priority (Advanced Features)
- [x] Test block-level scoping and nested functions
  - [x] Nested subroutines (completed)
  - [x] Block-level variable scoping (completed)
  - [x] Package-level scoping (completed)
- [ ] Test special control structures
  - [x] `given-when` constructs (completed)
  - [ ] `do-while` loops
  - [ ] Regular expression conditions
  - [ ] Method chaining

## Low Priority (Edge Cases)
- [ ] Test complex function signatures
  - [ ] Prototypes
  - [ ] Multiple parameter patterns
  - [ ] Default values
- [ ] Test special variables affecting control flow
- [ ] Test exception handling constructs
  - [ ] `eval` blocks
  - [ ] `try-catch` like patterns
- [ ] Test POD documentation impact on parsing

## Documentation
- [ ] Document all supported Perl syntax features
- [ ] Add examples for each supported construct
- [ ] Update language specification
