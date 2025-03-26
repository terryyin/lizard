# TODO List for Perl Language Support

## High Priority (Basic Function and Control Flow)
- [ ] Test basic function variations
  - [x] Package methods (e.g. `Package::method`) (completed)
  - [x] Methods with attributes (e.g. `sub method :attr`) (completed)
  - [x] Anonymous subroutines (completed)
  - [ ] One-liner functions
- [ ] Test core control structures
  - [ ] `if-elsif-else` chains
  - [ ] `unless` conditions
  - [ ] `while` and `until` loops
  - [ ] `for` and `foreach` loops
  - [ ] Compound conditions with `&&` and `||`
  - [ ] Ternary operator `?:`

## Medium Priority (Advanced Features)
- [ ] Test block-level scoping and nested functions
  - [ ] Nested subroutines
  - [ ] Block-level variable scoping
  - [ ] Package-level scoping
- [ ] Test special control structures
  - [ ] `given-when` constructs
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