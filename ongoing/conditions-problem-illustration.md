# Visual Illustration of the Conditions Mixing Problem

## The Current Problem

### What We Have Now (Mixed Concepts)
```
_conditions = {'if', 'for', 'while', '&&', '||', '?', 'catch', 'case'}
                 ↓      ↓      ↓      ↓     ↓    ↓     ↓       ↓
               [————————————————— all mixed together —————————————————]
```

### What We Should Have (Separated Concepts)

```
_control_flow_keywords = {'if', 'for', 'while', 'catch'}
                           ↓      ↓      ↓        ↓
                     [— Control Flow Structures —]

_logical_operators = {'&&', '||'}
                       ↓     ↓
                  [— Logical Ops —]

_case_keywords = {'case'}
                   ↓
              [— Case Labels —]

_ternary_operators = {'?'}
                      ↓
                 [— Ternary —]
```

## Real-World Examples

### Example 1: Python Code
```python
def complex_function(x, y, z):
    if x > 0 and y > 0 or z < 10:  # CCN: +3 (if, and, or)
        for i in range(10):         # CCN: +1 (for)
            while i < 5:            # CCN: +1 (while)
                try:
                    process()
                except Exception:   # CCN: +1 (except)
                    pass
    return result if result else 0  # CCN: +0 (no '?' in Python)
# Total CCN: 7 (1 base + 6)
```

**Current Python `_conditions`:**
```python
_conditions = {'if', 'for', 'while', 'and', 'or', 'elif', 'except', 'finally'}
#               🔀    🔀     🔀      🔗    🔗    🔀      🔀        🔀
```

**Problem**: All mixed together, extensions can't easily target just `and`/`or`

### Example 2: C++ Code
```cpp
int complex_function(int x, int y) {
    if (x > 0 && y > 0 || x < -10) {  // CCN: +4 (if, &&, ||, ||)
        switch(x) {                    // CCN: +0 (switch itself)
            case 1:                    // CCN: +1 (case)
            case 2:                    // CCN: +1 (case)
            case 3:                    // CCN: +1 (case)
                break;
        }
    }
    return (x > 0) ? y : -y;          // CCN: +1 (ternary ?)
}
// Total CCN: 9 (1 base + 8)
```

**Current Base `_conditions`:**
```python
_conditions = {'if', 'for', 'while', '&&', '||', '?', 'catch', 'case'}
#               🔀    🔀     🔀      🔗    🔗    ❓    🔀        🔢
```

**Problem**: Can't distinguish case counting from other constructs

## How Extensions Are Affected

### Extension: lizardnonstrict.py
**Goal**: Count complexity WITHOUT logical operators (only control flow)

**Current Implementation (Hacky):**
```python
def __call__(self, tokens, reader):
    reader.conditions -= set(['&&', '||', 'and', 'or'])  # Remove by hardcoding
    return tokens
```

**Problems:**
- Hard-coded list of operators
- Must know all languages' logical operators
- Easy to miss new operators

**With Separation (Clean):**
```python
def __call__(self, tokens, reader):
    reader.conditions -= reader.logical_operators  # Semantic!
    return tokens
```

### Extension: lizardmccabe.py
**Goal**: Count only FIRST case in switch (McCabe's definition)

**Current Implementation (Complex State Machine):**
```python
def _after_a_case(self, token):
    if token == "case":
        self.context.add_condition(-1)  # Subtract for consecutive cases
```

**Problems:**
- Needs complex state machine
- Hard to distinguish 'case' from other conditions
- Error-prone

**With Separation (Simpler):**
```python
def __call__(self, tokens, reader):
    # Could potentially simplify by targeting case_keywords specifically
    # (implementation still needs state machine, but intent is clearer)
```

## Language Variations Illustrated

### Symbol-based Logical Operators
```
C++/Java/C#:     if (a && b || c)
JavaScript:      if (a && b || c)
Kotlin:          if (a && b || c)
PHP:             if ($a && $b || $c)
```

### Word-based Logical Operators
```
Python:          if a and b or c:
Ruby:            if a and b or c
Perl:            if $a and $b or $c      # Also has && ||
Fortran:         IF (a .AND. b .OR. c)
PL/SQL:          IF a AND b OR c THEN
```

### Mixed Operators (Dual Purpose)
```
Perl:            
  if $x && $y     # Symbol form
  if $x and $y    # Word form (lower precedence!)

R:
  if (a && b)     # Short-circuit (scalar)
  result <- a & b # Element-wise (vectorized) ⚠️
```

## The R Language Bug in Detail

### Current R `_conditions`:
```python
_conditions = {
    'if', 'for', 'while', 'switch',          # Control flow ✓
    '&&', '||',                              # Short-circuit logical ✓
    '&', '|',                                # Element-wise logical ⚠️
    'ifelse', 'tryCatch', 'try'              # Functions ⚠️
}
```

### The Problem:
```r
# This SHOULD add to CCN (control flow decision):
if (x > 0 && y > 0) { ... }  # CCN: +2 (if, &&)

# This probably SHOULD NOT (vectorized operation):
flags <- (x > 0) & (y > 0)   # CCN: +1? (just &)
# This is element-wise operation on vectors, not a control flow decision!

# This is debatable (function call):
result <- ifelse(x > 0, 1, -1)  # CCN: +1? (ifelse)
# Is a function call a control flow decision?
```

### Why It's Confusing:
- `&&` and `&` look similar but have different semantics
- `&&` is short-circuit (control flow)
- `&` is vectorized (data operation)
- Currently both add +1 to CCN

## Before and After Comparison

### Before (Current State)
```python
# Base class
class CodeReader:
    _conditions = {'if', 'for', 'while', '&&', '||', '?', 'catch', 'case'}
    
    def __init__(self, context):
        self.conditions = copy(self._conditions)  # One big mixed set

# Extension trying to remove logical operators
class LizardNonStrict:
    def __call__(self, tokens, reader):
        reader.conditions -= set(['&&', '||', 'and', 'or'])  # Hardcoded!
```

**Issues:**
- ❌ All concepts mixed
- ❌ Extensions use hardcoded lists
- ❌ Unclear what each token represents
- ❌ Hard to maintain

### After (Proposed State)
```python
# Base class
class CodeReader:
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}
    
    def __init__(self, context):
        # Combine for backward compatibility
        self.conditions = (self._control_flow_keywords | 
                          self._logical_operators |
                          self._case_keywords |
                          self._ternary_operators)
        # Also expose separately
        self.control_flow_keywords = copy(self._control_flow_keywords)
        self.logical_operators = copy(self._logical_operators)
        self.case_keywords = copy(self._case_keywords)
        self.ternary_operators = copy(self._ternary_operators)

# Extension using semantic names
class LizardNonStrict:
    def __call__(self, tokens, reader):
        reader.conditions -= reader.logical_operators  # Semantic!
```

**Benefits:**
- ✅ Clear separation of concepts
- ✅ Extensions use semantic names
- ✅ Self-documenting code
- ✅ Easy to maintain
- ✅ Backward compatible

## Impact Summary

### Files to Update
```
Phase 1: Infrastructure
  ✏️  lizard_languages/code_reader.py (base class)

Phase 2-3: Language Readers (23 files)
  ✏️  lizard_languages/python.py
  ✏️  lizard_languages/javascript.py
  ✏️  lizard_languages/java.py
  ... (20 more)

Phase 4: Extensions (4 files)
  ✏️  lizard_ext/lizardnonstrict.py
  ✏️  lizard_ext/lizardmccabe.py
  ✏️  lizard_ext/lizardmodified.py
  ✏️  lizard_ext/lizardcomplextags.py (review)

Phase 5: Bug Fixes
  🐛 Fix R language element-wise operators
  🐛 Fix Rust incorrect 'case' keyword
  🐛 Fix Erlang '?' meaning
  🐛 Fix Perl duplicate definitions
```

### Testing Impact
```
✅ All existing tests must pass (backward compatible)
✅ Add new tests for bug fixes
✅ Extension tests must pass
✅ Integration tests with real code
```

### Documentation Impact
```
📚 Update language implementation guide
📚 Add migration guide for custom readers
📚 Update theory documentation if needed
```

## Conclusion

This refactoring:
- **Fixes conceptual confusion** by separating mixed concepts
- **Enables better extensions** by providing semantic categorization
- **Fixes real bugs** in R, Rust, Erlang, and Perl
- **Maintains compatibility** with existing code
- **Improves maintainability** for future development

The implementation is straightforward and can be done incrementally with full test coverage.

